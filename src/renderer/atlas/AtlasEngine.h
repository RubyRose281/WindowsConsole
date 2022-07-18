// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#pragma once

#include <d2d1.h>
#include <d3d11_1.h>
#include <dwrite_3.h>
#include <memory_resource>

#include "../../renderer/inc/IRenderEngine.hpp"
#include "AtlasEntry.h"

namespace Microsoft::Console::Render::Atlas
{
    struct TextAnalyzerResult
    {
        u32 textPosition = 0;
        u32 textLength = 0;

        // These 2 fields represent DWRITE_SCRIPT_ANALYSIS.
        // Not using DWRITE_SCRIPT_ANALYSIS drops the struct size from 20 down to 12 bytes.
        u16 script = 0;
        u8 shapes = 0;

        u8 bidiLevel = 0;
    };

    struct FontMetrics
    {
        wil::com_ptr<IDWriteFontCollection> fontCollection;
        wil::unique_process_heap_string fontName;
        float baselineInDIP = 0.0f;
        float fontSizeInDIP = 0.0f;
        u16x2 cellSize;
        u16 fontWeight = 0;
        u16 underlinePos = 0;
        u16 strikethroughPos = 0;
        u16 lineThickness = 0;
    };

    struct TileHashMap
    {
        TileHashMap() = default;

        AtlasEntry* findOrInsert(AtlasEntryKeyAttributes attributes, u16 charCount, u16 coordCount, const wchar_t* chars, bool& inserted)
        {
            const auto keySize = AtlasEntry::calculateKeySize(charCount);
            const auto key = static_cast<AtlasEntryKey*>(_allocator.allocate(keySize));
            u32 hash;

            key->attributes = attributes;
            key->charCount = charCount;
            key->coordCount = coordCount;
#pragma warning(suppress : 26490) // Don't use reinterpret_cast (type.1).
            {
                // totalSize is rounded up to the next multiple of 4, but
                // charCount might only amount to a multiple of 2 (bytes).
                // memset()ing the last wchar_t ensures we don't hash uninitialized data.
                const auto data = reinterpret_cast<u8*>(key);
                memset(data + keySize - 2, 0, 2);

                // This will potentially overwrite the memset()'d wchar_t above.
                std::copy_n(chars, charCount, &key->chars[0]);

                hash = hashData(data, keySize);
            }

            {
                const auto it = _map[hash & _mapMask];
                for (auto entry = it; entry; entry = entry->next)
                {
                    const auto itKeySize = til::bit_cast<uintptr_t>(entry->value) - til::bit_cast<uintptr_t>(&entry->key);
                    if (itKeySize == keySize && memcmp(key, &entry->key, keySize) == 0)
                    {
                        _allocator.deallocate(key);

                        lruRemove(entry);
                        lruPush(entry);

                        return entry;
                    }
                }
            }

            if (_size == _mapSize)
            {
                const auto newMapSize = _mapSize << 1;
                const auto newMapMask = newMapSize - 1;
                FAIL_FAST_IF(newMapSize <= _mapSize); // overflow
                auto newMap = std::make_unique<AtlasEntry*[]>(newMapSize);

                auto it = _map.get();
                const auto end = it + _mapSize;
                for (; it != end; ++it)
                {
                    for (auto entry = *it; entry;)
                    {
                        const auto next = entry->next;
                        mapPush(&newMap[entry->hash & newMapMask], entry);
                        entry = next;
                    }
                }

                _map = std::move(newMap);
                _mapSize = newMapSize;
                _mapMask = newMapMask;
            }

            const auto valueOffset = offsetof(AtlasEntry, key) + keySize;
            const auto totalSize = valueOffset + AtlasEntry::calculateValueSize(coordCount);
            const auto entry = static_cast<AtlasEntry*>(_allocator.allocate(totalSize));
            const auto slot = &_map[hash & _mapMask];

            entry->value = til::bit_cast<AtlasEntryValue*>(til::bit_cast<uintptr_t>(entry) + valueOffset);
            entry->hash = hash;
            memcpy(&entry->key, key, keySize);

            mapPush(slot, entry);
            lruPush(entry);

            _size++;

            inserted = true;
            return entry;
        }

        void makeNewest(AtlasEntry* entry) noexcept
        {
            lruRemove(entry);
            lruPush(entry);
        }

        void popOldestTiles(std::vector<u16x2>& out) noexcept
        {
            FAIL_FAST_IF(!_size || !_oldest);

            const auto entry = _oldest;

            if (!entry->newer)
            {
                __debugbreak();
            }

            const auto slot = &_map[entry->hash & _mapMask];
            const auto offset = out.size();
            const auto cellCount = entry->key.coordCount;
            out.resize(offset + cellCount);
            std::copy_n(&entry->value->coords[0], cellCount, out.begin() + offset);

            mapRemove(slot, entry);
            lruRemove(entry);

            auto it = std::find(_map.get(), _map.get() + _mapSize, entry);
            if (it != (_map.get() + _mapSize))
            {
                __debugbreak();
            }

            _size--;

            _allocator.deallocate(entry);
        }

        void reset() noexcept
        {
            _allocator.release();
            memset(_map.get(), 0, _mapSize * sizeof(_map[0]));
            _newest = nullptr;
            _oldest = nullptr;
            _size = 0;
        }

        static u32 hashData(const u8* beg, size_t length) noexcept
        {
            // This hash function only works with data fully aligned to u32 (including the length).
            assert(til::bit_cast<uintptr_t>(beg) % sizeof(u32) == 0);
            assert(length % sizeof(u32) == 0);

            const auto end = beg + length;

            // This loop is a simple LCG (linear congruential generator) with Donald Knuth's
            // widely used parameters. Unlike with normal LCGs however we mix in
            // 4 bytes of the input on each iteration using a simple XOR.
            auto h = UINT64_C(0x243F6A8885A308D3); // fractional digits of pi in hex (OEIS: A062964)
            for (; beg != end; beg += sizeof(u32))
            {
                // Neither x64 nor ARM64 assembly differentiates between aligned and unaligned loads.
                // As such we can freely use the standard compliant way of reading u8*: memcpy().
                // (In Release mode this should be inlined to a single instruction.)
                u32 v;
                memcpy(&v, beg, sizeof(u32));
                h = (h ^ v) * UINT64_C(6364136223846793005) + UINT64_C(1442695040888963407);
            }

            // PCG (permuted congruential generator) XSL-RR finalizer.
            // In testing it seemed sufficient for the purpose of a hash-map key generator.
            //
            // Copyright 2014-2017 Melissa O'Neill <oneill@pcg-random.org>, and the PCG Project contributors.
            // See oss/pcg/LICENSE-MIT.txt, oss/pcg/LICENSE-APACHE.txt or https://www.pcg-random.org/.
            const int r = h & 63;
            const auto x = gsl::narrow_cast<u32>(h >> 32) ^ gsl::narrow_cast<u32>(h);
            return _rotl(x, r);
        }

        void lruRemove(const AtlasEntry* entry) noexcept
        {
            if (entry->newer)
            {
                entry->newer->older = entry->older;
            }
            else
            {
                _newest = entry->older;
            }
            if (entry->older)
            {
                entry->older->newer = entry->newer;
            }
            else
            {
                _oldest = entry->newer;
            }
        }

        void lruPush(AtlasEntry* entry) noexcept
        {
            if (_newest)
            {
                _newest->newer = entry;
            }
            else
            {
                _oldest = entry;
            }
            entry->newer = nullptr;
            entry->older = _newest;
            _newest = entry;
        }

        static void mapRemove(AtlasEntry** slot, const AtlasEntry* entry) noexcept
        {
            if (entry->prev)
            {
                entry->prev->next = entry->next;
            }
            else
            {
                *slot = entry->next;
            }
            if (entry->next)
            {
                entry->next->prev = entry->prev;
            }
        }

        static void mapPush(AtlasEntry** slot, AtlasEntry* entry) noexcept
        {
            if (*slot)
            {
                (*slot)->prev = entry;
            }
            entry->next = *slot;
            entry->prev = nullptr;
            *slot = entry;
        }

        static constexpr u32 initialSize = 4;

        heapapi_pool _allocator{ 256 * AtlasEntry::calculateEntrySize(2, 2) };
        std::unique_ptr<AtlasEntry*[]> _map = std::make_unique<AtlasEntry*[]>(initialSize);
        AtlasEntry* _newest = nullptr;
        AtlasEntry* _oldest = nullptr;
        u32 _mapSize = initialSize;
        u32 _mapMask = initialSize - 1;
        u32 _size = 0;
    };

    // TileAllocator yields `tileSize`-sized tiles for our texture atlas.
    // While doing so it'll grow the atlas size() by a factor of 2 if needed.
    // Once the setMaxArea() is exceeded it'll stop growing and instead
    // snatch tiles back from the oldest TileHashMap entries.
    //
    // The quadratic growth works by alternating the size()
    // between an 1:1 and 2:1 aspect ratio, like so:
    //   (64,64) -> (128,64) -> (128,128) -> (256,128) -> (256,256)
    // These initial tile positions allocate() returns are in a Z
    // pattern over the available space in the atlas texture.
    // You can log the `return _pos;` in allocate() using "Tracepoint"s
    // in Visual Studio if you'd like to understand the Z pattern better.
    struct TileAllocator
    {
        TileAllocator() = default;

        explicit TileAllocator(u16x2 tileSize, u16x2 windowSize) noexcept :
            _tileSize{ tileSize }
        {
            const auto initialSize = std::max(u16{ _absoluteMinSize }, std::bit_ceil(std::max(tileSize.x, tileSize.y)));
            _size = { initialSize, initialSize };
            _limit = { gsl::narrow_cast<u16>(initialSize - _tileSize.x), gsl::narrow_cast<u16>(initialSize - _tileSize.y) };
            setMaxArea(windowSize);
        }

        u16x2 size() const noexcept
        {
            return _size;
        }

        void setMaxArea(u16x2 windowSize) noexcept
        {
            // _generate() uses a quadratic growth factor for _size's area.
            // Once it exceeds the _maxArea, it'll start snatching tiles back from the
            // TileHashMap using its LRU queue. Since _size will at least reach half
            // of _maxSize (because otherwise it could still grow by a factor of 2)
            // and by ensuring that _maxArea is at least twice the window size
            // we make it impossible* for _generate() to return false before
            // TileHashMap contains at least as many tiles as the window contains.
            // If that wasn't the case we'd snatch and reuse tiles that are still in use.
            // * lhecker's legal department:
            //   No responsibility is taken for the correctness of this information.
            setMaxArea(static_cast<size_t>(windowSize.x) * static_cast<size_t>(windowSize.y) * 2);
        }

        void setMaxArea(size_t max) noexcept
        {
            // We need to reserve at least 1 extra `tileArea`, because the tile
            // at position {0,0} is already reserved for the cursor texture.
            const auto tileArea = static_cast<size_t>(_tileSize.x) * static_cast<size_t>(_tileSize.y);
            _maxArea = clamp(max + tileArea, _absoluteMinArea, _absoluteMaxArea);
            _updateCanGenerate();
        }

        u16x2 allocate(TileHashMap& map) noexcept
        {
            if (_generate())
            {
                return _pos;
            }

            if (_cache.empty())
            {
                map.popOldestTiles(_cache);
            }

            const auto pos = _cache.back();
            _cache.pop_back();
            return pos;
        }

        void reset() noexcept
        {
            _cache.clear();
            _pos = {};
            _originX = 0;
            _canGenerate = true;
        }

    private:
        // This method generates the Z pattern coordinates
        // described above in the TileAllocator comment.
        bool _generate() noexcept
        {
            if (!_canGenerate)
            {
                return false;
            }

            // We need to backup _pos/_size in case our resize below exceeds _maxArea.
            // In that case we have to restore _pos/_size so that if _maxArea is increased
            // (window resize for instance), we can pick up where we previously left off.
            const auto pos = _pos;

            _pos.x += _tileSize.x;
            if (_pos.x <= _limit.x)
            {
                return true;
            }

            _pos.y += _tileSize.y;
            if (_pos.y <= _limit.y)
            {
                _pos.x = _originX;
                return true;
            }

            // Same as for pos.
            const auto size = _size;

            // This implements a quadratic growth factor for _size, by
            // alternating between an 1:1 and 2:1 aspect ratio, like so:
            //   (64,64) -> (128,64) -> (128,128) -> (256,128) -> (256,256)
            // This behavior is strictly dependent on setMaxArea(u16x2)'s
            // behavior. See it's comment for an explanation.
            if (_size.x == _size.y)
            {
                _size.x *= 2;
                _pos.y = 0;
            }
            else
            {
                _size.y *= 2;
                _pos.x = 0;
            }

            _updateCanGenerate();
            if (_canGenerate)
            {
                _limit = { gsl::narrow_cast<u16>(_size.x - _tileSize.x), gsl::narrow_cast<u16>(_size.y - _tileSize.y) };
                _originX = _pos.x;
            }
            else
            {
                _size = size;
                _pos = pos;
            }

            return _canGenerate;
        }

        void _updateCanGenerate() noexcept
        {
            _canGenerate = static_cast<size_t>(_size.x) * static_cast<size_t>(_size.y) <= _maxArea;
        }

        static constexpr u16 _absoluteMinSize = 256;
        static constexpr size_t _absoluteMinArea = _absoluteMinSize * _absoluteMinSize;
        // TODO: Consider using IDXGIAdapter3::QueryVideoMemoryInfo() and IDXGIAdapter3::RegisterVideoMemoryBudgetChangeNotificationEvent()
        // That way we can make better to use of a user's available video memory.
        static constexpr size_t _absoluteMaxArea = D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION * D3D10_REQ_TEXTURE2D_U_OR_V_DIMENSION;

        std::vector<u16x2> _cache;
        size_t _maxArea = _absoluteMaxArea;
        u16x2 _tileSize;
        u16x2 _size;
        u16x2 _limit;
        // Since _pos starts at {0, 0}, it'll result in the first allocate()d tile to be at {_tileSize.x, 0}.
        // Coincidentially that's exactly what we want as the cursor texture lives at {0, 0}.
        u16x2 _pos;
        u16 _originX = 0;
        // Indicates whether we've exhausted our Z pattern across the atlas texture.
        // If this is false, we have to snatch tiles back from TileHashMap.
        bool _canGenerate = true;
    };

    struct CachedCursorOptions
    {
        u32 cursorColor = INVALID_COLOR;
        u16 cursorType = gsl::narrow_cast<u16>(CursorType::Legacy);
        u8 heightPercentage = 20;

        ATLAS_POD_OPS(CachedCursorOptions)
    };

    struct BufferLineMetadata
    {
        u32x2 colors;
        CellFlags flags = CellFlags::None;
    };

    // NOTE: D3D constant buffers sizes must be a multiple of 16 bytes.
    struct alignas(16) ConstBuffer
    {
        // WARNING: Modify this carefully after understanding how HLSL struct packing works.
        // The gist is:
        // * Minimum alignment is 4 bytes (like `#pragma pack 4`)
        // * Members cannot straddle 16 byte boundaries
        //   This means a structure like {u32; u32; u32; u32x2} would require
        //   padding so that it is {u32; u32; u32; <4 byte padding>; u32x2}.
        // * bool will probably not work the way you want it to,
        //   because HLSL uses 32-bit bools and C++ doesn't.
        alignas(sizeof(f32x4)) f32x4 viewport;
        alignas(sizeof(f32x4)) f32 gammaRatios[4]{};
        alignas(sizeof(f32)) f32 enhancedContrast = 0;
        alignas(sizeof(u32)) u32 cellCountX = 0;
        alignas(sizeof(u32x2)) u32x2 cellSize;
        alignas(sizeof(u32x2)) u32x2 underlinePos;
        alignas(sizeof(u32x2)) u32x2 strikethroughPos;
        alignas(sizeof(u32)) u32 backgroundColor = 0;
        alignas(sizeof(u32)) u32 cursorColor = 0;
        alignas(sizeof(u32)) u32 selectionColor = 0;
        alignas(sizeof(u32)) u32 useClearType = 0;
#pragma warning(suppress : 4324) // 'ConstBuffer': structure was padded due to alignment specifier
    };

    // Handled in BeginPaint()
    enum class ApiInvalidations : u8
    {
        None = 0,
        Title = 1 << 0,
        Device = 1 << 1,
        SwapChain = 1 << 2,
        Size = 1 << 3,
        Font = 1 << 4,
        Settings = 1 << 5,
    };
    ATLAS_FLAG_OPS(ApiInvalidations, u8)

    // Handled in Present()
    enum class RenderInvalidations : u8
    {
        None = 0,
        Cursor = 1 << 0,
        ConstBuffer = 1 << 1,
    };
    ATLAS_FLAG_OPS(RenderInvalidations, u8)

    class AtlasEngine final : public IRenderEngine
    {
    public:
        explicit AtlasEngine();

        AtlasEngine(const AtlasEngine&) = delete;
        AtlasEngine& operator=(const AtlasEngine&) = delete;

        // IRenderEngine
        [[nodiscard]] HRESULT StartPaint() noexcept override;
        [[nodiscard]] HRESULT EndPaint() noexcept override;
        [[nodiscard]] bool RequiresContinuousRedraw() noexcept override;
        void WaitUntilCanRender() noexcept override;
        [[nodiscard]] HRESULT Present() noexcept override;
        [[nodiscard]] HRESULT PrepareForTeardown(_Out_ bool* pForcePaint) noexcept override;
        [[nodiscard]] HRESULT ScrollFrame() noexcept override;
        [[nodiscard]] HRESULT Invalidate(const til::rect* psrRegion) noexcept override;
        [[nodiscard]] HRESULT InvalidateCursor(const til::rect* psrRegion) noexcept override;
        [[nodiscard]] HRESULT InvalidateSystem(const til::rect* prcDirtyClient) noexcept override;
        [[nodiscard]] HRESULT InvalidateSelection(const std::vector<til::rect>& rectangles) noexcept override;
        [[nodiscard]] HRESULT InvalidateScroll(const til::point* pcoordDelta) noexcept override;
        [[nodiscard]] HRESULT InvalidateAll() noexcept override;
        [[nodiscard]] HRESULT InvalidateFlush(_In_ const bool circled, _Out_ bool* const pForcePaint) noexcept override;
        [[nodiscard]] HRESULT InvalidateTitle(std::wstring_view proposedTitle) noexcept override;
        [[nodiscard]] HRESULT NotifyNewText(const std::wstring_view newText) noexcept override;
        [[nodiscard]] HRESULT PrepareRenderInfo(const RenderFrameInfo& info) noexcept override;
        [[nodiscard]] HRESULT ResetLineTransform() noexcept override;
        [[nodiscard]] HRESULT PrepareLineTransform(LineRendition lineRendition, size_t targetRow, size_t viewportLeft) noexcept override;
        [[nodiscard]] HRESULT PaintBackground() noexcept override;
        [[nodiscard]] HRESULT PaintBufferLine(gsl::span<const Cluster> clusters, til::point coord, bool fTrimLeft, bool lineWrapped) noexcept override;
        [[nodiscard]] HRESULT PaintBufferGridLines(GridLineSet lines, COLORREF color, size_t cchLine, til::point coordTarget) noexcept override;
        [[nodiscard]] HRESULT PaintSelection(const til::rect& rect) noexcept override;
        [[nodiscard]] HRESULT PaintCursor(const CursorOptions& options) noexcept override;
        [[nodiscard]] HRESULT UpdateDrawingBrushes(const TextAttribute& textAttributes, const RenderSettings& renderSettings, gsl::not_null<IRenderData*> pData, bool usingSoftFont, bool isSettingDefaultBrushes) noexcept override;
        [[nodiscard]] HRESULT UpdateFont(const FontInfoDesired& FontInfoDesired, _Out_ FontInfo& FontInfo) noexcept override;
        [[nodiscard]] HRESULT UpdateSoftFont(gsl::span<const uint16_t> bitPattern, til::size cellSize, size_t centeringHint) noexcept override;
        [[nodiscard]] HRESULT UpdateDpi(int iDpi) noexcept override;
        [[nodiscard]] HRESULT UpdateViewport(const til::inclusive_rect& srNewViewport) noexcept override;
        [[nodiscard]] HRESULT GetProposedFont(const FontInfoDesired& FontInfoDesired, _Out_ FontInfo& FontInfo, int iDpi) noexcept override;
        [[nodiscard]] HRESULT GetDirtyArea(gsl::span<const til::rect>& area) noexcept override;
        [[nodiscard]] HRESULT GetFontSize(_Out_ til::size* pFontSize) noexcept override;
        [[nodiscard]] HRESULT IsGlyphWideByFont(std::wstring_view glyph, _Out_ bool* pResult) noexcept override;
        [[nodiscard]] HRESULT UpdateTitle(std::wstring_view newTitle) noexcept override;

        // DxRenderer - getter
        HRESULT Enable() noexcept override;
        [[nodiscard]] bool GetRetroTerminalEffect() const noexcept override;
        [[nodiscard]] float GetScaling() const noexcept override;
        [[nodiscard]] HANDLE GetSwapChainHandle() override;
        [[nodiscard]] Types::Viewport GetViewportInCharacters(const Types::Viewport& viewInPixels) const noexcept override;
        [[nodiscard]] Types::Viewport GetViewportInPixels(const Types::Viewport& viewInCharacters) const noexcept override;
        // DxRenderer - setter
        void SetAntialiasingMode(D2D1_TEXT_ANTIALIAS_MODE antialiasingMode) noexcept override;
        void SetCallback(std::function<void()> pfn) noexcept override;
        void EnableTransparentBackground(const bool isTransparent) noexcept override;
        void SetForceFullRepaintRendering(bool enable) noexcept override;
        [[nodiscard]] HRESULT SetHwnd(HWND hwnd) noexcept override;
        void SetPixelShaderPath(std::wstring_view value) noexcept override;
        void SetRetroTerminalEffect(bool enable) noexcept override;
        void SetSelectionBackground(COLORREF color, float alpha = 0.5f) noexcept override;
        void SetSoftwareRendering(bool enable) noexcept override;
        void SetWarningCallback(std::function<void(HRESULT)> pfn) noexcept override;
        [[nodiscard]] HRESULT SetWindowSize(til::size pixels) noexcept override;
        void ToggleShaderEffects() noexcept override;
        [[nodiscard]] HRESULT UpdateFont(const FontInfoDesired& pfiFontInfoDesired, FontInfo& fiFontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes) noexcept override;
        void UpdateHyperlinkHoveredId(uint16_t hoveredId) noexcept override;

    private:
        // AtlasEngine.cpp
        [[nodiscard]] HRESULT _handleException(const wil::ResultException& exception) noexcept;
        __declspec(noinline) void _createResources();
        void _releaseSwapChain();
        __declspec(noinline) void _createSwapChain();
        __declspec(noinline) void _recreateSizeDependentResources();
        __declspec(noinline) void _recreateFontDependentResources();
        IDWriteTextFormat* _getTextFormat(AtlasEntryKeyAttributes attributes) const noexcept;
        const Buffer<DWRITE_FONT_AXIS_VALUE>& _getTextFormatAxis(AtlasEntryKeyAttributes attributes) const noexcept;
        Cell* _getCell(u16 x, u16 y) noexcept;
        AtlasEntry** _getCellGlyphMapping(u16 x, u16 y) noexcept;
        void _setCellFlags(u16r coords, CellFlags mask, CellFlags bits) noexcept;
        void _flushBufferLine();
        void _emplaceGlyph(IDWriteFontFace* fontFace, size_t bufferPos1, size_t bufferPos2);

        // AtlasEngine.api.cpp
        void _resolveAntialiasingMode() noexcept;
        void _updateFont(const wchar_t* faceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes);
        void _resolveFontMetrics(const wchar_t* faceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, FontMetrics* fontMetrics = nullptr) const;

        // AtlasEngine.r.cpp
        void _setShaderResources() const;
        void _updateConstantBuffer() const noexcept;
        void _adjustAtlasSize();
        void _processGlyphQueue();
        void _drawGlyph(const AtlasEntry& item) const;
        void _drawCursor();

        static constexpr bool debugGlyphGenerationPerformance = false;
        static constexpr bool debugGeneralPerformance = false || debugGlyphGenerationPerformance;

        static constexpr u16 u16min = 0x0000;
        static constexpr u16 u16max = 0xffff;
        static constexpr i16 i16min = -0x8000;
        static constexpr i16 i16max = 0x7fff;
        static constexpr u16r invalidatedAreaNone = { u16max, u16max, u16min, u16min };
        static constexpr u16x2 invalidatedRowsNone{ u16max, u16min };
        static constexpr u16x2 invalidatedRowsAll{ u16min, u16max };

        struct StaticResources
        {
            wil::com_ptr<ID2D1Factory> d2dFactory;
            wil::com_ptr<IDWriteFactory1> dwriteFactory;
            wil::com_ptr<IDWriteFontFallback> systemFontFallback;
            wil::com_ptr<IDWriteTextAnalyzer1> textAnalyzer;
            bool isWindows10OrGreater = true;

#ifndef NDEBUG
            std::filesystem::path sourceDirectory;
            wil::unique_folder_change_reader_nothrow sourceCodeWatcher;
            std::atomic<int64_t> sourceCodeInvalidationTime{ INT64_MAX };
#endif
        } _sr;

        struct Resources
        {
            // D3D resources
            wil::com_ptr<ID3D11Device> device;
            wil::com_ptr<ID3D11DeviceContext1> deviceContext;
            wil::com_ptr<IDXGISwapChain1> swapChain;
            wil::unique_handle frameLatencyWaitableObject;
            wil::com_ptr<ID3D11RenderTargetView> renderTargetView;
            wil::com_ptr<ID3D11VertexShader> vertexShader;
            wil::com_ptr<ID3D11PixelShader> pixelShader;
            wil::com_ptr<ID3D11Buffer> constantBuffer;
            wil::com_ptr<ID3D11Buffer> cellBuffer;
            wil::com_ptr<ID3D11ShaderResourceView> cellView;

            // D2D resources
            wil::com_ptr<ID3D11Texture2D> atlasBuffer;
            wil::com_ptr<ID3D11ShaderResourceView> atlasView;
            wil::com_ptr<ID2D1RenderTarget> d2dRenderTarget;
            wil::com_ptr<ID2D1Brush> brush;
            wil::com_ptr<IDWriteTextFormat> textFormats[4];
            Buffer<DWRITE_FONT_AXIS_VALUE> textFormatAxes[4];
            wil::com_ptr<IDWriteTypography> typography;

            Buffer<Cell, 32> cells; // invalidated by ApiInvalidations::Size
            Buffer<AtlasEntry*> cellGlyphMapping; // invalidated by ApiInvalidations::Size
            f32x2 cellSizeDIP; // invalidated by ApiInvalidations::Font, caches _api.cellSize but in DIP
            u16x2 cellSize; // invalidated by ApiInvalidations::Font, caches _api.cellSize
            u16x2 cellCount; // invalidated by ApiInvalidations::Font|Size, caches _api.cellCount
            u16 underlinePos = 0;
            u16 strikethroughPos = 0;
            u16 lineThickness = 0;
            u16 dpi = USER_DEFAULT_SCREEN_DPI; // invalidated by ApiInvalidations::Font, caches _api.dpi
            u16x2 atlasSizeInPixel; // invalidated by ApiInvalidations::Font
            TileHashMap glyphs;
            TileAllocator tileAllocator;
            std::vector<const AtlasEntry*> glyphQueue;

            f32 gamma = 0;
            f32 cleartypeEnhancedContrast = 0;
            f32 grayscaleEnhancedContrast = 0;
            u32 backgroundColor = 0xff000000;
            u32 selectionColor = 0x7fffffff;

            CachedCursorOptions cursorOptions;
            RenderInvalidations invalidations = RenderInvalidations::None;

#ifndef NDEBUG
            // See documentation for IDXGISwapChain2::GetFrameLatencyWaitableObject method:
            // > For every frame it renders, the app should wait on this handle before starting any rendering operations.
            // > Note that this requirement includes the first frame the app renders with the swap chain.
            bool frameLatencyWaitableObjectUsed = false;
#endif
        } _r;

        struct ApiState
        {
            // This structure is loosely sorted in chunks from "very often accessed together"
            // to seldom accessed and/or usually not together.

            std::vector<wchar_t> bufferLine;
            std::vector<u16> bufferLineColumn;
            Buffer<BufferLineMetadata> bufferLineMetadata;
            std::vector<TextAnalyzerResult> analysisResults;
            Buffer<u16> clusterMap;
            Buffer<DWRITE_SHAPING_TEXT_PROPERTIES> textProps;
            Buffer<u16> glyphIndices;
            Buffer<DWRITE_SHAPING_GLYPH_PROPERTIES> glyphProps;
            Buffer<f32> glyphAdvances;
            Buffer<DWRITE_GLYPH_OFFSET> glyphOffsets;
            std::vector<DWRITE_FONT_FEATURE> fontFeatures; // changes are flagged as ApiInvalidations::Font|Size
            std::vector<DWRITE_FONT_AXIS_VALUE> fontAxisValues; // changes are flagged as ApiInvalidations::Font|Size
            FontMetrics fontMetrics; // changes are flagged as ApiInvalidations::Font|Size

            u16x2 cellCount; // caches `sizeInPixel / cellSize`
            u16x2 sizeInPixel; // changes are flagged as ApiInvalidations::Size

            // UpdateDrawingBrushes()
            u32 backgroundOpaqueMixin = 0xff000000; // changes are flagged as ApiInvalidations::Device
            u32x2 currentColor;
            AtlasEntryKeyAttributes attributes = AtlasEntryKeyAttributes::None;
            u16x2 lastPaintBufferLineCoord;
            CellFlags flags = CellFlags::None;
            // SetSelectionBackground()
            u32 selectionColor = 0x7fffffff;
            // UpdateHyperlinkHoveredId()
            u16 hyperlinkHoveredId = 0;
            bool bufferLineWasHyperlinked = false;

            // dirtyRect is a computed value based on invalidatedRows.
            til::rect dirtyRect;
            // These "invalidation" fields are reset in EndPaint()
            u16r invalidatedCursorArea = invalidatedAreaNone;
            u16x2 invalidatedRows = invalidatedRowsNone; // x is treated as "top" and y as "bottom"
            i16 scrollOffset = 0;

            std::function<void(HRESULT)> warningCallback;
            std::function<void()> swapChainChangedCallback;
            wil::unique_handle swapChainHandle;
            HWND hwnd = nullptr;
            u16 dpi = USER_DEFAULT_SCREEN_DPI; // changes are flagged as ApiInvalidations::Font|Size
            u8 antialiasingMode = D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE; // changes are flagged as ApiInvalidations::Font
            u8 realizedAntialiasingMode = D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE; // caches antialiasingMode, depends on antialiasingMode and backgroundOpaqueMixin, see _resolveAntialiasingMode

            ApiInvalidations invalidations = ApiInvalidations::Device;
        } _api;
    };
}

namespace Microsoft::Console::Render
{
    using AtlasEngine = Atlas::AtlasEngine;
}
