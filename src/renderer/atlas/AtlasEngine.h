// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#pragma once

#include <d2d1_3.h>
#include <d3d11_2.h>
#include <dwrite_3.h>

#include <til/generational.h>

#include "DWriteTextAnalysis.h"
#include "helpers.h"
#include "../../renderer/inc/IRenderEngine.hpp"

namespace Microsoft::Console::Render::Atlas
{
    inline constexpr bool debugForceD2DMode = false;
    inline constexpr bool debugGlyphGenerationPerformance = false;
    inline constexpr bool debugTextParsingPerformance = false || debugGlyphGenerationPerformance;
    inline constexpr bool debugGeneralPerformance = false || debugTextParsingPerformance;

    // This structure works similar to how std::string works:
    // You can think of a std::string as a structure consisting of:
    //   char*  data;
    //   size_t size;
    //   size_t capacity;
    // where data is some backing memory allocated on the heap.
    //
    // But std::string employs an optimization called "small string optimization" (SSO).
    // To simplify things it could be explained as:
    // If the string capacity is small, then the characters are stored inside the "data"
    // pointer and you make sure to set the lowest bit in the pointer one way or another.
    // Heap allocations are always aligned by at least 4-8 bytes on any platform.
    // If the address of the "data" pointer is not even you know data is stored inline.
    template<typename T>
    union SmallObjectOptimizer
    {
        static_assert(std::is_trivially_copyable_v<T>);
        static_assert(std::has_unique_object_representations_v<T>);

        T* allocated = nullptr;
        T inlined;

        constexpr SmallObjectOptimizer() = default;

        SmallObjectOptimizer(const SmallObjectOptimizer& other) = delete;
        SmallObjectOptimizer& operator=(const SmallObjectOptimizer& other) = delete;

        SmallObjectOptimizer(SmallObjectOptimizer&& other) noexcept
        {
            memcpy(this, &other, std::max(sizeof(allocated), sizeof(inlined)));
            other.allocated = nullptr;
        }

        SmallObjectOptimizer& operator=(SmallObjectOptimizer&& other) noexcept
        {
            std::destroy_at(this);
            return *std::construct_at(this, std::move(other));
        }

        ~SmallObjectOptimizer()
        {
            if (!is_inline())
            {
#pragma warning(suppress : 26408) // Avoid malloc() and free(), prefer the nothrow version of new with delete (r.10).
                free(allocated);
            }
        }

        T* initialize(size_t byteSize)
        {
            if (would_inline(byteSize))
            {
                return &inlined;
            }

#pragma warning(suppress : 26408) // Avoid malloc() and free(), prefer the nothrow version of new with delete (r.10).
            allocated = THROW_IF_NULL_ALLOC(static_cast<T*>(malloc(byteSize)));
            return allocated;
        }

        constexpr bool would_inline(size_t byteSize) const noexcept
        {
            return byteSize <= sizeof(T);
        }

        bool is_inline() const noexcept
        {
            // VSO-1430353: __builtin_bitcast crashes the compiler under /permissive-. (BODGY)
#pragma warning(suppress : 26490) // Don't use reinterpret_cast (type.1).
            return (reinterpret_cast<uintptr_t>(allocated) & 1) != 0;
        }

        const T* data() const noexcept
        {
            return is_inline() ? &inlined : allocated;
        }

        size_t size() const noexcept
        {
            return is_inline() ? sizeof(inlined) : _msize(allocated);
        }
    };

    // These flags are shared with shader_ps.hlsl.
    // If you change this be sure to copy it over to shader_ps.hlsl.
    //
    // clang-format off
    enum class CellFlags : u32
    {
        None            = 0x00000000,
        Inlined         = 0x00000001,

        ColoredGlyph    = 0x00000002,

        Cursor          = 0x00000008,
        Selected        = 0x00000010,

        BorderLeft      = 0x00000020,
        BorderTop       = 0x00000040,
        BorderRight     = 0x00000080,
        BorderBottom    = 0x00000100,
        Underline       = 0x00000200,
        UnderlineDotted = 0x00000400,
        UnderlineDouble = 0x00000800,
        Strikethrough   = 0x00001000,
    };
    // clang-format on
    ATLAS_FLAG_OPS(CellFlags, u32)

    // This structure is shared with the GPU shader and needs to follow certain alignment rules.
    // You can generally assume that only u32 or types of that alignment are allowed.
    struct Cell
    {
        alignas(u32) u16x2 tileIndex;
        alignas(u32) CellFlags flags = CellFlags::None;
        u32x2 color;
    };

    struct AtlasKeyAttributes
    {
        u16 inlined : 1;
        u16 bold : 1;
        u16 italic : 1;
        u16 cellCount : 13;

        ATLAS_POD_OPS(AtlasKeyAttributes)
    };

    struct AtlasKeyData
    {
        AtlasKeyAttributes attributes;
        u16 charCount;
        wchar_t chars[14];
    };

    struct AtlasKey
    {
        AtlasKey(AtlasKeyAttributes attributes, u16 charCount, const wchar_t* chars)
        {
            const auto size = dataSize(charCount);
            const auto data = _data.initialize(size);
            attributes.inlined = _data.would_inline(size);
            data->attributes = attributes;
            data->charCount = charCount;
            memcpy(&data->chars[0], chars, static_cast<size_t>(charCount) * sizeof(AtlasKeyData::chars[0]));
        }

        const AtlasKeyData* data() const noexcept
        {
            return _data.data();
        }

        size_t hash() const noexcept
        {
            const auto d = data();
#pragma warning(suppress : 26490) // Don't use reinterpret_cast (type.1).
            return std::_Fnv1a_append_bytes(std::_FNV_offset_basis, reinterpret_cast<const u8*>(d), dataSize(d->charCount));
        }

        bool operator==(const AtlasKey& rhs) const noexcept
        {
            const auto a = data();
            const auto b = rhs.data();
            return a->charCount == b->charCount && memcmp(a, b, dataSize(a->charCount)) == 0;
        }

    private:
        SmallObjectOptimizer<AtlasKeyData> _data;

        static constexpr size_t dataSize(u16 charCount) noexcept
        {
            // This returns the actual byte size of a AtlasKeyData struct for the given charCount.
            // The `wchar_t chars[2]` is only a buffer for the inlined variant after
            // all and the actual charCount can be smaller or larger. Due to this we
            // remove the size of the `chars` array and add it's true length on top.
            return sizeof(AtlasKeyData) - sizeof(AtlasKeyData::chars) + static_cast<size_t>(charCount) * sizeof(AtlasKeyData::chars[0]);
        }
    };

    struct CachedGlyphLayout
    {
        wil::com_ptr<IDWriteTextLayout> textLayout;
        f32x2 halfSize;
        f32x2 offset;
        f32x2 scale;
        D2D1_DRAW_TEXT_OPTIONS options = D2D1_DRAW_TEXT_OPTIONS_NONE;
        bool scalingRequired = false;

        explicit operator bool() const noexcept;
        void reset() noexcept;
        void applyScaling(ID2D1RenderTarget* d2dRenderTarget, D2D1_POINT_2F origin) const noexcept;
        void undoScaling(ID2D1RenderTarget* d2dRenderTarget) const noexcept;
    };

    struct AtlasValueData
    {
        CellFlags flags = CellFlags::None;
        u16x2 coords[7];
    };

    struct AtlasValue
    {
        AtlasValue(CellFlags flags, u16 cellCount, u16x2** coords)
        {
            __assume(coords != nullptr);
            const auto size = dataSize(cellCount);
            const auto data = _data.initialize(size);
            WI_SetFlagIf(flags, CellFlags::Inlined, _data.would_inline(size));
            data->flags = flags;
            *coords = &data->coords[0];
        }

        const AtlasValueData* data() const noexcept
        {
            return _data.data();
        }

        CachedGlyphLayout cachedLayout;

    private:
        SmallObjectOptimizer<AtlasValueData> _data;

        static constexpr size_t dataSize(u16 coordCount) noexcept
        {
            return sizeof(AtlasValueData) - sizeof(AtlasValueData::coords) + static_cast<size_t>(coordCount) * sizeof(AtlasValueData::coords[0]);
        }
    };

    struct AtlasKeyHasher
    {
        using is_transparent = int;

        size_t operator()(const AtlasKey& v) const noexcept
        {
            return v.hash();
        }

        size_t operator()(const std::list<std::pair<AtlasKey, AtlasValue>>::iterator& v) const noexcept
        {
            return operator()(v->first);
        }
    };

    struct AtlasKeyEq
    {
        using is_transparent = int;

        bool operator()(const AtlasKey& a, const std::list<std::pair<AtlasKey, AtlasValue>>::iterator& b) const noexcept
        {
            return a == b->first;
        }

        bool operator()(const std::list<std::pair<AtlasKey, AtlasValue>>::iterator& a, const std::list<std::pair<AtlasKey, AtlasValue>>::iterator& b) const noexcept
        {
            return operator()(a->first, b);
        }
    };

    struct TileHashMap
    {
        using iterator = std::list<std::pair<AtlasKey, AtlasValue>>::iterator;

        TileHashMap() noexcept = default;

        iterator end() noexcept
        {
            return _lru.end();
        }

        iterator find(const AtlasKey& key)
        {
            const auto it = _map.find(key);
            if (it != _map.end())
            {
                // Move the key to the head of the LRU queue.
                makeNewest(*it);
                return *it;
            }
            return end();
        }

        iterator insert(AtlasKey&& key, AtlasValue&& value)
        {
            // Insert the key/value right at the head of the LRU queue, just like find().
            //
            // && decays to & if the argument is named, because C++ is a simple language
            // and so you have to std::move it again, because C++ is a simple language.
            _lru.emplace_front(std::move(key), std::move(value));
            auto it = _lru.begin();
            _map.emplace(it);
            return it;
        }

        void makeNewest(const iterator& it)
        {
            _lru.splice(_lru.begin(), _lru, it);
        }

        void popOldestTiles(std::vector<u16x2>& out) noexcept
        {
            Expects(!_lru.empty());
            const auto it = --_lru.end();

            const auto key = it->first.data();
            const auto value = it->second.data();
            const auto beg = &value->coords[0];
            const auto cellCount = key->attributes.cellCount;

            const auto offset = out.size();
            out.resize(offset + cellCount);
            std::copy_n(beg, cellCount, out.begin() + offset);

            _map.erase(it);
            _lru.pop_back();
        }

    private:
        // Please don't copy this code. It's a proof-of-concept.
        // If you need a LRU hash-map, write a custom one with an intrusive
        // prev/next linked list (it's easier than you might think!).
        std::list<std::pair<AtlasKey, AtlasValue>> _lru;
        std::unordered_set<iterator, AtlasKeyHasher, AtlasKeyEq> _map;
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

        explicit TileAllocator(u16x2 tileSize) noexcept :
            _tileSize{ tileSize }
        {
            const auto initialSize = std::max(u16{ _absoluteMinSize }, std::bit_ceil(std::max(tileSize.x, tileSize.y)));
            _size = { initialSize, initialSize };
            _limit = { gsl::narrow_cast<u16>(initialSize - _tileSize.x), gsl::narrow_cast<u16>(initialSize - _tileSize.y) };
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
        // Coincidentally that's exactly what we want as the cursor texture lives at {0, 0}.
        u16x2 _pos;
        u16 _originX = 0;
        // Indicates whether we've exhausted our Z pattern across the atlas texture.
        // If this is false, we have to snatch tiles back from TileHashMap.
        bool _canGenerate = true;
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
        alignas(sizeof(u32)) u32 underlinePos = 0;
        alignas(sizeof(u32)) u32 underlineWidth = 0;
        alignas(sizeof(u32)) u32 strikethroughPos = 0;
        alignas(sizeof(u32)) u32 strikethroughWidth = 0;
        alignas(sizeof(u32x2)) u32x2 doubleUnderlinePos;
        alignas(sizeof(u32)) u32 thinLineWidth = 0;
        alignas(sizeof(u32)) u32 backgroundColor = 0;
        alignas(sizeof(u32)) u32 cursorColor = 0;
        alignas(sizeof(u32)) u32 selectionColor = 0;
        alignas(sizeof(u32)) u32 useClearType = 0;
#pragma warning(suppress : 4324) // 'ConstBuffer': structure was padded due to alignment specifier
    };

    struct alignas(16) CustomConstBuffer
    {
        // WARNING: Same rules as for ConstBuffer above apply.
        alignas(sizeof(f32)) f32 time = 0;
        alignas(sizeof(f32)) f32 scale = 0;
        alignas(sizeof(f32x2)) f32x2 resolution;
        alignas(sizeof(f32x4)) f32x4 background;
#pragma warning(suppress : 4324) // 'CustomConstBuffer': structure was padded due to alignment specifier
    };

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

    struct TargetSettings
    {
        HWND hwnd = nullptr;
        bool enableTransparentBackground = false;
    };

    struct FontSettings
    {
        wil::com_ptr<IDWriteFontCollection> fontCollection;
        std::wstring fontName;
        std::vector<DWRITE_FONT_FEATURE> fontFeatures;
        std::vector<DWRITE_FONT_AXIS_VALUE> fontAxisValues;
        float baselineInDIP = 0.0f;
        float fontSizeInDIP = 0.0f;
        f32 advanceScale = 0;
        u16x2 cellSize;
        u16 fontWeight = 0;
        u16 underlinePos = 0;
        u16 underlineWidth = 0;
        u16 strikethroughPos = 0;
        u16 strikethroughWidth = 0;
        u16x2 doubleUnderlinePos;
        u16 thinLineWidth = 0;
        u16 dpi = 0;
    };

    struct CursorSettings
    {
        ATLAS_POD_OPS(CursorSettings)

        u32 cursorColor = INVALID_COLOR;
        u16 cursorType = gsl::narrow_cast<u16>(CursorType::Legacy);
        u8 heightPercentage = 20;
        u8 _padding = 0;
    };

    struct MiscellaneousSettings
    {
        u32 backgroundColor = 0;
        u32 selectionColor = 0x7fffffff;
        u8 antialiasingMode = D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE;
        std::wstring customPixelShaderPath;
        bool useRetroTerminalEffect = false;
    };

    struct Settings
    {
        static auto invalidated() noexcept
        {
            return til::generational<Settings>{
                til::generation_t{ 1 },
                til::generational<TargetSettings>{ til::generation_t{ 1 } },
                til::generational<FontSettings>{ til::generation_t{ 1 } },
                til::generational<CursorSettings>{ til::generation_t{ 1 } },
                til::generational<MiscellaneousSettings>{ til::generation_t{ 1 } },
            };
        }

        til::generational<TargetSettings> target;
        til::generational<FontSettings> font;
        til::generational<CursorSettings> cursor;
        til::generational<MiscellaneousSettings> misc;
        u16x2 targetSize;
        u16x2 cellCount;
    };

    struct FontDependents
    {
        wil::com_ptr<IDWriteTextFormat> textFormats[2][2];
        Buffer<DWRITE_FONT_AXIS_VALUE> textFormatAxes[2][2];
        wil::com_ptr<IDWriteTypography> typography;
        f32 dipPerPixel = 1.0f; // caches USER_DEFAULT_SCREEN_DPI / dpi
        f32 pixelPerDIP = 1.0f; // caches dpi / USER_DEFAULT_SCREEN_DPI
        f32x2 cellSizeDIP; // caches cellSize in DIP
    };

    struct Dependents
    {
        FontDependents font;
    };

    struct RenderingPayload
    {
        // Parameters which are constant across backends.
        wil::com_ptr<ID2D1Factory> d2dFactory;
        wil::com_ptr<IDWriteFactory2> dwriteFactory;
        wil::com_ptr<IDWriteFontFallback> systemFontFallback;
        wil::com_ptr<IDWriteTextAnalyzer1> textAnalyzer;
        wil::com_ptr<IDWriteRenderingParams1> renderingParams;
        f32 gamma = 0;
        f32 cleartypeEnhancedContrast = 0;
        f32 grayscaleEnhancedContrast = 0;
        std::function<void(HRESULT)> warningCallback;
        std::function<void(HANDLE)> swapChainChangedCallback;

        // Parameters which are constant for the existence of the backend.
        wil::com_ptr<IDXGIFactory3> dxgiFactory;

        // Parameters which change seldom.
        til::generational<Settings> s;
        Dependents d;

        // Parameters which change every frame.
        Buffer<Cell, 32> cells;
        Buffer<TileHashMap::iterator> cellGlyphMapping;
        std::vector<TileHashMap::iterator> glyphQueue;
        TileHashMap glyphs;
        TileAllocator tileAllocator;
        til::rect dirtyRect;
        i16 scrollOffset = 0;
    };
    class SwapChainManager
    {
    public:
        void UpdateSwapChainSettings(const RenderingPayload& payload, IUnknown* device, auto&& prepareResize, auto&& prepareRecreate)
        {
            if (_targetGeneration != payload.s->target.generation())
            {
                if (_swapChain)
                {
                    prepareRecreate();
                }
                _createSwapChain(payload, device);
            }
            else if (_targetSize != payload.s->targetSize)
            {
                _targetSize = payload.s->targetSize;
                prepareResize();
                THROW_IF_FAILED(_swapChain->ResizeBuffers(0, _targetSize.x, _targetSize.y, DXGI_FORMAT_UNKNOWN, DXGI_SWAP_CHAIN_FLAG_FRAME_LATENCY_WAITABLE_OBJECT));
            }
        }

        void UpdateFontSettings(const RenderingPayload& payload) const;

        wil::com_ptr<ID3D11Texture2D> GetBuffer() const;
        void Present(const RenderingPayload& payload);
        void WaitUntilCanRender() noexcept;

    private:
        void _createSwapChain(const RenderingPayload& payload, IUnknown* device);

        wil::com_ptr<IDXGISwapChain2> _swapChain;
        wil::unique_handle _swapChainHandle;
        wil::unique_handle _frameLatencyWaitableObject;
        til::generation_t _targetGeneration{};
        u16x2 _targetSize;
        bool _waitForPresentation = false;
    };

    class IDriver
    {
    public:
        virtual ~IDriver() = default;

        virtual void Render(const RenderingPayload& payload) = 0;
        virtual void WaitUntilCanRender() noexcept = 0;

    protected:
        static CachedGlyphLayout _getCachedGlyphLayout(const RenderingPayload& payload, const wchar_t* chars, u16 charsLength, u16 cellCount, IDWriteTextFormat* textFormat, bool coloredGlyph);
        static void _drawCursor(const RenderingPayload& payload, ID2D1RenderTarget* renderTarget, u16r rect, ID2D1Brush* brush, bool clear);
    };

    class DriverD2D : public IDriver
    {
    public:
        DriverD2D(wil::com_ptr<ID3D11Device2> device, wil::com_ptr<ID3D11DeviceContext2> deviceContext);

        void Render(const RenderingPayload& payload) override;
        void WaitUntilCanRender() noexcept override;

    private:
        u16 _d2dDrawGlyph(const RenderingPayload& payload, const TileHashMap::iterator& it, u16x2 coord, u32 color);
        void _d2dDrawLine(const RenderingPayload& payload, u16r rect, u16 pos, u16 width, u32 color, ID2D1StrokeStyle* strokeStyle = nullptr);
        void _d2dFillRectangle(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererCursor(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererSelected(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererUnderline(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererUnderlineDotted(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererUnderlineDouble(const RenderingPayload& payload, u16r rect, u32 color);
        void _d2dCellFlagRendererStrikethrough(const RenderingPayload& payload, u16r rect, u32 color);
        ID2D1Brush* _brushWithColor(u32 color);

        til::generation_t _generation{};
        til::generation_t _targetGeneration{};
        til::generation_t _fontGeneration{};
        til::generation_t _cursorGeneration{};
        til::generation_t _miscGeneration{};

        SwapChainManager _swapChainManager;
        wil::com_ptr<ID3D11Device2> _device;
        wil::com_ptr<ID3D11DeviceContext2> _deviceContext;
        wil::com_ptr<ID2D1DeviceContext1> _d2dRenderTarget;
        wil::com_ptr<ID2D1SolidColorBrush> _brush;
        wil::com_ptr<ID2D1StrokeStyle> _dottedStrokeStyle;
        u32 _brushColor = 0;
    };

    class DriverD3D11 : public IDriver
    {
    public:
        DriverD3D11(wil::com_ptr<ID3D11Device2> device, wil::com_ptr<ID3D11DeviceContext2> deviceContext);

        void Render(const RenderingPayload& payload) override;
        void WaitUntilCanRender() noexcept override;

    private:
        void _createCustomShaderResources(const RenderingPayload& p);
        void _renderWithCustomShader(const RenderingPayload& p) const;
        void _setShaderResources(const RenderingPayload& p) const;
        void _updateConstantBuffer(const RenderingPayload& p) const noexcept;
        void _adjustAtlasSize(const RenderingPayload& p);
        void _processGlyphQueue(const RenderingPayload& p);
        void _drawGlyph(const RenderingPayload& payload, const TileHashMap::iterator& it) const;

#ifndef NDEBUG
        struct DebugState
        {
            std::filesystem::path sourceDirectory;
            wil::unique_folder_change_reader_nothrow sourceCodeWatcher;
            std::atomic<int64_t> sourceCodeInvalidationTime{ INT64_MAX };
        } _d;
#endif

        til::generation_t _generation{};
        til::generation_t _fontGeneration{};
        til::generation_t _cursorGeneration{};
        til::generation_t _miscGeneration{};
        u16x2 _targetSize;
        u16x2 _cellCount;
        u16x2 _atlasSizeInPixel;

        SwapChainManager _swapChainManager;

        wil::com_ptr<ID3D11Device2> _device;
        wil::com_ptr<ID3D11DeviceContext2> _deviceContext;
        wil::com_ptr<ID3D11RenderTargetView> _renderTargetView;
        wil::com_ptr<ID3D11VertexShader> _vertexShader;
        wil::com_ptr<ID3D11PixelShader> _pixelShader;
        wil::com_ptr<ID3D11Buffer> _constantBuffer;
        wil::com_ptr<ID3D11Buffer> _cellBuffer;
        wil::com_ptr<ID3D11ShaderResourceView> _cellView;
        wil::com_ptr<ID3D11Texture2D> _atlasBuffer;
        wil::com_ptr<ID3D11ShaderResourceView> _atlasView;
        wil::com_ptr<ID2D1DeviceContext1> _d2dRenderTarget;
        wil::com_ptr<ID2D1SolidColorBrush> _brush;

        wil::com_ptr<ID3D11Texture2D> _customOffscreenTexture;
        wil::com_ptr<ID3D11ShaderResourceView> _customOffscreenTextureView;
        wil::com_ptr<ID3D11RenderTargetView> _customOffscreenTextureTargetView;
        wil::com_ptr<ID3D11VertexShader> _customVertexShader;
        wil::com_ptr<ID3D11PixelShader> _customPixelShader;
        wil::com_ptr<ID3D11Buffer> _customShaderConstantBuffer;
        wil::com_ptr<ID3D11SamplerState> _customShaderSamplerState;
        std::chrono::steady_clock::time_point _customShaderStartTime;

        bool _requiresContinuousRedraw = false;
    };

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
        [[nodiscard]] HRESULT PrepareLineTransform(LineRendition lineRendition, til::CoordType targetRow, til::CoordType viewportLeft) noexcept override;
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
        [[nodiscard]] std::wstring_view GetPixelShaderPath() noexcept override;
        [[nodiscard]] bool GetRetroTerminalEffect() const noexcept override;
        [[nodiscard]] float GetScaling() const noexcept override;
        [[nodiscard]] Types::Viewport GetViewportInCharacters(const Types::Viewport& viewInPixels) const noexcept override;
        [[nodiscard]] Types::Viewport GetViewportInPixels(const Types::Viewport& viewInCharacters) const noexcept override;
        // DxRenderer - setter
        void SetAntialiasingMode(D2D1_TEXT_ANTIALIAS_MODE antialiasingMode) noexcept override;
        void SetCallback(std::function<void(HANDLE)> pfn) noexcept override;
        void EnableTransparentBackground(const bool isTransparent) noexcept override;
        void SetForceFullRepaintRendering(bool enable) noexcept override;
        [[nodiscard]] HRESULT SetHwnd(HWND hwnd) noexcept override;
        void SetPixelShaderPath(std::wstring_view value) noexcept override;
        void SetRetroTerminalEffect(bool enable) noexcept override;
        void SetSelectionBackground(COLORREF color, float alpha = 0.5f) noexcept override;
        void SetSoftwareRendering(bool enable) noexcept override;
        void SetWarningCallback(std::function<void(HRESULT)> pfn) noexcept override;
        [[nodiscard]] HRESULT SetWindowSize(til::size pixels) noexcept override;
        [[nodiscard]] HRESULT UpdateFont(const FontInfoDesired& pfiFontInfoDesired, FontInfo& fiFontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes) noexcept override;
        void UpdateHyperlinkHoveredId(uint16_t hoveredId) noexcept override;

    private:
        // AtlasEngine.cpp
        [[nodiscard]] HRESULT _handleException(const wil::ResultException& exception) noexcept;
        __declspec(noinline) void _recreateSizeDependentResources();
        __declspec(noinline) void _recreateFontDependentResources();
        IDWriteTextFormat* _getTextFormat(bool bold, bool italic) const noexcept;
        const Buffer<DWRITE_FONT_AXIS_VALUE>& _getTextFormatAxis(bool bold, bool italic) const noexcept;
        Cell* _getCell(u16 x, u16 y) noexcept;
        TileHashMap::iterator* _getCellGlyphMapping(u16 x, u16 y) noexcept;
        void _setCellFlags(u16r coords, CellFlags mask, CellFlags bits) noexcept;
        void _flushBufferLine();
        bool _emplaceGlyph(IDWriteFontFace* fontFace, size_t bufferPos1, size_t bufferPos2);

        // AtlasEngine.api.cpp
        void _resolveTransparencySettings() noexcept;
        void _updateFont(const wchar_t* faceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, const std::unordered_map<std::wstring_view, uint32_t>& features, const std::unordered_map<std::wstring_view, float>& axes);
        static void _resolveFontMetrics(u16 dpi, const wchar_t* faceName, const FontInfoDesired& fontInfoDesired, FontInfo& fontInfo, FontSettings* fontMetrics = nullptr);

        // AtlasEngine.r.cpp
        void _recreateBackend();

        static constexpr u16 u16min = 0x0000;
        static constexpr u16 u16max = 0xffff;
        static constexpr i16 i16min = -0x8000;
        static constexpr i16 i16max = 0x7fff;
        static constexpr u16r invalidatedAreaNone = { u16max, u16max, u16min, u16min };
        static constexpr u16x2 invalidatedRowsNone{ u16max, u16min };
        static constexpr u16x2 invalidatedRowsAll{ u16min, u16max };

        RenderingPayload _p;

        struct Resources
        {
            std::unique_ptr<IDriver> driver;
            bool useSoftwareRendering = false;
            bool useSoftwareRenderingNext = false;
        } _r;

        struct ApiState
        {
            til::generational<Settings> s = Settings::invalidated();
            u32 backgroundOpaqueMixin = 0xff000000;
            u8 antialiasingMode = D2D1_TEXT_ANTIALIAS_MODE_CLEARTYPE;
            bool enableTransparentBackground = false;
            bool invalidatedTitle = false;
            bool useSoftwareRendering = false;

            std::vector<wchar_t> bufferLine;
            std::vector<u16> bufferLineColumn;
            Buffer<BufferLineMetadata> bufferLineMetadata;
            std::vector<TextAnalysisSinkResult> analysisResults;
            Buffer<u16> clusterMap;
            Buffer<DWRITE_SHAPING_TEXT_PROPERTIES> textProps;
            Buffer<u16> glyphIndices;
            Buffer<DWRITE_SHAPING_GLYPH_PROPERTIES> glyphProps;
            Buffer<f32> glyphAdvances;
            Buffer<DWRITE_GLYPH_OFFSET> glyphOffsets;

            // UpdateDrawingBrushes()
            u32x2 currentColor;
            AtlasKeyAttributes attributes{};
            u16x2 lastPaintBufferLineCoord;
            CellFlags flags = CellFlags::None;
            // UpdateHyperlinkHoveredId()
            u16 hyperlinkHoveredId = 0;
            bool bufferLineWasHyperlinked = false;

            // dirtyRect is a computed value based on invalidatedRows.
            til::rect dirtyRect;
            // These "invalidation" fields are reset in EndPaint()
            u16r invalidatedCursorArea = invalidatedAreaNone;
            u16x2 invalidatedRows = invalidatedRowsNone; // x is treated as "top" and y as "bottom"
            i16 scrollOffset = 0;
        } _api;

#undef ATLAS_POD_OPS
#undef ATLAS_FLAG_OPS
    };
}

namespace Microsoft::Console::Render
{
    using AtlasEngine = Atlas::AtlasEngine;
}
