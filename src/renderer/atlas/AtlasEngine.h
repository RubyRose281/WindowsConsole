// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#pragma once

#include <d2d1.h>
#include <d3d11_1.h>
#include <dwrite_3.h>

#include "../../renderer/inc/IRenderEngine.hpp"

namespace Microsoft::Console::Render
{
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

        // Some helper classes for the implementation.
        // public because I don't want to sprinkle the code with friends.
    public:
#define ATLAS_POD_OPS(type)                                    \
    constexpr bool operator==(const type& rhs) const noexcept  \
    {                                                          \
        return __builtin_memcmp(this, &rhs, sizeof(rhs)) == 0; \
    }                                                          \
                                                               \
    constexpr bool operator!=(const type& rhs) const noexcept  \
    {                                                          \
        return __builtin_memcmp(this, &rhs, sizeof(rhs)) != 0; \
    }

#define ATLAS_FLAG_OPS(type, underlying)                                                                                                                    \
    friend constexpr type operator~(type v) noexcept { return static_cast<type>(~static_cast<underlying>(v)); }                                             \
    friend constexpr type operator|(type lhs, type rhs) noexcept { return static_cast<type>(static_cast<underlying>(lhs) | static_cast<underlying>(rhs)); } \
    friend constexpr type operator&(type lhs, type rhs) noexcept { return static_cast<type>(static_cast<underlying>(lhs) & static_cast<underlying>(rhs)); } \
    friend constexpr void operator|=(type& lhs, type rhs) noexcept { lhs = lhs | rhs; }                                                                     \
    friend constexpr void operator&=(type& lhs, type rhs) noexcept { lhs = lhs & rhs; }

        template<typename T>
        struct vec2
        {
            T x{};
            T y{};

            ATLAS_POD_OPS(vec2)

            constexpr vec2 operator/(const vec2& rhs) noexcept
            {
                assert(rhs.x != 0 && rhs.y != 0);
                return { gsl::narrow_cast<T>(x / rhs.x), gsl::narrow_cast<T>(y / rhs.y) };
            }
        };

        template<typename T>
        struct vec4
        {
            T x{};
            T y{};
            T z{};
            T w{};

            ATLAS_POD_OPS(vec4)
        };

        template<typename T>
        struct rect
        {
            T left{};
            T top{};
            T right{};
            T bottom{};

            ATLAS_POD_OPS(rect)

            constexpr bool non_empty() noexcept
            {
                return (left < right) & (top < bottom);
            }
        };

        using u8 = uint8_t;

        using u16 = uint16_t;
        using u16x2 = vec2<u16>;
        using u16r = rect<u16>;

        using i16 = int16_t;

        using u32 = uint32_t;
        using u32x2 = vec2<u32>;

        using i32 = int32_t;

        using f32 = float;
        using f32x2 = vec2<f32>;
        using f32x4 = vec4<f32>;

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

    private:
        template<typename T, size_t Alignment = alignof(T)>
        struct Buffer
        {
            constexpr Buffer() noexcept = default;

            explicit Buffer(size_t size) :
                _data{ allocate(size) },
                _size{ size }
            {
            }

            Buffer(const T* data, size_t size) :
                _data{ allocate(size) },
                _size{ size }
            {
                static_assert(std::is_trivially_copyable_v<T>);
                memcpy(_data, data, size * sizeof(T));
            }

            ~Buffer()
            {
                deallocate(_data);
            }

            Buffer(Buffer&& other) noexcept :
                _data{ std::exchange(other._data, nullptr) },
                _size{ std::exchange(other._size, 0) }
            {
            }

#pragma warning(suppress : 26432) // If you define or delete any default operation in the type '...', define or delete them all (c.21).
            Buffer& operator=(Buffer&& other) noexcept
            {
                deallocate(_data);
                _data = std::exchange(other._data, nullptr);
                _size = std::exchange(other._size, 0);
                return *this;
            }

            explicit operator bool() const noexcept
            {
                return _data != nullptr;
            }

            T& operator[](size_t index) noexcept
            {
                assert(index < _size);
                return _data[index];
            }

            const T& operator[](size_t index) const noexcept
            {
                assert(index < _size);
                return _data[index];
            }

            T* data() noexcept
            {
                return _data;
            }

            const T* data() const noexcept
            {
                return _data;
            }

            size_t size() const noexcept
            {
                return _size;
            }

            T* begin() noexcept
            {
                return _data;
            }

            T* begin() const noexcept
            {
                return _data;
            }

            T* end() noexcept
            {
                return _data + _size;
            }

            T* end() const noexcept
            {
                return _data + _size;
            }

        private:
            // These two functions don't need to use scoped objects or standard allocators,
            // since this class is in fact an scoped allocator object itself.
#pragma warning(push)
#pragma warning(disable : 26402) // Return a scoped object instead of a heap-allocated if it has a move constructor (r.3).
#pragma warning(disable : 26409) // Avoid calling new and delete explicitly, use std::make_unique<T> instead (r.11).
            static T* allocate(size_t size)
            {
                if constexpr (Alignment <= __STDCPP_DEFAULT_NEW_ALIGNMENT__)
                {
                    return static_cast<T*>(::operator new(size * sizeof(T)));
                }
                else
                {
                    return static_cast<T*>(::operator new(size * sizeof(T), static_cast<std::align_val_t>(Alignment)));
                }
            }

            static void deallocate(T* data) noexcept
            {
                if constexpr (Alignment <= __STDCPP_DEFAULT_NEW_ALIGNMENT__)
                {
                    ::operator delete(data);
                }
                else
                {
                    ::operator delete(data, static_cast<std::align_val_t>(Alignment));
                }
            }
#pragma warning(pop)

            T* _data = nullptr;
            size_t _size = 0;
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

        // These flags are shared with shader_ps.hlsl.
        // If you change this be sure to copy it over to shader_ps.hlsl.
        //
        // clang-format off
        enum class MetaFlags : u32
        {
            None            = 0x00000000,
            Initialized     = 0x00000001,
            HeapdKey        = 0x00000002,
            HeapdCoords     = 0x00000004,

            ColoredGlyph    = 0x00000008,

            Cursor          = 0x00000010,
            Selected        = 0x00000020,

            BorderLeft      = 0x00000040,
            BorderTop       = 0x00000080,
            BorderRight     = 0x00000100,
            BorderBottom    = 0x00000200,
            Underline       = 0x00000400,
            UnderlineDotted = 0x00000800,
            UnderlineDouble = 0x00001000,
            Strikethrough   = 0x00002000,
        };
        // clang-format on
        ATLAS_FLAG_OPS(MetaFlags, u32)

        // This structure is shared with the GPU shader and needs to follow certain alignment rules.
        // You can generally assume that only u32 or types of that alignment are allowed.
        struct Cell
        {
            alignas(u32) u16x2 tileIndex;
            alignas(u32) MetaFlags flags = MetaFlags::None;
            u32x2 color;
        };

        enum class AtlasEntryKeyAttributes : u16
        {
            None = 0x0,
            Intense = 0x1,
            Italic = 0x2,
            // The Intense and Italic flags are used to directly index into arrays.
            // If you ever add more flags here, make sure to fix _getTextFormat()
            // and _getTextFormatAxis() and to add a `& 3` mask for instance.
        };
        ATLAS_FLAG_OPS(AtlasEntryKeyAttributes, u16)

        struct AtlasEntryKey
        {
            AtlasEntryKeyAttributes attributes;
            u16 charCount;
            u16 coordCount;
            wchar_t chars[13];
        };

        struct alignas(std::hardware_destructive_interference_size) AtlasEntry
        {
            // The ordering of the fields is chosen in this rather unsightly way so that this struct is optimally compact
            // and fits on a single cache line (unless it's x86, which I didn't pay any mind to still optimize for).
            u32 hash = 0;
            MetaFlags flags = MetaFlags::None;
            union
            {
                AtlasEntryKey* allocatedKey = nullptr;
                AtlasEntryKey inlineKey;
            };
            union
            {
                u16x2* allocatedCoords = nullptr;
                u16x2 inlineCoords[2];
            };
            AtlasEntry* prev = nullptr;
            AtlasEntry* next = nullptr;

            AtlasEntry() = default;

            AtlasEntry(AtlasEntryKeyAttributes attributes, u16 charCount, u16 coordCount, const wchar_t* chars)
            {
                // _emplaceGlyph has a Expects() assertions for this, which isn't compiled out in Release mode.
                // If charCount was 0 our memset()/memcpy() code below would fail.
                assert(charCount != 0);
                assert(coordCount != 0);

                // hash_data() only works with data fully aligned to u32 (including the length).
                const auto totalSize = nextMultipleOf(keySize(charCount), sizeof(u32));

                auto key = &inlineKey;
                if (charCount > std::size(inlineKey.chars))
                {
                    key = THROW_IF_NULL_ALLOC(static_cast<AtlasEntryKey*>(malloc(totalSize)));
                    allocatedKey = key;
                    WI_SetFlag(flags, MetaFlags::HeapdKey);
                }

                key->attributes = attributes;
                key->charCount = charCount;
                key->coordCount = coordCount;

                // The hash_data() function only accepts data/length aligned by u32
                // and our chars are made up of wchar_t/u16.
                // --> We need to memset() the last u16 word in our key->chars
                //     because we might otherwise hash uninitialized data.
                {
                    const auto data = reinterpret_cast<u8*>(&key->chars[0]);
                    const auto length = sizeof(wchar_t) * charCount;
                    memset(&data[length - 2], 0, 2);
                    memcpy(data, chars, length);
                }

#pragma warning(suppress : 26490) // Don't use reinterpret_cast (type.1).
                hash = hash_data(reinterpret_cast<const u8*>(key), totalSize);
            }

            // Second part of the constructor
            u16x2* finalize(MetaFlags additionalFlags, u16 coordCount)
            {
                auto coords = &inlineCoords[0];
                if (coordCount > std::size(inlineCoords))
                {
                    coords = THROW_IF_NULL_ALLOC(static_cast<u16x2*>(malloc(sizeof(u16x2) * coordCount)));
                    allocatedCoords = coords;
                    WI_SetFlag(additionalFlags, MetaFlags::HeapdCoords);
                }
                flags |= additionalFlags | MetaFlags::Initialized;
                return coords;
            }

            ~AtlasEntry()
            {
                if (WI_IsFlagSet(flags, MetaFlags::HeapdKey))
                {
                    free(allocatedKey);
                }
                if (WI_IsFlagSet(flags, MetaFlags::HeapdCoords))
                {
                    free(allocatedCoords);
                }
            }

            AtlasEntry(const AtlasEntry& other)
            {
                *this = other;
            }

            AtlasEntry& operator=(const AtlasEntry& other) noexcept
            {
                if (this == &other)
                {
                    return *this;
                }

                if (WI_IsFlagSet(flags, MetaFlags::HeapdKey))
                {
                    free(allocatedKey);
                }
                if (WI_IsFlagSet(flags, MetaFlags::HeapdCoords))
                {
                    free(allocatedCoords);
                }

                memcpy(this, &other, sizeof(AtlasEntry));

                bool allocFailure = false;
                if (WI_IsFlagSet(flags, MetaFlags::HeapdKey))
                {
                    allocatedKey = static_cast<AtlasEntryKey*>(mallocClone(other.allocatedKey));
                    allocFailure |= !allocatedKey;
                }
                if (WI_IsFlagSet(flags, MetaFlags::HeapdCoords))
                {
                    allocatedCoords = static_cast<u16x2*>(mallocClone(other.allocatedCoords));
                    allocFailure |= !allocatedCoords;
                }
                if (allocFailure)
                {
                    memset(this, 0, sizeof(AtlasEntry));
                    THROW_IF_NULL_ALLOC(nullptr);
                }

                return *this;
            }

            AtlasEntry(AtlasEntry&& other) noexcept
            {
                *this = std::move(other);
            }

            AtlasEntry& operator=(AtlasEntry&& other) noexcept
            {
                if (this != &other)
                {
                    memcpy(this, &other, sizeof(AtlasEntry));
                    memset(&other, 0, sizeof(AtlasEntry));
                }
                return *this;
            }

            bool operator==(const AtlasEntry& other) const noexcept
            {
                const auto& a = key();
                const auto& b = other.key();
                const auto al = keySize(a.charCount);
                const auto bl = keySize(b.charCount);
                return al == bl && memcmp(&a, &b, al) == 0;
            }

            const AtlasEntryKey& key() const noexcept
            {
                return WI_IsFlagClear(flags, MetaFlags::HeapdKey) ? inlineKey : *allocatedKey;
            }

            const u16x2* coords() const noexcept
            {
                return WI_IsFlagClear(flags, MetaFlags::HeapdCoords) ? &inlineCoords[0] : allocatedCoords;
            }

        private:
            static void* mallocClone(void* ref)
            {
                const auto size = _msize(ref);
                const auto data = malloc(size);
                if (data)
                {
                    memcpy(data, ref, size);
                }
                return data;
            }

            static constexpr size_t keySize(size_t charCount) noexcept
            {
                return sizeof(AtlasEntryKey) - sizeof(AtlasEntryKey::chars) + sizeof(wchar_t) * charCount;
            }

            static constexpr size_t nextMultipleOf(size_t n, size_t powerOf2) noexcept
            {
                return (n + powerOf2 - 1) & ~(powerOf2 - 1);
            }

            static u32 hash_data(const u8* beg, size_t length) noexcept
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
                // In testing it seemed sufficient for our purpose of a hash-map key.
                //
                // Copyright 2014-2017 Melissa O'Neill <oneill@pcg-random.org>, and the PCG Project contributors.
                // See oss/pcg/LICENSE-MIT.txt, oss/pcg/LICENSE-APACHE.txt or https://www.pcg-random.org/.
                const int r = h & 63;
                const auto x = gsl::narrow_cast<u32>(h >> 32) ^ gsl::narrow_cast<u32>(h);
                return _rotl(x, r);
            }
        };
        static_assert(sizeof(AtlasEntry) == 64);

        struct BoringHashset
        {
            AtlasEntry& emplace(AtlasEntry&& entry, bool& inserted)
            {
                if (size > entries.size() / 2)
                {
                    std::vector<AtlasEntry> newEntries{ entries.size() * 2 };
                    const auto newMask = (mask << 1) | 1;

                    for (auto& it : entries)
                    {
                        if (it.key().charCount != 0)
                        {
                            for (auto i = it.hash;; ++i)
                            {
                                const auto target = &newEntries[i & newMask];
                                if (target->key().charCount == 0)
                                {
                                    *target = std::move(it);
                                    break;
                                }
                            }
                        }
                    }

                    this->entries = std::move(newEntries);
                    this->mask = newMask;
                }

                AtlasEntry* it;

                for (auto i = entry.hash;; ++i)
                {
                    it = &entries[i & mask];
                    if (*it == entry)
                    {
                        inserted = false;
                        break;
                    }
                    if (it->flags == MetaFlags::None)
                    {
                        *it = std::move(entry);
                        size++;
                        inserted = true;
                        break;
                    }
                }

                if (!tail)
                {
                    tail = it;
                }
                if (head)
                {
                    head->next = it;
                }
                head = it;
                return *it;
            }

        private:
            AtlasEntry* tail = nullptr;
            AtlasEntry* head = nullptr;
            std::vector<AtlasEntry> entries{ 64 };
            size_t size = 0;
            u32 mask = 63;
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
            MetaFlags flags = MetaFlags::None;
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

        // MSVC STL (version 22000) implements std::clamp<T>(T, T, T) in terms of the generic
        // std::clamp<T, Predicate>(T, T, T, Predicate) with std::less{} as the argument,
        // which introduces branching. While not perfect, this is still better than std::clamp.
        template<typename T>
        static constexpr T clamp(T val, T min, T max)
        {
            return std::max(min, std::min(max, val));
        }

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
        void _setCellFlags(u16r coords, MetaFlags mask, MetaFlags bits) noexcept;
        u16x2 _allocateAtlasTile() noexcept;
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
        void _reserveScratchpadSize(u16 minWidth);
        void _processGlyphQueue();
        void _drawGlyph(const AtlasEntry& entry) const;
        void _drawCursor();
        void _copyScratchpadTile(uint32_t scratchpadIndex, u16x2 target, uint32_t copyFlags = 0) const noexcept;

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
            wil::com_ptr<ID3D11Texture2D> atlasScratchpad;
            wil::com_ptr<ID2D1RenderTarget> d2dRenderTarget;
            wil::com_ptr<ID2D1Brush> brush;
            wil::com_ptr<IDWriteTextFormat> textFormats[4];
            Buffer<DWRITE_FONT_AXIS_VALUE> textFormatAxes[4];
            wil::com_ptr<IDWriteTypography> typography;

            Buffer<Cell, 32> cells; // invalidated by ApiInvalidations::Size
            f32x2 cellSizeDIP; // invalidated by ApiInvalidations::Font, caches _api.cellSize but in DIP
            u16x2 cellSize; // invalidated by ApiInvalidations::Font, caches _api.cellSize
            u16x2 cellCount; // invalidated by ApiInvalidations::Font|Size, caches _api.cellCount
            u16 underlinePos = 0;
            u16 strikethroughPos = 0;
            u16 lineThickness = 0;
            u16 dpi = USER_DEFAULT_SCREEN_DPI; // invalidated by ApiInvalidations::Font, caches _api.dpi
            u16 maxEncounteredCellCount = 0;
            u16 scratchpadCellWidth = 0;
            u16x2 atlasSizeInPixelLimit; // invalidated by ApiInvalidations::Font
            u16x2 atlasSizeInPixel; // invalidated by ApiInvalidations::Font
            u16x2 atlasPosition;
            BoringHashset glyphs;
            std::vector<AtlasEntry> glyphQueue;

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
            MetaFlags flags = MetaFlags::None;
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

#undef ATLAS_POD_OPS
#undef ATLAS_FLAG_OPS
    };
}
