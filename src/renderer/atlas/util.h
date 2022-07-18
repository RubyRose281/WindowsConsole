#pragma once

namespace Microsoft::Console::Render::Atlas
{
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
    constexpr type operator~(type v) noexcept { return static_cast<type>(~static_cast<underlying>(v)); }                                             \
    constexpr type operator|(type lhs, type rhs) noexcept { return static_cast<type>(static_cast<underlying>(lhs) | static_cast<underlying>(rhs)); } \
    constexpr type operator&(type lhs, type rhs) noexcept { return static_cast<type>(static_cast<underlying>(lhs) & static_cast<underlying>(rhs)); } \
    constexpr void operator|=(type& lhs, type rhs) noexcept { lhs = lhs | rhs; }                                                                     \
    constexpr void operator&=(type& lhs, type rhs) noexcept { lhs = lhs & rhs; }

    template<typename T>
    struct vec2
    {
        T x{};
        T y{};

        ATLAS_POD_OPS(vec2)
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

    using u64 = uint64_t;

    using f32 = float;
    using f32x2 = vec2<f32>;
    using f32x4 = vec4<f32>;

        // MSVC STL (version 22000) implements std::clamp<T>(T, T, T) in terms of the generic
        // std::clamp<T, Predicate>(T, T, T, Predicate) with std::less{} as the argument,
        // which introduces branching. While not perfect, this is still better than std::clamp.
        template<typename T>
        static constexpr T clamp(T val, T min, T max)
        {
            return std::max(min, std::min(max, val));
        }

    // I wrote `Buffer` instead of using `std::vector`, because I want to convey that these things
    // explicitly _don't_ hold resizeable contents, but rather plain content of a fixed size.
    // For instance I didn't want a resizeable vector with a `push_back` method for my fixed-size
    // viewport arrays - that doesn't make sense after all. `Buffer` also doesn't initialize
    // contents to zero, allowing rapid creation/destruction and you can easily specify a custom
    // (over-)alignment which can improve rendering perf by up to ~20% over `std::vector`.
    template<typename T, size_t Alignment = alignof(T)>
    struct Buffer
    {
        constexpr Buffer() noexcept = default;

        explicit Buffer(size_t size) :
            _data{ allocate(size) },
            _size{ size }
        {
            std::uninitialized_default_construct_n(_data, size);
        }

        Buffer(const T* data, size_t size) :
            _data{ allocate(size) },
            _size{ size }
        {
            // Changing the constructor arguments to accept std::span might
            // be a good future extension, but not to improve security here.
            // You can trivially construct std::span's from invalid ranges.
            // Until then the raw-pointer style is more practical.
#pragma warning(suppress : 26459) // You called an STL function '...' with a raw pointer parameter at position '3' that may be unsafe [...].
            std::uninitialized_copy_n(data, size, _data);
        }

        ~Buffer()
        {
            destroy();
        }

        Buffer(Buffer&& other) noexcept :
            _data{ std::exchange(other._data, nullptr) },
            _size{ std::exchange(other._size, 0) }
        {
        }

#pragma warning(suppress : 26432) // If you define or delete any default operation in the type '...', define or delete them all (c.21).
        Buffer& operator=(Buffer&& other) noexcept
        {
            destroy();
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

        void destroy() noexcept
        {
            std::destroy_n(_data, _size);
            deallocate(_data);
        }

        T* _data = nullptr;
        size_t _size = 0;
    };

    struct heapapi_pool
    {
        explicit heapapi_pool(size_t initialSize) :
            _handle{ THROW_LAST_ERROR_IF_NULL(HeapCreate(0, initialSize, 0)) }
        {
            ULONG HeapInformation = 0;
            HeapQueryInformation(_handle, HeapCompatibilityInformation, &HeapInformation, sizeof(HeapInformation), nullptr);
            UNREFERENCED_PARAMETER(HeapInformation);
        }

        ~heapapi_pool()
        {
            destroy();
        }

        heapapi_pool(const heapapi_pool&) = delete;
        heapapi_pool& operator=(const heapapi_pool&) = delete;

        heapapi_pool(heapapi_pool&& other) noexcept :
            _handle{ std::exchange(other._handle, nullptr) }
        {
        }
        heapapi_pool& operator=(heapapi_pool&& other) noexcept
        {
            destroy();
            _handle = std::exchange(other._handle, nullptr);
            return *this;
        }

        [[nodiscard]] void* allocate(size_t bytes) const
        {
            const auto ptr = HeapAlloc(_handle, HEAP_NO_SERIALIZE, bytes);
            if (!ptr)
            {
                throw std::bad_alloc{};
            }
            return ptr;
        }

        void deallocate(void* ptr) const noexcept
        {
            HeapFree(_handle, HEAP_NO_SERIALIZE, ptr);
        }

        void release()
        {
        }

    private:
        void destroy() noexcept
        {
            if (_handle && !HeapDestroy(_handle))
            {
                LOG_LAST_ERROR();
            }
        }

        // I could have used wil::unique_hheap here, but
        // I wanted to use LOG_LAST_ERROR on HeapDestroy.
        HANDLE _handle = nullptr;
    };

}
