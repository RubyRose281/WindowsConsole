// Copyright (c) Microsoft Corporation.
// Licensed under the MIT license.

#pragma once

#pragma warning(push)
// small_vector::_data can reference both non-owned (&_buffer[0]) and owned (new[]) data.
#pragma warning(disable : 26401) // Do not delete a raw pointer that is not an owner<T> (i.11).
// small_vector::_data can reference both non-owned (&_buffer[0]) and owned (new[]) data.
#pragma warning(disable : 26403) // Reset or explicitly delete an owner<T> pointer '...' (r.3).
// small_vector manages a _capacity of potentially uninitialized data. We can't use regular new/delete.
#pragma warning(disable : 26409) // Avoid calling new and delete explicitly, use std::make_unique<T> instead (r.11).
// That's how the STL implemented their std::vector<>::iterator. I simply copied the concept.
#pragma warning(disable : 26434) // Function '...' hides a non-virtual function '...'.
// Functions like front()/back()/operator[]() are explicitly unchecked, just like the std::vector equivalents.
#pragma warning(disable : 26446) // Prefer to use gsl::at() instead of unchecked subscript operator (bounds.4).
// small_vector::_data references potentially uninitialized data and so we can't pass it regular iterators which reference initialized data.
#pragma warning(disable : 26459) // You called an STL function '...' with a raw pointer parameter at position '...' that may be unsafe ... (stl.1).
// small_vector::_data references potentially uninitialized data and so we can't pass it regular iterators which reference initialized data.
#pragma warning(disable : 26481) // Don't use pointer arithmetic. Use span instead (bounds.1).

namespace til
{
    // This class was adopted from std::span<>::iterator.
    template<typename T>
    struct small_vector_const_iterator
    {
        using iterator_concept = std::contiguous_iterator_tag;
        using iterator_category = std::random_access_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = const T*;
        using reference = const T&;

        small_vector_const_iterator() = default;

#if _ITERATOR_DEBUG_LEVEL == 0
        constexpr small_vector_const_iterator(pointer ptr) :
            _ptr{ ptr }
        {
        }
#else // _ITERATOR_DEBUG_LEVEL == 0
        constexpr small_vector_const_iterator(pointer ptr, pointer begin, pointer end) :
            _ptr{ ptr },
            _begin{ begin },
            _end{ end }
        {
        }
#endif // _ITERATOR_DEBUG_LEVEL == 0

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin, "cannot dereference value-initialized iterator");
            _STL_VERIFY(_ptr < _end, "cannot dereference end iterator");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            return *_ptr;
        }

        [[nodiscard]] constexpr pointer operator->() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin, "cannot dereference value-initialized iterator");
            _STL_VERIFY(_ptr < _end, "cannot dereference end iterator");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            return _ptr;
        }

        constexpr small_vector_const_iterator& operator++() noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin, "cannot increment value-initialized iterator");
            _STL_VERIFY(_ptr < _end, "cannot increment iterator past end");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            ++_ptr;
            return *this;
        }

        constexpr small_vector_const_iterator operator++(int) noexcept
        {
            small_vector_const_iterator _Tmp{ *this };
            ++*this;
            return _Tmp;
        }

        constexpr small_vector_const_iterator& operator--() noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin, "cannot decrement value-initialized iterator");
            _STL_VERIFY(_begin < _ptr, "cannot decrement iterator before begin");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            --_ptr;
            return *this;
        }

        constexpr small_vector_const_iterator operator--(int) noexcept
        {
            small_vector_const_iterator _Tmp{ *this };
            --*this;
            return _Tmp;
        }

        constexpr void _Verify_offset([[maybe_unused]] const difference_type _Off) const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            if (_Off != 0)
            {
                _STL_VERIFY(_begin, "cannot seek value-initialized iterator");
            }

            if (_Off < 0)
            {
                _STL_VERIFY(_ptr - _begin >= -_Off, "cannot seek iterator before begin");
            }

            if (_Off > 0)
            {
                _STL_VERIFY(_end - _ptr >= _Off, "cannot seek iterator after end");
            }
#endif // _ITERATOR_DEBUG_LEVEL >= 1
        }

        constexpr small_vector_const_iterator& operator+=(const difference_type _Off) noexcept
        {
            _Verify_offset(_Off);
            _ptr += _Off;
            return *this;
        }

        [[nodiscard]] constexpr small_vector_const_iterator operator+(const difference_type _Off) const noexcept
        {
            small_vector_const_iterator _Tmp{ *this };
            _Tmp += _Off;
            return _Tmp;
        }

        constexpr small_vector_const_iterator& operator-=(const difference_type _Off) noexcept
        {
            _Verify_offset(-_Off);
            _ptr -= _Off;
            return *this;
        }

        [[nodiscard]] constexpr small_vector_const_iterator operator-(const difference_type _Off) const noexcept
        {
            small_vector_const_iterator _Tmp{ *this };
            _Tmp -= _Off;
            return _Tmp;
        }

        [[nodiscard]] constexpr difference_type operator-(const small_vector_const_iterator& _Right) const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin == _Right._begin && _end == _Right._end, "cannot subtract incompatible iterators");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            return _ptr - _Right._ptr;
        }

        [[nodiscard]] constexpr reference operator[](const difference_type _Off) const noexcept
        {
            return *(*this + _Off);
        }

        [[nodiscard]] constexpr bool operator==(const small_vector_const_iterator& _Right) const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(_begin == _Right._begin && _end == _Right._end, "cannot compare incompatible iterators for equality");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            return _ptr == _Right._ptr;
        }

        [[nodiscard]] constexpr std::strong_ordering operator<=>(const small_vector_const_iterator& _Right) const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            _STL_VERIFY(
                _begin == _Right._begin && _end == _Right._end, "cannot compare incompatible iterators");
#endif // _ITERATOR_DEBUG_LEVEL >= 1
            return _ptr <=> _Right._ptr;
        }

        using _Prevent_inheriting_unwrap = small_vector_const_iterator;

        [[nodiscard]] constexpr pointer _Unwrapped() const noexcept
        {
            return _ptr;
        }

        static constexpr bool _Unwrap_when_unverified = _ITERATOR_DEBUG_LEVEL == 0;

        constexpr void _Seek_to(const pointer _It) noexcept
        {
            _ptr = _It;
        }

        pointer _ptr = nullptr;
#if _ITERATOR_DEBUG_LEVEL >= 1
        pointer _begin = nullptr;
        pointer _end = nullptr;
#endif // _ITERATOR_DEBUG_LEVEL >= 1
    };

    // This class was adopted from std::vector<>::iterator.
    template<class T>
    class small_vector_iterator : public small_vector_const_iterator<T>
    {
    public:
        using base = small_vector_const_iterator<T>;

        using iterator_concept = std::contiguous_iterator_tag;
        using iterator_category = std::random_access_iterator_tag;
        using value_type = T;
        using difference_type = ptrdiff_t;
        using pointer = T*;
        using reference = T&;

        using base::base;

        [[nodiscard]] constexpr reference operator*() const noexcept
        {
#pragma warning(suppress : 26492) // Don't use const_cast to cast away const or volatile (type.3).
            return const_cast<reference>(base::operator*());
        }

        [[nodiscard]] constexpr pointer operator->() const noexcept
        {
#pragma warning(suppress : 26492) // Don't use const_cast to cast away const or volatile (type.3).
            return const_cast<pointer>(base::operator->());
        }

        constexpr small_vector_iterator& operator++() noexcept
        {
            base::operator++();
            return *this;
        }

        constexpr small_vector_iterator operator++(int) noexcept
        {
            small_vector_iterator _Tmp = *this;
            base::operator++();
            return _Tmp;
        }

        constexpr small_vector_iterator& operator--() noexcept
        {
            base::operator--();
            return *this;
        }

        constexpr small_vector_iterator operator--(int) noexcept
        {
            small_vector_iterator _Tmp = *this;
            base::operator--();
            return _Tmp;
        }

        constexpr small_vector_iterator& operator+=(const difference_type _Off) noexcept
        {
            base::operator+=(_Off);
            return *this;
        }

        [[nodiscard]] constexpr small_vector_iterator operator+(const difference_type _Off) const noexcept
        {
            small_vector_iterator _Tmp = *this;
            _Tmp += _Off;
            return _Tmp;
        }

        constexpr small_vector_iterator& operator-=(const difference_type _Off) noexcept
        {
            base::operator-=(_Off);
            return *this;
        }

        using base::operator-;

        [[nodiscard]] constexpr small_vector_iterator operator-(const difference_type _Off) const noexcept
        {
            small_vector_iterator _Tmp = *this;
            _Tmp -= _Off;
            return _Tmp;
        }

        [[nodiscard]] constexpr reference operator[](const difference_type _Off) const noexcept
        {
            return const_cast<reference>(base::operator[](_Off));
        }

        using _Prevent_inheriting_unwrap = small_vector_iterator;

        [[nodiscard]] constexpr pointer _Unwrapped() const noexcept
        {
#pragma warning(suppress : 26492) // Don't use const_cast to cast away const or volatile (type.3).
            return const_cast<pointer>(this->_ptr);
        }
    };

    template<typename T, size_t N>
    class small_vector
    {
    public:
        static_assert(N != 0, "A small_vector without a small buffer isn't very useful");
        static_assert(std::is_nothrow_constructible_v<T>, "the copy/move operators don't guard against exceptions");
        static_assert(std::is_nothrow_move_assignable_v<T>, "_generic_insert doesn't guard against exceptions");
        static_assert(std::is_nothrow_move_constructible_v<T>, "_grow doesn't guard against exceptions");

        using value_type = T;
        using allocator_type = std::allocator<T>;
        using pointer = T*;
        using const_pointer = const T*;
        using reference = T&;
        using const_reference = const T&;
        using size_type = size_t;
        using difference_type = ptrdiff_t;

        using iterator = small_vector_iterator<T>;
        using const_iterator = small_vector_const_iterator<T>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        small_vector() noexcept :
            _data{ &_buffer[0] }
        {
        }

        explicit small_vector(size_type count, const T& value = T{}) :
            small_vector{}
        {
            insert(end(), count, value);
        }

        template<typename InputIt>
        small_vector(InputIt first, InputIt last) :
            small_vector{}
        {
            insert(end(), first, last);
        }

        small_vector(std::initializer_list<T> list) :
            small_vector{}
        {
            insert(end(), list.begin(), list.end());
        }

        small_vector(const small_vector& other) :
            small_vector{}
        {
            operator=(other);
        }

        small_vector& operator=(const small_vector& other)
        {
            auto data = _data;
            if (other._size > _capacity)
            {
                const auto new_cap = _calculate_new_capacity(other._size);
                data = _allocate(new_cap);
            }

            std::destroy(begin(), end());
            // The earlier static_assert(std::is_nothrow_constructible_v<T>)
            // ensures that we don't exit in a weird state and leak `data`.
            std::uninitialized_copy(other.begin(), other.end(), data);

            if (data != _data && _data != &_buffer[0])
            {
                _deallocate(_data);
            }

            _data = data;
            _size = other._size;
            return *this;
        }

        small_vector(small_vector&& other) noexcept :
            small_vector{}
        {
            operator=(std::move(other));
        }

        small_vector& operator=(small_vector&& other) noexcept
        {
            std::destroy(begin(), end());
            if (_capacity != N)
            {
                _deallocate(_data);
            }

            if (other._capacity == N)
            {
                _data = &_buffer[0];
                _capacity = N;
                _size = other._size;
                // The earlier static_assert(std::is_nothrow_constructible_v<T>)
                // ensures that we don't exit in a weird state with invalid `_size`.
#pragma warning(suppress : 26447) // The function is declared 'noexcept' but calls function '...' which may throw exceptions (f.6).
                std::uninitialized_move(other.begin(), other.end(), _uninitialized_begin());
                std::destroy(other.begin(), other.end());
            }
            else
            {
                _data = other._data;
                _capacity = other._capacity;
                _size = other._size;
            }

            other._data = &other._buffer[0];
            other._capacity = N;
            other._size = 0;

            return *this;
        }

        ~small_vector()
        {
            std::destroy(begin(), end());
            if (_capacity != N)
            {
                _deallocate(_data);
            }
        }

        constexpr size_type max_size() const noexcept { return static_cast<size_t>(-1) / sizeof(T); }

        constexpr pointer data() noexcept { return _data; }
        constexpr const_pointer data() const noexcept { return _data; }
        constexpr size_type capacity() const noexcept { return _capacity; }
        constexpr size_type size() const noexcept { return _size; }
        constexpr size_type empty() const noexcept { return _size == 0; }

        constexpr iterator begin() noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data, _data, _data + _size };
#else
            return { _data };
#endif
        }

        constexpr const_iterator begin() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data, _data, _data + _size };
#else
            return { _data };
#endif
        }

        constexpr const_iterator cbegin() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data, _data, _data + _size };
#else
            return { _data };
#endif
        }

        constexpr reverse_iterator rbegin() noexcept
        {
            return std::make_reverse_iterator(end());
        }

        constexpr const_reverse_iterator rbegin() const noexcept
        {
            return std::make_reverse_iterator(end());
        }

        constexpr const_reverse_iterator crbegin() const noexcept
        {
            return std::make_reverse_iterator(end());
        }

        constexpr iterator end() noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data + _size, _data, _data + _size };
#else
            return { _data + _size };
#endif
        }

        constexpr const_iterator end() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data + _size, _data, _data + _size };
#else
            return { _data + _size };
#endif
        }

        constexpr const_iterator cend() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data + _size, _data, _data + _size };
#else
            return { _data + _size };
#endif
        }

        constexpr reverse_iterator rend() noexcept
        {
            return std::make_reverse_iterator(begin());
        }

        constexpr const_reverse_iterator rend() const noexcept
        {
            return std::make_reverse_iterator(begin());
        }

        constexpr const_reverse_iterator crend() const noexcept
        {
            return std::make_reverse_iterator(begin());
        }

        bool operator==(const small_vector& other) const noexcept
        {
            return std::equal(begin(), end(), other.begin(), other.end());
        }

        reference front() noexcept
        {
            return operator[](0);
        }

        const_reference front() const noexcept
        {
            return operator[](0);
        }

        reference back() noexcept
        {
            return operator[](_size - 1);
        }

        const_reference back() const noexcept
        {
            return operator[](_size - 1);
        }

        reference operator[](size_type off) noexcept
        {
#if _CONTAINER_DEBUG_LEVEL > 0
            _STL_VERIFY(off < _size, "subscript out of range");
#endif // _CONTAINER_DEBUG_LEVEL > 0
            return _data[off];
        }

        const_reference operator[](size_type off) const noexcept
        {
#if _CONTAINER_DEBUG_LEVEL > 0
            _STL_VERIFY(off < _size, "subscript out of range");
#endif // _CONTAINER_DEBUG_LEVEL > 0
            return _data[off];
        }

        reference at(size_type off)
        {
            if (off >= _size)
            {
                _throw_invalid_subscript();
            }
            return _data[off];
        }

        const_reference at(size_type off) const
        {
            if (off >= _size)
            {
                _throw_invalid_subscript();
            }
            return _data[off];
        }

        void clear() noexcept
        {
            std::destroy(begin(), end());
            _size = 0;
        }

        void reserve(size_type capacity)
        {
            _ensure_capacity(capacity);
        }

        void resize(size_type new_size)
        {
            _generic_resize(new_size, [](auto&& beg, auto&& end) {
                std::uninitialized_value_construct(beg, end);
            });
        }

        void resize(size_type new_size, const_reference value)
        {
            _generic_resize(new_size, [&](auto&& beg, auto&& end) {
                std::uninitialized_fill(beg, end, value);
            });
        }

        void shrink_to_fit()
        {
            if (_capacity == N || _size == _capacity)
            {
                return;
            }

            auto data = &_buffer[0];
            auto capacity = N;
            if (_size > N)
            {
                data = _allocate(_size);
                capacity = _size;
            }

            std::uninitialized_move(begin(), end(), data);
            std::destroy(begin(), end());
            _deallocate(_data);

            _data = data;
            _capacity = capacity;
        }

        void push_back(const T& value)
        {
            const auto new_size = _ensure_capacity(_size + 1);
            new (_data + _size) T(value);
            _size = new_size;
        }

        void push_back(T&& value)
        {
            const auto new_size = _ensure_capacity(_size + 1);
            new (_data + _size) T(std::move(value));
            _size = new_size;
        }

        void pop_back()
        {
#if _ITERATOR_DEBUG_LEVEL == 2
            _STL_VERIFY(_size != 0, "empty before pop");
#endif // _ITERATOR_DEBUG_LEVEL == 2
            std::destroy_at(_data + _size - 1);
            _size--;
        }

        template<typename... Args>
        reference emplace_back(Args&&... args)
        {
            const auto new_size = _ensure_capacity(_size + 1);
            const auto it = new (_data + _size) T(std::forward<Args>(args)...);
            _size = new_size;
            return *it;
        }

        iterator insert(const_iterator pos, const T& value)
        {
            return _generic_insert(pos, 1, [&](auto&& it) {
                std::construct_at(&*it, value);
            });
        }

        iterator insert(const_iterator pos, T&& value)
        {
            return _generic_insert(pos, 1, [&](auto&& it) {
                std::construct_at(&*it, std::move(value));
            });
        }

        iterator insert(const_iterator pos, size_type count, const T& value)
        {
            return _generic_insert(pos, count, [&](auto&& it) {
                std::uninitialized_fill_n(it, count, value);
            });
        }

        template<typename InputIt, std::enable_if_t<std::_Is_iterator_v<InputIt>, int> = 0>
        iterator insert(const_iterator pos, InputIt first, InputIt last)
        {
            return _generic_insert(pos, std::distance(first, last), [&](auto&& it) {
                std::uninitialized_copy(first, last, it);
            });
        }

        iterator insert(const_iterator pos, std::initializer_list<T> list)
        {
            return insert(pos, list.begin(), list.end());
        }

        void erase(const_iterator pos)
        {
            erase(pos, pos + 1);
        }

        void erase(const_iterator first, const_iterator last)
        {
            if (first >= last)
            {
                return;
            }

            const auto beg = begin();
            const auto off = first - cbegin();
            const auto it = beg + off;
            const auto end_old = end();
            const auto end_new = std::move<const_iterator, iterator>(last, end_old, it);
            std::destroy(end_new, end_old);
            _size = end_new - beg;
        }

    private:
        [[noreturn]] static void _throw_invalid_subscript()
        {
            throw std::out_of_range("invalid small_vector subscript");
        }

        [[noreturn]] static void _throw_too_long()
        {
            throw std::length_error("small_vector too long");
        }

        static T* _allocate(size_t size)
        {
            if constexpr (alignof(T) <= __STDCPP_DEFAULT_NEW_ALIGNMENT__)
            {
                return static_cast<T*>(::operator new(size * sizeof(T)));
            }
            else
            {
                return static_cast<T*>(::operator new(size * sizeof(T), static_cast<std::align_val_t>(alignof(T))));
            }
        }

        static void _deallocate(T* data) noexcept
        {
            if constexpr (alignof(T) <= __STDCPP_DEFAULT_NEW_ALIGNMENT__)
            {
                ::operator delete(data);
            }
            else
            {
                ::operator delete(data, static_cast<std::align_val_t>(alignof(T)));
            }
        }

        iterator _uninitialized_begin() const noexcept
        {
#if _ITERATOR_DEBUG_LEVEL >= 1
            return { _data, _data + _size, _data + _capacity };
#else
            return { _data };
#endif
        }

        size_type _calculate_new_capacity(size_type min_cap) const
        {
            const auto new_cap = std::max(min_cap, _capacity + _capacity / 2);
            // min_cap < _capacity indicates a overflow in the calling code.
            // new_cap > max_size() indicates that our multiplication below would overflow.
            if (min_cap < _capacity || new_cap > max_size())
            {
                _throw_too_long();
            }
            return new_cap;
        }

        size_type _ensure_capacity(size_type cap)
        {
            if (cap > _capacity) [[unlikely]]
            {
                _grow(cap);
            }
            return cap;
        }

        void _grow(size_type min_cap)
        {
            const auto new_cap = _calculate_new_capacity(min_cap);
            const auto data = _allocate(new_cap);

            // The earlier static_assert(std::is_nothrow_move_constructible_v<T>)
            // ensures that we don't leak `data` here since no exceptions will be thrown.
            std::uninitialized_move(begin(), end(), data);
            std::destroy(begin(), end());

            if (_capacity != N)
            {
                _deallocate(_data);
            }

            _data = data;
            _capacity = new_cap;
        }

        template<typename F>
        void _generic_resize(size_type new_size, F&& func)
        {
            if (new_size < _size)
            {
                std::destroy(begin() + new_size, end() - new_size);
            }
            else if (new_size > _size)
            {
                _ensure_capacity(new_size);
                func(_uninitialized_begin() + _size, _uninitialized_begin() + new_size);
            }

            _size = new_size;
        }

        template<typename F>
        iterator _generic_insert(const_iterator pos, size_type count, F&& func)
        {
            const auto offset = pos - cbegin();
            const auto moveable = _size - offset;
            const auto displacement = std::min(count, moveable);
            const auto new_size = _ensure_capacity(_size + count);

            // The earlier static_assert(std::is_nothrow_move_assignable_v<T>)
            // ensures that we don't exit in a weird state with invalid `_size`.

            // 1. We've resized the vector to fit an additional `count` items.
            //    Initialize the `count` items at the end by moving existing items over.
            std::uninitialized_move(end() - displacement, end(), _uninitialized_begin() + (_size + count - displacement));
            _size = new_size;

            // 2. Now that all `new_size` items are initialized we can move as many
            //    items towards the end as we need to make space for `count` items.
            const auto displacement_beg = begin() + offset;
            std::move_backward(displacement_beg, displacement_beg + (moveable - displacement), end() - displacement);

            // 3. Destroy the displaced existing items, so that we can use
            //    std::uninitialized_*/std::construct_* functions inside `func`.
            //    This isn't optimal, but better than nothing.
            std::destroy(displacement_beg, displacement_beg + displacement);

            func(displacement_beg);

            return displacement_beg;
        }

        T* _data;
        size_t _capacity = N;
        size_t _size = 0;
        T _buffer[N];
    };
}

#pragma warning(pop)
