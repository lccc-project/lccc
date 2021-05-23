/**
 * xlang++/Layout.h
 * This file is part of libxlang, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  libxlang is additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

#ifndef LCCC_LAYOUT_H
#define LCCC_LAYOUT_H

#include <cstring>
#include <cstddef>
#include <memory>
#include <string>
#include <type_traits>
#include <string_view>
#include <functional>
#include <tuple>
#include <utility>

#include <xlang++/Properties.h>

namespace lccc
{
    /**
     * @brief Implementation of std::basic_string_view for layout purposes. 
     * 
     * basic_stirng_view contains two members of type const CharT*, though niether member is accessible.
     * basic_string_view is a Trivially Copyable, Standard-Layout type. 
     * 
     * This implementation is safe to use accross module bounderies.
     * @tparam CharT The type of the characters in the string_view
     */
    template <typename CharT>
    struct basic_string_view
    {
    private:
        const CharT *_begin;
        const CharT *_end;

    public:
        basic_string_view() noexcept : _begin(), _end() {}
        basic_string_view(const CharT *string) noexcept : basic_string_view(std::basic_string_view<CharT>(string)) {}
        constexpr basic_string_view(const CharT *begin, const CharT *end) noexcept : _begin(limit(begin, end - begin)), _end(end) {}
        constexpr basic_string_view(const CharT *begin, std::size_t len) noexcept : _begin(limit(begin, len)), _end(begin + len) {}
        template <typename CharTraits, typename Allocator>
        basic_string_view(const std::basic_string<CharT, CharTraits, Allocator> &s) noexcept : _begin(s.data()), _end(s.data() + s.size()) {}

        template <typename CharTraits>
        constexpr basic_string_view(std::basic_string_view<CharT, CharTraits> sv) noexcept : _begin(limit(sv.data(), sv.size())), _end(sv.data() + sv.size()) {}

        basic_string_view(const basic_string_view &) noexcept = default;
        basic_string_view(basic_string_view &&) noexcept = default;
        basic_string_view &operator=(const basic_string_view &) = default;
        basic_string_view &operator=(basic_string_view &&) noexcept = default;

        using value_type = CharT;
        using iterator = const CharT *;
        using reference = const CharT &;
        using const_iterator = iterator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        constexpr static inline size_type npos = static_cast<size_type>(-1);

        [[nodiscard]] iterator begin() const noexcept
        {
            return _begin;
        }
        [[nodiscard]] iterator end() const noexcept
        {
            return _end;
        }
        [[nodiscard]] const_iterator cbegin() const noexcept
        {
            return _begin;
        }
        [[nodiscard]] const_iterator cend() const noexcept
        {
            return _end;
        }

        [[nodiscard]] size_type size() const noexcept
        {
            return _end - _begin;
        }

        [[nodiscard]] const value_type *data() const noexcept
        {
            return limit(_begin, _end - _begin);
        }

        template <typename CharTraits>
        operator std::basic_string_view<CharT, CharTraits>() const noexcept
        {
            return std::basic_string_view<CharT, CharTraits>(_begin, _end - _begin);
        }

        [[nodiscard]] reference operator[](difference_type dif) const noexcept
        {
            return _begin[dif];
        }

        basic_string_view subview(size_type begin, size_type end = npos) noexcept
        {
            if (end == npos)
                return basic_string_view{this->_begin + begin, this->_end};
            else
                return basic_string_view{this->_begin + begin, this->_begin + end};
        }
    };

    template <typename CharT>
    bool operator==(lccc::basic_string_view<CharT> sv1, lccc::basic_string_view<CharT> sv2) noexcept(noexcept(std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits>
    bool operator==(lccc::basic_string_view<CharT> sv1, std::basic_string_view<CharT, CharTraits> sv2) noexcept(noexcept(std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits>
    bool operator==(std::basic_string_view<CharT, CharTraits> sv2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::equal(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator==(lccc::basic_string_view<CharT> sv1, const std::basic_string<CharT, CharTraits, Allocator> &s2) noexcept(noexcept(std::equal(sv1.begin(), sv1.end(), s2.begin(), s2.end())))
    {
        return std::equal(sv1.begin(), sv1.end(), s2.begin(), s2.end());
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator==(const std::basic_string<CharT, CharTraits, Allocator> &s2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(std::equal(sv1.begin(), sv1.end(), s2.begin(), s2.end())))
    {
        return std::equal(sv1.begin(), sv1.end(), s2.begin(), s2.end());
    }

    template <typename CharT>
    bool operator!=(lccc::basic_string_view<CharT> sv1, lccc::basic_string_view<CharT> sv2) noexcept(noexcept(sv1 == sv2))
    {
        return !(sv1 == sv2);
    }

    template <typename CharT, typename CharTraits>
    bool operator!=(lccc::basic_string_view<CharT> sv1, std::basic_string_view<CharT, CharTraits> sv2) noexcept(noexcept(sv1 == sv2))
    {
        return !(sv1 == sv2);
    }

    template <typename CharT, typename CharTraits>
    bool operator!=(std::basic_string_view<CharT, CharTraits> sv2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(sv1 == sv2))
    {
        return !(sv1 == sv2);
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator!=(lccc::basic_string_view<CharT> sv1, const std::basic_string<CharT, CharTraits, Allocator> &sv2) noexcept(noexcept(sv1 == sv2))
    {
        return !(sv1 == sv2);
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator!=(std::basic_string_view<CharT, CharTraits> sv2, const std::basic_string<CharT, CharTraits, Allocator> &sv1) noexcept(noexcept(sv1 == sv2))
    {
        return !(sv1 == sv2);
    }

    template <typename CharT>
    bool operator<(lccc::basic_string_view<CharT> sv1, lccc::basic_string_view<CharT> sv2) noexcept(noexcept(std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits>
    bool operator<(lccc::basic_string_view<CharT> sv1, std::basic_string_view<CharT, CharTraits> sv2) noexcept(noexcept(std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits>
    bool operator<(std::basic_string_view<CharT, CharTraits> sv2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end())))
    {
        return std::lexicographical_compare(sv1.begin(), sv1.end(), sv2.begin(), sv2.end());
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator<(lccc::basic_string_view<CharT> sv1, const std::basic_string<CharT, CharTraits, Allocator> &s2) noexcept(noexcept(std::lexicographical_compare(sv1.begin(), sv1.end(), s2.begin(), s2.end())))
    {
        return std::lexicographical_compare(sv1.begin(), sv1.end(), s2.begin(), s2.end());
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator<(const std::basic_string<CharT, CharTraits, Allocator> &s2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(std::lexicographical_compare(sv1.begin(), sv1.end(), s2.begin(), s2.end())))
    {
        return std::lexicographical_compare(sv1.begin(), sv1.end(), s2.begin(), s2.end());
    }

    template <typename CharT, typename CharTraits>
    bool operator>(lccc::basic_string_view<CharT> sv1, std::basic_string_view<CharT, CharTraits> sv2) noexcept(noexcept(sv1 < sv2))
    {
        return sv2 < sv1;
    }

    template <typename CharT, typename CharTraits>
    bool operator>(std::basic_string_view<CharT, CharTraits> sv2, lccc::basic_string_view<CharT> sv1) noexcept(noexcept(sv1 < sv2))
    {
        return sv2 < sv1;
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator>(lccc::basic_string_view<CharT> sv1, const std::basic_string<CharT, CharTraits, Allocator> &sv2) noexcept(noexcept(sv1 < sv2))
    {
        return sv2 < sv1;
    }

    template <typename CharT, typename CharTraits, typename Allocator>
    bool operator>(std::basic_string_view<CharT, CharTraits> sv2, const std::basic_string<CharT, CharTraits, Allocator> &sv1) noexcept(noexcept(sv1 < sv2))
    {
        return sv2 < sv1;
    }

    using string_view = basic_string_view<char>;
    using wstring_view = basic_string_view<wchar_t>;
    using u16string_view = basic_string_view<char16_t>;
    using u32string_view = basic_string_view<char32_t>;

    constexpr string_view operator""_sv(const char *c, std::size_t sz)
    {
        return string_view{c, c + sz};
    }

    constexpr wstring_view operator""_sv(const wchar_t *c, std::size_t sz)
    {
        return wstring_view{c, c + sz};
    }

    constexpr u16string_view operator""_sv(const char16_t *c, std::size_t sz)
    {
        return u16string_view{c, c + sz};
    }

    constexpr u32string_view operator""_sv(const char32_t *c, std::size_t sz)
    {
        return u32string_view{c, c + sz};
    }

    struct bad_function_call
    {
    };

    template <typename Fn>
    struct function;

    /// A type Erased invocable object.
    /// Compatible with the layout presented in Rust RFC 2955, and by MIA.
    /// Contains one virtual member, auto call(Self*,Args&&...)->R
    template <typename R, typename... Args>
    struct function<R(Args...)>
    {
    private:
        struct vtable
        {
        public:
            std::size_t size;
            std::size_t align;
            void (*destroy)(void *);
            void (*dealloc)(void *);
            R(*call)
            (void *, Args...);
        };

        template <typename T>
        static const vtable *vtable_for()
        {
            static constexpr vtable vtbl = {
                sizeof(T),
                alignof(T),
                [](void *v) { static_cast<T *>(v)->~T(); },
                [](void *v) { ::operator delete(v); },
                [](void *v, Args... args) {
                    return std::invoke(*static_cast<T *>(v), std::forward<Args>(args)...);
                }};
            return &vtbl;
        }

        void *allocation;
        const vtable *vtbl;

    public:
        function() : allocation{}, vtbl{} {}
        function(std::nullptr_t) : function{} {}

        template <typename T, typename = std::enable_if_t<std::is_invocable_r_v<R, T, Args &&...>>>
        function(T &&t) : allocation{new T{t}}, vtbl{vtable_for<T>()} {}

        function(function &&f) noexcept : allocation{std::exchange(f.allocation, nullptr)}, vtbl{f.vtbl} {}

        function(const function &) = delete;

        void swap(function &f)
        {
            using std::swap;
            swap(allocation, f.allocation);
            swap(vtbl, f.vtbl);
        }

        friend void swap(function &f1, function &f2)
        {
            f1.swap(f2);
        }

        function &operator=(function &&f) noexcept
        {
            this->swap(f);
            return *this;
        }

        ~function()
        {
            if (allocation)
            {
                // invariant: if allocation is non-null, vtbl is as well.
                this->vtbl->destroy(allocation);
                this->vtbl->dealloc(allocation);
            }
        }

        R operator()(Args... args)
        {
            if (!allocation)
                throw bad_function_call{}; // Shut up clang-tidy, I can't use stdlib types that cross module bounderies
            return this->vtbl->call(allocation, std::forward<Args>(args)...);
        }
    };

    constexpr inline std::ptrdiff_t dynamic_extent{-1};

    template <typename T>
    struct type_identity
    {
        using type = T;
    };

    template <typename T>
    using type_identity_t = typename type_identity<T>::type;

    template <typename ElementT, std::ptrdiff_t = dynamic_extent>
    struct span;

    namespace _detail
    {
        template <typename T>
        struct is_array_or_span_specialization : std::false_type
        {
        };
        template <typename T, std::size_t N>
        struct is_array_or_span_specialization<std::array<T, N>> : std::true_type
        {
        };
        template <typename T, std::ptrdiff_t N>
        struct is_array_or_span_specialization<span<T, N>> : std::true_type
        {
        };

        template <typename T>
        struct remove_cvref : std::remove_cv<std::remove_reference_t<T>>
        {
        };

        template <typename T>
        using remove_cvref_t = typename remove_cvref<T>::type;

        namespace adl
        {
            namespace _
            {
                using std::begin;
                using std::data;
                using std::end;
                using std::size;
                using std::swap;

                constexpr auto size_ = [](auto &&a) -> decltype(size(std::forward<decltype(a)>(a))) { return size(a); };
                constexpr auto data_ = [](auto &&a) -> decltype(data(std::forward<decltype(a)>(a))) { return data(a); };
                constexpr auto begin_ = [](auto &&a) -> decltype(begin(std::forward<decltype(a)>(a))) { return begin(a); };
                constexpr auto end_ = [](auto &&a) -> decltype(end(std::forward<decltype(a)>(a))) { return end(a); };
                constexpr auto swap_ = [](auto &a, auto &b) -> decltype(swap(a, b)) { return swap(a, b); };
            } // namespace _
            constexpr auto size = _::size_;
            constexpr auto data = _::data_;
            constexpr auto begin = _::begin_;
            constexpr auto end = _::end_;
            constexpr auto swap = _::swap_;
        } // namespace adl
    }     // namespace _detail

    template <typename ElementT, std::ptrdiff_t Extent>
    struct span
    {
    private:
        static_assert(Extent >= 0, "extent must be positive for primary template");
        ElementT *_m_begin;

    public:
        using element_type = ElementT;
        using value_type = std::remove_cv_t<element_type>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using pointer = element_type *;
        using const_pointer = const element_type *;
        using reference = element_type &;
        using const_reference = const element_type &;
        using iterator = element_type *;
        using reverse_iterator = std::reverse_iterator<iterator>;
        static constexpr size_type extent{Extent};

        template <typename = std::enable_if_t<Extent == 0>>
        constexpr span() : _m_begin(nullptr) {}

        template <typename Iter,
                  typename = std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<Iter>::iterator_category>>>
        constexpr explicit span(Iter begin, std::size_t size) : _m_begin(&*begin) {}

        template <typename Iter,
                  typename = std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<Iter>::iterator_category>>>
        constexpr explicit span(Iter begin, Iter end) : _m_begin(&*begin) {}

        constexpr span(type_identity_t<element_type> (&arr)[Extent]) : _m_begin(limit(arr, Extent)) {}

        template <typename T, typename = std::enable_if_t<
                                  std::is_convertible_v<T (*)[], element_type (*)[]>>>
        constexpr span(std::array<T, Extent> &arr) : _m_begin(std::data(arr)) {}
        template <typename T, typename = std::enable_if_t<
                                  std::is_convertible_v<const T (*)[], element_type (*)[]>>>
        constexpr span(const std::array<T, Extent> &arr) : _m_begin(std::data(arr)) {}

        template <typename Container, typename = std::enable_if_t<
                                          std::conjunction_v<
                                              std::negation<_detail::is_array_or_span_specialization<_detail::remove_cvref_t<Container>>>,
                                              std::negation<std::is_array<_detail::remove_cvref_t<Container>>>,
                                              std::is_convertible<std::remove_pointer_t<decltype(_detail::adl::data(std::declval<Container>()))> (*)[],
                                                                  element_type (*)[]>,
                                              std::disjunction<std::is_const<element_type>, std::is_lvalue_reference<Container>>>>>
        explicit constexpr span(Container &&a) : _m_begin(limit(_detail::adl::data(std::forward<Container>(a)), Extent)) {}

        constexpr span(const span &) noexcept = default;

        template <typename OtherElementType,
                  typename = std::enable_if_t<
                      std::is_convertible_v<OtherElementType (*)[], element_type (*)[]>>>
        constexpr span(const span<OtherElementType, Extent> &s) : _m_begin(s._m_begin) {}
        template <typename OtherElementType,
                  typename = std::enable_if_t<
                      std::is_convertible_v<OtherElementType (*)[], element_type (*)[]>>>
        constexpr explicit span(const span<OtherElementType, dynamic_extent> &s) : _m_begin(limit(s._m_begin, extent)) {}
        [[nodiscard]] constexpr pointer data() const
        {
            return _m_begin;
        }

        [[nodiscard]] constexpr size_type size() const
        {
            return Extent;
        }

        constexpr size_type size_bytes() const
        {
            return Extent * sizeof(ElementT);
        }

        constexpr iterator begin() const
        {
            return _m_begin;
        }
        constexpr iterator end() const
        {
            return _m_begin + extent;
        }

        constexpr reverse_iterator rbegin() const
        {
            return reverse_iterator{end()};
        }

        constexpr reverse_iterator rend() const
        {
            return reverse_iterator{begin()};
        }
    };

    template <typename ElementT>
    struct span<ElementT, dynamic_extent>
    {
    private:
        ElementT *_m_ptr;
        std::size_t _m_size;

    public:
        using element_type = ElementT;
        using value_type = std::remove_cv_t<element_type>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using pointer = element_type *;
        using const_pointer = const element_type *;
        using reference = element_type &;
        using const_reference = const element_type &;
        using iterator = element_type *;
        using reverse_iterator = std::reverse_iterator<iterator>;

        constexpr span() : _m_ptr{}, _m_size{} {}

        template <typename Iter,
                  typename = std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<Iter>::iterator_category>>>
        constexpr span(Iter begin, std::size_t size) : _m_ptr(&*begin), _m_size(size) {}

        template <typename Iter, std::size_t N,
                  typename = std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag, typename std::iterator_traits<Iter>::iterator_category>>>
        constexpr span(Iter begin, Iter end) : _m_ptr(&*begin), _m_size(end - begin) {}

        template <std::size_t N>
        constexpr span(type_identity_t<element_type> (&arr)[N]) : _m_ptr{arr}, _m_size{N} {}

        template <typename T, std::size_t N,
                  typename = std::enable_if_t<std::is_convertible_v<T (*)[], element_type (*)[]>>>
        constexpr span(std::array<T, N> &arr) : _m_ptr{data(arr)}, _m_size(N) {}

        template <typename T, std::size_t N,
                  typename = std::enable_if_t<std::is_convertible_v<const T (*)[], element_type (*)[]>>>
        constexpr span(const std::array<T, N> &arr) : _m_ptr{data(arr)}, _m_size(N) {}

        template <typename Container, typename = std::enable_if_t<std::is_convertible_v<std::remove_pointer_t<decltype(_detail::adl::data(std::declval<Container>()))> (*)[], element_type (*)[]>>,
                  typename = decltype(_detail::adl::size(std::declval<Container>()))>
        constexpr span(Container &&c) : _m_ptr{_detail::adl::data(c)}, _m_size(_detail::adl::size(c)) {}

        template <typename OtherElementType, std::ptrdiff_t OtherExtent,
                  typename = std::enable_if_t<std::is_convertible_v<OtherElementType (*)[], element_type (*)[]>>>
        constexpr span(const span<OtherElementType, OtherExtent> &s) : _m_ptr(s.data()), _m_size(s.size()) {}

        constexpr span(const span &s) noexcept = default;

        constexpr std::size_t size() const
        {
            return _m_size;
        }

        constexpr std::size_t size_bytes() const
        {
            return _m_size * sizeof(element_type);
        }

        constexpr element_type *data() const
        {
            return _m_ptr;
        }

        constexpr iterator begin() const noexcept
        {
            return _m_ptr;
        }

        constexpr friend span::iterator begin(const span& s) noexcept{
            return s.begin();
        }
        constexpr iterator end() const noexcept
        {
            return _m_ptr + _m_size;
        }

        constexpr friend span::iterator end(const span& s) noexcept{
            return s.end();
        }

        constexpr reverse_iterator rbegin() const noexcept
        {
            return reverse_iterator{end()};
        }

        constexpr friend span::reverse_iterator rbegin(const span& s) noexcept{
            return s.rbegin();
        }

        constexpr reverse_iterator rend() const noexcept
        {
            return reverse_iterator{begin()};
        }

        constexpr friend span::reverse_iterator rend(const span& s) noexcept{
            return s.rend();
        }
    };

    template <typename T, std::ptrdiff_t N>
    span<const std::byte, N == dynamic_extent ? dynamic_extent : N * sizeof(T)> as_bytes(const span<T, N> &s)
    {
        return span{reinterpret_cast<const std::byte *>(s.data()), s.size_bytes()};
    }

    template <typename T, std::ptrdiff_t N, typename = std::enable_if_t<!std::is_const_v<T>>>
    span<std::byte, N == dynamic_extent ? dynamic_extent : N * sizeof(T)> as_writable_bytes(const span<T, N> &s)
    {
        return span{reinterpret_cast<std::byte *>(s.data()), s.size_bytes()};
    }

    template <typename T>
    struct unique_ptr
    {
    private:
        T *_m_ptr;
        void (*_m_delete)(void *);
        static void do_delete(void *v)
        {
            delete (T *)v;
        }

    public:
        unique_ptr(T *ptr, void (*deleter)(void *) = do_delete) : _m_ptr{ptr}, _m_delete{deleter} {}
        unique_ptr() : _m_ptr{}, _m_delete{do_delete} {}
        unique_ptr(unique_ptr &&u) noexcept : _m_ptr{std::exchange(u._m_ptr, nullptr)}, _m_delete{u._m_delete} {}

        template <typename U, typename = std::enable_if_t<std::is_convertible_v<U *, T *>>>
        unique_ptr(unique_ptr<U> &&ptr) : _m_ptr{ptr.release()}, _m_delete(do_delete) {}
        unique_ptr(const unique_ptr &) = delete;
        ~unique_ptr()
        {
            if (_m_ptr)
                _m_delete(_m_ptr);
        }

        void swap(unique_ptr &other) noexcept
        {
            std::swap(this->_m_ptr, other._m_ptr);
            std::swap(this->_m_delete, other._m_delete);
        }

        friend void swap(unique_ptr &a, unique_ptr &b) noexcept
        {
            a.swap(b);
        }

        unique_ptr &operator=(unique_ptr u) noexcept
        {
            swap(u);
            return *this;
        }

        T &operator*()
        {
            return *this->_m_ptr;
        }
        const T &operator*() const
        {
            return *this->_m_ptr;
        }

        T *operator->()
        {
            return this->_m_ptr;
        }

        const T *operator->() const
        {
            return this->_m_ptr;
        }

        void reset(T *n = nullptr)
        {
            if ((n = std::exchange(this->_m_ptr, n)))
                this->_m_delete(n);
        }

        T* release(){
            return std::exchange(this->_m_ptr,nullptr);
        }
        
        T* get(){
            return this->_m_ptr;
        }

        const T* get()const{
            return this->_m_ptr;
        }
    };

    template<typename T,typename... Args,typename=std::enable_if_t<std::is_constructible_v<T,Args&&...>>> lccc::unique_ptr<T> make_unique(Args&&... args){
        return {new T(std::forward<Args>(args)...)};
    }

    enum class Architecture : std::uint32_t
    {
        X86_64 = 0,
        IX86 = 1,
        W65816 = 2,
        ARM = 3,
        AARCH64 = 4,
        SUPERH = 5,
        MIPS = 6,
        POWERPC = 7,
        POWERPC64 = 8,
        AVR = 9,
        M68000 = 10,

        SPARC = 14,
        RISCV32 = 15,
        RISCV64 = 16,
        WASM32 = 17,
        WASM64 = 18,

        WDC65C816 = 19,
        M6502     = 20,
        M65C02    = 21,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class Vendor : std::uint32_t
    {
        PC = 0,
        APPLE = 1,
        SNES = 2,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class OperatingSystem : std::uint32_t
    {
        NONE = 0,
        LINUX = 1,
        WINDOWS = 2,
        MINGW32 = 3,
        MACOS = 4,
        IOS = 5,
        PHANTOM = 6,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class Environment : std::uint32_t
    {
        NONE = 0,
        GNU = 1,
        EABI = 2,
        MSVC = 4,
        MUSL = 5,
        LC = 6,
        PHANTOM_STD = 7,
        PHANTOM_KERNEL = 8,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class ObjectFormat : std::uint32_t {
        AOUT = 0,
        COFF = 1,
        ELF  = 2,
        PE   = 3,
        XCOFF= 4,
        XO65 = 5,

        UNKNOWN = static_cast<std::uint32_t>(-1) 
    };

    struct XLANG_API Target
    {
    private:
        lccc::string_view name;
        Architecture arch;
        Vendor vendor;
        OperatingSystem os;
        Environment env;

    public:
        explicit Target(lccc::string_view) noexcept;

        lccc::string_view getName() const noexcept;

        lccc::string_view getArchName() const noexcept;
        lccc::string_view getCanonicalArch() const noexcept;
        Architecture getArch() const noexcept;

        lccc::string_view getVendorName() const noexcept;
        lccc::string_view getCanonicalVendor() const noexcept;
        Vendor getVendor()const noexcept;

        lccc::string_view getOperatingSystemName() const noexcept;
        lccc::string_view getCanonicalOperatingSystem() const noexcept;
        OperatingSystem getOperatingSystem()const noexcept;

        lccc::string_view getEnvironmentName() const noexcept;
        lccc::string_view getCanonicalEnvironment() const noexcept;
        Environment getEnvironment()const noexcept;
    };

    template<typename T,typename U> struct pair{
        T first;
        U second;

        template<std::enable_if_t<std::is_default_constructible_v<T>&&std::is_default_constructible_v<U>>* =nullptr>
            constexpr pair()=default;

        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(T1&& t1,T2&& t2) : first(std::forward<T1>(t1)), second(std::forward<U1>(t2)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(T1&& t1,T2&& t2) : first(std::forward<T1>(t1)), second(std::forward<U1>(t2)){}
        
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(std::pair<T1,T2>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(std::pair<T1,T2>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const std::pair<T1,T2>& p) : first(p.first), second(p.second){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const std::pair<T1,T2>& p) : first(p.first), second(p.second){}

        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(std::tuple<T1,T2>&& p) : first(std::get<0>(std::move(p))), second(std::get<1>(std::move(p))){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(std::tuple<T1,T2>&& p) : first(std::get<0>(std::move(p))), second(std::get<1>(std::move(p))){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const std::tuple<T1,T2>& p) : first(std::get<0>(p)), second(std::get<1>(p)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const std::tuple<T1,T2>& p) : first(std::get<0>(p)), second(std::get<1>(p)){}
        
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(lccc::pair<T1,T2>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(lccc::pair<T1,T2>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const lccc::pair<T1,T2>& p) : first(p.first), second(p.second){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const lccc::pair<T1,T2>& p) : first(p.first), second(p.second){}

        template<typename T1,typename U1,std::enable_if_t<std::is_convertible_v<const T&,T1>&&std::is_convertible<const U&,U1>>* =nullptr>
            constexpr std::pair<T1,U1>() const noexcept{
                return std::pair<T1,U1>(p.first,p.second);
            }
        template<typename T1,typename U1,std::enable_if_t<!(std::is_convertible_v<const T&,T1>&&std::is_convertible<const U&,U1>)&&std::is_constructible_v<U1,const U&>&&std::is_constructible_v<T1,const T&>>* =nullptr>
            constexpr explicit std::pair<T1,U1>() const noexcept{
                return std::pair<T1,U1>(p.first,p.second);
            }
    };

    namespace _detail{

        template<typename KeyType,typename... Ts> struct variant_storage{
            std::aligned_union_t<1,lccc::pair<KeyType,Ts>...> _m_storage;

            KeyType discriminant() const noexcept{
                return *std::launder(reinterpret_cast<const KeyType*>(&this->_m_storage));
            }

            template<typename T> T& get_element_unchecked() noexcept{
                return std::launder(reinterpret_cast<lccc::pair<KeyType,T>*(&this->_m_storage))->second;
            }

            template<typename T> const T& get_element_unchecked() const noexcept{
                return std::launder(reinterpret_cast<const lccc::pair<KeyType,T>*(&this->_m_storage))->second;
            }
        };
        template<typename KeyType,typename T> void do_in_place_copy(void* to_storage,const void* from_storage){
            ::new(to_storage) lccc::pair<KeyType,T>(*std::launder(static_cast<const lccc::pair<KeyType,T>*>(from_storage)));
        }



        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is> void in_place_copy_sel(KeyType key,void* to_storage,const void* from_storage,std::index_sequence<I1,Is...> seq){
            if(static_cast<std::size_t>(key)==I1)
                do_in_place_copy<KeyType,T1,to_storage,from_storage);
            else{
                if constexpr(sizeof...(Is)==0)
                    std::terminate();
                else
                    in_place_copy_sel<KeyType,Ts...>(key,to_storage,from_storage,std::index_sequence<Is...>{});
            }    
        } 
        
        template<typename KeyType,typename... Ts> void in_place_copy(KeyType key,variant_storage<KeyType,Ts...>* to_storage,const variant_storage<KeyType,Ts...>* from_storage){
            in_place_copy_sel(key,&to_storage->_m_storage,from_storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,typename T> void do_in_place_move(void* to_storage,void* from_storage){
            ::new(to_storage) lccc::pair<KeyType,T>(std::move(*std::launder(static_cast<lccc::pair<KeyType,T>*>(from_storage))));
        }



        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is> void in_place_move_sel(KeyType key,void* to_storage,const void* from_storage,std::index_sequence<I1,Is...> seq){
            if(static_cast<std::size_t>(key)==I1)
                do_in_place_move<KeyType,T1,to_storage,from_storage);
            else{
                if constexpr(sizeof...(Is)==0)
                    std::terminate();
                else
                    in_place_move_sel<KeyType,Ts...>(key,to_storage,from_storage,std::index_sequence<Is...>{});
            }    
        } 
        
        template<typename KeyType,typename... Ts> void in_place_move(KeyType key,variant_storage<KeyType,Ts...>* to_storage,const variant_storage<KeyType,Ts...>* from_storage){
            in_place_move_sel(key,&to_storage->_m_storage,from_storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,typename T> void do_in_place_destroy(void* storage){
            std::launder(static_cast<lccc::pair<KeyType,T>*>(storage))->~lccc::pair<KeyType,T>();
        }

        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is>
            void in_place_destroy_sel(KeyType key,void* storage,std::index_sequence<I1,Is...>){
                if(static_cast<std::size_t>(key)==I1)
                    do_in_place_destroy<KeyType,T1>(storage);
                else{
                    if constexpr(sizeof...(Is)==0)
                        std::terminate();
                    else
                        in_place_destroy_sel<KeyType,Ts...>(key,storage,std::index_sequence<Is...>{});
                }
            }

        template<typename KeyType,typename... Ts> void in_place_destroy(KeyType key,variant_storage<KeyType,Ts...>* storage){
            in_place_destroy_sel(key,storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,bool trivial_destructor,typename... Ts> struct variant_destructor : variant_storage<KeyType,Ts...>{
            ~variant_destructor()=default;
        };

        template<typename KeyType,typename... Ts> struct variant_destructor<KeyType,false,Ts...> : variant_storage<KeyType,Ts...>{
            ~variant_destructor() noexcept((std::is_nothrow_destructible_v<Ts> && ...)){
                KeyType k = *std::launder(static_cast<KeyType*>(this));
                in_place_destroy(k,this);
            }
        };
        

        template<typename KeyType,bool has_default_ctor,typename... Ts>
            struct variant_default_ctor : variant_destructor<KeyStorage,std::conjunction_v<std::is_trivially_destructible<Ts>...>,Ts...>{
                variant_default_ctor() =delete;
            };
        template<typename KeyType,typename T1,typename... Ts> struct variant_default_ctor<KeyType,true,T1,Ts...> : variant_destructor<KeyStorage,std::conjunction_v<std::is_trivially_destructible<T1>,std::is_trivially_destructible<Ts>...>,T1,Ts...>{
            variant_default_ctor() noexcept{
                ::new(&this->_m_storage) lccc::pair<KeyType,T1>{}; // get zero-initialization
            }
        };

        template<typename KeyType,bool has_copy_ctor,bool trivial_copy_ctor,typename T1,typename... Ts>
            struct variant_copy_ctor : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                variant_copy_ctor(const variant_copy_ctor&)=delete;
                variant_copy_ctor& operator=(const variant_copy_ctor&)=delete;
            };
        template<typename KeyType,typename T1,typename... Ts>
            struct variant_copy_ctor<KeyType,true,true,T1,Ts...> : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                constexpr variant_copy_ctor(const variant_copy_ctor&)=default;
                contexpr variant_copy_ctor& operator=(const variant_copy_ctor&)=default;
            };
        
        template<typename KeyType,typename T1,typename... Ts>
            struct variant_copy_ctor<KeyType,true,false,T1,Ts...> : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                variant_copy_ctor(const variant_copy_ctor& c1) noexcept {
                    KeyType k = *static_cast<const KeyType*>(static_cast<const void*>(&c1));
                    in_place_copy(this,&c1);
                }
                variant_copy_ctor& operator=(const variant_copy_ctor& c1) noexcept{
                    this->~variant_copy_ctor();
                    return *::new(this) variant_copy_ctor(c1);
                }
            };

        template<typename KeyType,bool has_move_ctor,bool trivial_move_ctor,typename... Ts>
            struct variant_move_ctor : variant_copy_ctor<KeyType,(std::copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...> {
                variant_move_ctor(variant_move_ctor&&)=delete;
                variant_move_ctor& operator=(variant_move_ctor&&)=delete;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,true,Ts...> : variant_copy_ctor<KeyType,(std::copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                constexpr variant_move_ctor(variant_move_ctor&&)=default;
                constexpr variant_move_ctor& operator=(variant_move_ctor&&)=default;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,false,Ts...> : variant_copy_ctor<KeyType,(std::copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                variant_move_ctor(variant_move_ctor&& m1) noexcept{
                    KeyType k = m1.discriminant();
                    in_place_move(k,this,&m1);
                }
                variant_move_ctor& operator=(variant_move_ctor&& m1) noexcept{
                    this->~variant_move_ctor();
                    return *::new(this) variant_move_ctor(std::move(m1));
                }
            };
        
        template<typename KeyType,typename... Ts>
            struct variant_impl : variant_move_ctor<KeyType,(std::is_move_constructible_v<Ts> && ...),(std::is_trivially_move_constructible_v<Ts> && ...),Ts...>{};
    }

    template<typename KeyType,typename... Ts> struct variant;
}

namespace std{
    template<std::size_t I,typename KeyType,typename T1,typename... Ts> struct variant_alternative<I,lccc::variant<KeyType,T1,Ts...>> : variant_alternative<(I-1),lccc::variant<KeyType,Ts...>>{};
    template<typename KeyType,typename T1,typename... Ts> struct variant_alternative<0,lccc::variant<KeyType,T1,Ts...>>{
        using type = T1;
    };

    template<typename KeyType,typename... Ts> struct variant_size<lccc::variant<KeyType,Ts...>> : std::integral_constant<std::size_t,sizeof...(Ts)>{};
}

namespace lccc{

    template<typename KeyType,KeyType Key> struct in_place_key_t{};

    template<typename KeyType,KeyType Key> constexpr inline in_place_key_t in_place_key{};

    template<typename KeyType,typename... Ts> struct variant : private _detail::variant_impl<KeyType,Ts...>{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::variant must be an integral or enumeration type");
        static_assert((std::is_object_v<Ts>&&...),"Each type in Ts... shall be a complete object type");

        
    };
    // Empty variant has no members
    template<typename KeyType> struct variant<KeyType>{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::variant must be an integral or enumeration type");
        variant()=delete;
        variant(const variant& v){
            unreachable_unchecked();
        }

        variant(variant&& v){
            unreachable_unchecked();
        }

        variant& operator=(const variant& v){
            unreachable_unchecked();
            return (*this = v);
        }

        variant& operator=(variant&& v){
            unreachable_unchecked();
            return (*this = v);
        }

        template<typename T> operator T()const noexcept{
            unreachable_unchecked();
            return static_cast<T>(*this);
        }
    };
+

} // namespace lccc


namespace std{
    template<typename T,typename U> struct tuple_size<lccc::pair<T,U>> : std::integral_constant<std::size_t,2>{};

    template<typename T,typename U> struct tuple_element<0,lccc::pair<T,U>> {
        using type = T;
    };
    template<typename T,typename U> struct tuple_element<1,lccc::pair<T,U>>{
        using type = U;
    };
}

namespace lccc{
    template<std::size_t I,typename T,typename U> std::tuple_element<I,lccc::pair<T,U>>& get(lccc::pair<T,U>& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)")
        if constexpr(I==0)
            return p.first;
        else
            return p.second;
    }
    template<std::size_t I,typename T,typename U> const std::tuple_element<I,lccc::pair<T,U>>& get(const lccc::pair<T,U>& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)")
        if constexpr(I==0)
            return p.first;
        else
            return p.second;
    }
    template<std::size_t I,typename T,typename U> std::tuple_element<I,lccc::pair<T,U>>&& get(lccc::pair<T,U>&& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)")
        if constexpr(I==0)
            return std::forward<T>(p.first);
        else
            return std::forward<U>(p.second);
    }
    template<std::size_t I,typename T,typename U> const std::tuple_element<I,lccc::pair<T,U>>&& get(const lccc::pair<T,U>&& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)")
        if constexpr(I==0)
            return std::forward<const T>(p.first);
        else
            return std::forward<const U>(p.second);
    }
}

#endif //LCCC_LAYOUT_H
