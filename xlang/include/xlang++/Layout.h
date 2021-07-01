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
#include <variant>
#include <exception>
#include <optional>

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

    template<typename T,typename U> struct pair{
        T first;
        U second;

        constexpr pair()=default;

        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(T1&& t1,U1&& t2) : first(std::forward<T1>(t1)), second(std::forward<U1>(t2)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(T1&& t1,U1&& t2) : first(std::forward<T1>(t1)), second(std::forward<U1>(t2)){}
        
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(std::pair<T1,U1>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(std::pair<T1,U1>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const std::pair<T1,U1>& p) : first(p.first), second(p.second){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const std::pair<T1,U1>& p) : first(p.first), second(p.second){}

        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(std::tuple<T1,U1>&& p) : first(std::get<0>(std::move(p))), second(std::get<1>(std::move(p))){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(std::tuple<T1,U1>&& p) : first(std::get<0>(std::move(p))), second(std::get<1>(std::move(p))){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const std::tuple<T1,U1>& p) : first(std::get<0>(p)), second(std::get<1>(p)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const std::tuple<T1,U1>& p) : first(std::get<0>(p)), second(std::get<1>(p)){}
        
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>>* =nullptr>
            constexpr pair(lccc::pair<T1,U1>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<T1&&,T>&&std::is_convertible_v<U1&&,U>)&&std::is_constructible_v<T,T1&&>&&std::is_constructible_v<U,U1&&>>* =nullptr>
            constexpr explicit pair(lccc::pair<T1,U1>&& p) : first(std::forward<T1>(p.first)), second(std::forward<U1>(p.second)){}
        template<typename T1,typename U1,
            std::enable_if_t<std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>>* =nullptr>
            constexpr pair(const lccc::pair<T1,U1>& p) : first(p.first), second(p.second){}
        template<typename T1,typename U1,
            std::enable_if_t<!(std::is_convertible_v<const T1&,T>&&std::is_convertible_v<const U1&,U>)&&std::is_constructible_v<T,const T1&>&&std::is_constructible_v<U,const U1&>>* =nullptr>
            constexpr explicit pair(const lccc::pair<T1,U1>& p) : first(p.first), second(p.second){}

        template<typename T1,typename U1,std::enable_if_t<std::is_convertible_v<const T&,T1>&&std::is_convertible_v<const U&,U1>>* =nullptr>
            constexpr operator std::pair<T1,U1>() const noexcept{
                return std::pair<T1,U1>(this->first,this->second);
            }
        template<typename T1,typename U1,std::enable_if_t<!(std::is_convertible_v<const T&,T1>&&std::is_convertible_v<const U&,U1>)&&std::is_constructible_v<U1,const U&>&&std::is_constructible_v<T1,const T&>>* =nullptr>
            constexpr explicit operator std::pair<T1,U1>() const noexcept{
                return std::pair<T1,U1>(this->first,this->second);
            }
    };

    namespace _detail{

        template<typename KeyType,typename... Ts> struct variant_storage{
            std::aligned_union_t<1,lccc::pair<KeyType,Ts>...> _m_storage;

            KeyType discriminant() const noexcept{
                KeyType val = *std::launder(reinterpret_cast<const KeyType*>(&this->_m_storage));
                xlang_assert(static_cast<std::size_t>(val)<sizeof...(Ts),"The discriminant of a variant must be in range for the type list");
                return val;
            }

            template<typename T> T& get_element_unchecked() noexcept{
                return std::launder(reinterpret_cast<lccc::pair<KeyType,T>*>(&this->_m_storage))->second;
            }

            template<typename T> const T& get_element_unchecked() const noexcept{
                return std::launder(reinterpret_cast<const lccc::pair<KeyType,T>*>(&this->_m_storage))->second;
            }
        };
        template<typename KeyType,typename T> void do_in_place_copy(void* to_storage,const void* from_storage){
            ::new(to_storage) lccc::pair<KeyType,T>(*std::launder(static_cast<const lccc::pair<KeyType,T>*>(from_storage)));
        }



        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is> void in_place_copy_sel(KeyType key,void* to_storage,const void* from_storage,std::index_sequence<I1,Is...> seq){
            if(static_cast<std::size_t>(key)==I1)
                do_in_place_copy<KeyType,T1>(to_storage,from_storage);
            else{
                if constexpr(sizeof...(Is)==0)
                    xlang_assert(false,"Reached end of type list for copy construction");
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
                do_in_place_move<KeyType,T1>(to_storage,from_storage);
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
            std::launder(static_cast<lccc::pair<KeyType,T>*>(storage))->~pair<KeyType,T>();
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
            struct variant_default_ctor : variant_destructor<KeyType,std::conjunction_v<std::is_trivially_destructible<Ts>...>,Ts...>{
                variant_default_ctor() =delete;
            };
        template<typename KeyType,typename T1,typename... Ts> struct variant_default_ctor<KeyType,true,T1,Ts...> : variant_destructor<KeyType,std::conjunction_v<std::is_trivially_destructible<T1>,std::is_trivially_destructible<Ts>...>,T1,Ts...>{
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
                constexpr variant_copy_ctor& operator=(const variant_copy_ctor&)=default;
            };
        
        template<typename KeyType,typename T1,typename... Ts>
            struct variant_copy_ctor<KeyType,true,false,T1,Ts...> : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                variant_copy_ctor(const variant_copy_ctor& c1) noexcept {
                    KeyType k = *static_cast<const KeyType*>(static_cast<const void*>(&c1));
                    in_place_copy(this,&c1);
                }
                variant_copy_ctor& operator=(const variant_copy_ctor& c1) noexcept{
                    if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                        in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                    in_place_copy(this->discriminant(),this,&c1);
                    return *this;
                }
            };

        template<typename KeyType,bool has_move_ctor,bool trivial_move_ctor,typename... Ts>
            struct variant_move_ctor : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...> {
                variant_move_ctor(variant_move_ctor&&)=delete;
                variant_move_ctor& operator=(variant_move_ctor&&)=delete;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,true,Ts...> : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                constexpr variant_move_ctor(variant_move_ctor&&)=default;
                constexpr variant_move_ctor& operator=(variant_move_ctor&&)=default;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,false,Ts...> : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                variant_move_ctor(variant_move_ctor&& m1) noexcept{
                    KeyType k = m1.discriminant();
                    in_place_move(k,this,&m1);
                }
                variant_move_ctor& operator=(variant_move_ctor&& m1) noexcept{
                    if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                        in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                    in_place_move(this->discriminant(),this,&m1);
                    return *this;
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


    struct monostate{};

    template<typename KeyType,KeyType Key> struct in_place_key_t{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::in_place_key_t must be an integral or enumeration type");
    };

    template<auto Key> constexpr inline in_place_key_t<decltype(Key),Key> in_place_key{};

    /// A partial implementation of std::variant, with custom key types and a guaranteed layout
    /// 
    /// The layout of each variant member V_i, where 0<=i<sizeof...(Ts) is the struct{KeyType discrim;T_i}, 
    ///  where i is the ith corresponding type in Ts...
    /// The layout of `variant<KeyType,Ts...>` is the layout of the union{Vs... vs;} where Vs is the list of types described above.
    /// If KeyType is a fixed width integer type `std::uintN_t` or `std::intN_t`, or an enum type with an underlying type which is such a type,
    ///  this layout is equivalent to the rust declaration `#[repr(uN)] enum Variant{ Cs(Ts)...}`, where `Cs` is an pack of anonymous constructors for each value of KeyType
    ///  present for Ts... 
    /// If `sizeof...(Ts)-1` does not fit in KeyType, or it's underlying type if it is an enum type, the behaviour is undefined.
    /// The behaviour is undefined if a variant used with a value of KeyType that does not correspond with an element of Ts...
    template<typename KeyType,typename... Ts> struct variant : private _detail::variant_impl<KeyType,Ts...>{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::variant must be an integral or enumeration type");
        static_assert((std::is_object_v<Ts>&&...),"Each type in Ts... shall be a complete object type");

        using _detail::variant_impl<KeyType,Ts...>::variant_impl;

        template<KeyType Key,typename... Us,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>,
            typename = std::enable_if_t<std::is_constructible_v<Ty,Us&&...>>>
            explicit variant(in_place_key_t<KeyType,Key>,Us&&... args) noexcept {
                ::new(&this->_m_storage) lccc::pair<KeyType,Ty>(Key,Ty(std::forward<Us>(args)...));
            }

        template<KeyType Key,typename... Us,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>,
            typename = std::enable_if_t<std::is_constructible_v<Ty,Us&&...>>>
            void emplace(Us&&... args) noexcept {
                if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                    _detail::in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                ::new(this->_m_storage) lccc::pair<KeyType,Ty>(Key,Ty(std::forward<Us>(args)...));
            }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> Ty& get_unchecked()noexcept{
            xlang_assert(Key==this->discriminant(),"Failed get_unchecked, key!=discriminant");
            return this->template get_element_unchecked<Ty>();
        }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> const Ty& get_unchecked()const noexcept{
            xlang_assert(Key==this->discriminant(),"Failed get_unchecked, key!=discriminant");
            return this->template get_element_unchecked<Ty>();
        }
        
        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> Ty* get_pointer()noexcept{
            if(Key!=this->discriminant())
                return nullptr;
            else
                return &this->template get_element_unchecked<Ty>();
        }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> const Ty* get_pointer()const noexcept{
            if(Key!=this->discriminant())
                return nullptr;
            else
                return &this->template get_element_unchecked<Ty>();
        } 
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

    using empty = variant<std::size_t>;

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
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)");
        if constexpr(I==0)
            return p.first;
        else
            return p.second;
    }
    template<std::size_t I,typename T,typename U> const std::tuple_element<I,lccc::pair<T,U>>& get(const lccc::pair<T,U>& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)");
        if constexpr(I==0)
            return p.first;
        else
            return p.second;
    }
    template<std::size_t I,typename T,typename U> std::tuple_element<I,lccc::pair<T,U>>&& get(lccc::pair<T,U>&& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)");
        if constexpr(I==0)
            return std::forward<T>(p.first);
        else
            return std::forward<U>(p.second);
    }
    template<std::size_t I,typename T,typename U> const std::tuple_element<I,lccc::pair<T,U>>&& get(const lccc::pair<T,U>&& p)noexcept{
        static_assert(I<2,"I must be less than std::tuple_size<lccc::pair<T,U>>::value (=2)");
        if constexpr(I==0)
            return std::forward<const T>(p.first);
        else
            return std::forward<const U>(p.second);
    }

    extern"C"{
        void* xlang_allocate(std::size_t) noexcept;
        void* xlang_allocate_aligned(std::size_t,std::size_t) noexcept;
        void xlang_deallocate(void* v)noexcept;
        void xlang_deallocate_aligned(void* v,std::size_t)noexcept;
    }

    struct bad_alloc{};

    template<typename T> struct allocator{
        using value_type = T; 
        T* allocate(std::size_t n){
            T* t;
            if constexpr(alignof(T)<=alignof(std::max_align_t))
                t = static_cast<T*>(xlang_allocate(sizeof(T)*n));
            else
                t = static_cast<T*>(xlang_allocate_aligned(sizeof(T)*n,alignof(T)));
            if(t)
                return t;
            else
                throw lccc::bad_alloc{};
        }
        void deallocate(T* t,std::size_t) noexcept{
            if constexpr(alignof(T)<=alignof(std::max_align_t))
                xlang_deallocate(t);
            else
                xlang_deallocate_aligned(t,alignof(T));
        }
    };

    namespace _detail{
        struct terminate_on_unwind{
            int excepts;
            terminate_on_unwind() : excepts{std::uncaught_exceptions()}{}
            ~terminate_on_unwind(){
                if(std::uncaught_exceptions()!=excepts)
                    std::terminate();
            }
        };
    }

    template<typename T,typename Alloc=lccc::allocator<T>> struct vector{
    public:
        static_assert(std::is_same_v<T,typename Alloc::value_type>);
        using element_type = T;
        using value_type = T;
        using pointer = typename std::allocator_traits<Alloc>::pointer;
        using const_pointer = typename std::allocator_traits<Alloc>::const_pointer;
        using reference = T&;
        using const_reference = const T&;
        using size_type = typename std::allocator_traits<Alloc>::size_type;
        using difference_type = std::make_signed_t<size_type>;
        using iterator = pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    private:
        pointer _m_begin;
        size_type _m_len;
        size_type _m_capacity;
        Alloc _m_alloc;

        void reallocate(size_type ncap){
            pointer nbegin = std::allocator_traits<Alloc>::allocate(_m_alloc,ncap);
            for(size_t n = 0;n<_m_len;n++){
                try{
                    
                    std::allocator_traits<Alloc>::construct(_m_alloc,nbegin+n,std::move_if_noexcept(_m_begin[n]));
                }catch(...){
                    {
                        _detail::terminate_on_unwind _guard{};
                        for(;n>0;n--)
                            std::allocator_traits<Alloc>::destroy(_m_alloc,nbegin+n-1);
                    }
                    throw;
                }
            }
            _m_capacity = ncap;
            std::swap(nbegin,_m_begin);
            for(size_t n = _m_len;n>0;n--)
                std::allocator_traits<Alloc>::destroy(_m_alloc,nbegin[n-1]);
            std::allocator_traits<Alloc>::deallocate(_m_alloc,nbegin);
        }

    public:
        vector(const Alloc& alloc=Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc{alloc}{
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,16);
        }

        ~vector(){
            if(_m_begin){
                for(;_m_len>0;_m_len--)
                    std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len-1);
                std::allocator_traits<Alloc>::deallocate(_m_alloc,_m_begin);
            }
        }

        vector(const vector& v) : _m_alloc{std::allocator_traits<Alloc>::select_on_container_copy_construction(v.get_allocator())}, _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,v._m_begin[_m_len]);
        }

        vector(vector&& v) : _m_begin{std::exchange(v,nullptr)}, _m_len{v._m_len}, _m_capacity{v._m_capacity}, _m_alloc{std::move(v._m_alloc)}{}

        template<typename Alloc2> vector(const vector<T,Alloc2>& v,const Alloc& alloc) : _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity}, _m_alloc{alloc} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,v._m_begin[_m_len]);
        }

        template<typename Alloc2> vector(vector<T,Alloc2>&& v,const Alloc& alloc) : _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity}, _m_alloc{alloc} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::move_if_noexcept(v._m_begin[_m_len]));
        }

        template<typename InputIt,typename=std::enable_if_t<!std::is_integral_v<InputIt>>,typename=typename std::iterator_traits<InputIt>::iterator_category>
            explicit vector(InputIt i1,InputIt i2,const Alloc& alloc = Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc{alloc} {
                if constexpr(std::is_base_of_v<std::forward_iterator_tag,typename std::iterator_traits<InputIt>::iterator_category>){
                    _m_capacity = std::distance(i1,i2);
                    _m_capacity--;
                    _m_capacity |= _m_capacity>>1;
                    _m_capacity |= _m_capacity>>2;
                    _m_capacity |= _m_capacity>>4;
                    _m_capacity |= _m_capacity>>8;
                    _m_capacity |= _m_capacity>>16;
                    if constexpr(sizeof(size_type)>4)
                        _m_capacity |= _m_capacity>>32;
                    _m_capacity++;
                    _m_capacity=std::max(_m_capacity,16);
                    _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
                    for(;i1!=i2;i1++,_m_len++)
                        std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,*i1);
                }else{
                    _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
                    for(;i1!=i2;i1++)
                        push_back(*i1);
                }
            }

        explicit vector(std::initializer_list<T> il,const Alloc& alloc=Alloc()) : vector(begin(il),end(il),alloc){}

        explicit vector(size_type num,const T& t=T(),const Alloc& alloc=Alloc()) : _m_begin{}, _m_capacity{num}, _m_len{}, _m_alloc{alloc}{
            _m_capacity--;
            _m_capacity |= _m_capacity>>1;
            _m_capacity |= _m_capacity>>2;
            _m_capacity |= _m_capacity>>4;
            _m_capacity |= _m_capacity>>8;
            _m_capacity |= _m_capacity>>16;
            if constexpr(sizeof(size_type)>4)
                _m_capacity |= _m_capacity>>32;
            _m_capacity++;
            _m_capacity=std::max(_m_capacity,16);
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(;_m_len<num;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,t);
        }

        const Alloc& get_allocator() const noexcept{
            return _m_alloc;
        }

        vector& operator=(const vector& v) {
            using std::swap;
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value)
                _m_alloc = v._m_alloc;
            vector nvec(v,_m_alloc);
            swap(_m_begin,nvec._m_begin);
            swap(_m_capacity,nvec._m_capacity);
            swap(_m_len,nvec._m_len);
            return *this;
        }

        vector& operator=(vector&& v){
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value){
                if(_m_begin){
                    pointer pbegin = std::exchange(_m_begin,nullptr);
                    // If we throw, we would rather leak then double-destroy.
                    for(;_m_len>0;_m_len--)
                        std::allocator_traits<Alloc>::destroy(_m_alloc,pbegin+_m_len-1);
                    std::allocator_traits<Alloc>::deallocate(_m_alloc,pbegin);
                }
                _m_alloc = std::move(v._m_alloc);
                _m_begin = std::exchange(v._m_begin,nullptr);
                _m_capacity = v._m_capacity;
                _m_len = v._m_len;
            }else{
                using std::swap;
                vector nvec(std::move(v),_m_alloc);
                swap(_m_begin,nvec._m_begin);
                swap(_m_capacity,nvec._m_capacity);
                swap(_m_len,nvec._m_len);
                return *this;
            }
        }

        void swap(vector& rhs) noexcept{
            using std::swap;
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_swap::value)
                swap(_m_alloc,rhs._m_alloc);
            swap(_m_begin,rhs._m_begin);
            swap(_m_capacity,rhs._m_capacity);
            swap(_m_len,rhs._m_len);
        }

        friend void swap(vector& a,vector& b) noexcept{
            a.swap(b);
        }

        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
        void push_back(const T& t){
            if(_m_len==_m_capacity)
                reallocate(_m_capacity<<1);
            std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,t);
            _m_len++;
        }
        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
        void push_back(T&& t){
            if(_m_len==_m_capacity)
                reallocate(_m_capacity<<1);
            std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::move(t));
            _m_len++;
        }

        template<typename... Args,typename=std::enable_if_t<std::is_constructible_v<T,Args&&...>>>
            void emplace_back(Args&&... args){
                if(_m_len==_m_capacity)
                    reallocate(_m_capacity<<1);
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::forward<Args>(args)...);
                _m_len++;
            }

        void pop_back() noexcept{
            std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+(_m_len--));
        }

        void clear() noexcept{
            for(;_m_len>0;_m_len--)
                std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len-1);
        }

        iterator erase(const_iterator it){
            difference_type pos = it-_m_begin;
            _m_len--;
            for(size_type n = pos;n<_m_len;n++)
                _m_begin[n] = std::move(_m_begin[n+1]);
            std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len);
            return _m_begin+pos;
        }

        iterator erase(const_iterator begin,const_iterator end) {
            difference_type pos = begin-_m_begin;
            difference_type len = end-begin;
            _m_len-=len;
            for(size_type n = pos;n<_m_len;n++)
                _m_begin[n] = std::move(_m_begin[n+len]);
            for(size_type t = _m_len;t<_m_len+len;t++)
                return std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len);
            return _m_begin+pos;
        }

        bool empty() const noexcept{
            return _m_len;
        }

        reference back() noexcept{
            return _m_begin[_m_len-1];
        }
        reference front() noexcept{
            return _m_begin[0];
        }

        const_reference back() const noexcept{
            return _m_begin[_m_len-1];
        }
        const_reference front() const noexcept{
            return _m_begin[0];
        }


        pointer data() noexcept{
            return _m_begin;
        }

        friend vector<T,Alloc>::pointer data(vector<T,Alloc>& v) noexcept{
            return v.data();
        }

        const_pointer data()const noexcept{
            return _m_begin;
        }

        friend vector<T,Alloc>::pointer data(const vector<T,Alloc>& v) noexcept{
            return v.data();
        }

        size_type size() const noexcept{
            return _m_len;
        }

        friend vector<T,Alloc>::size_type size(const vector<T,Alloc>& v) noexcept{
            return v.size();
        }

        size_type capacity()const noexcept{
            return _m_capacity;
        }

        void reserve(size_type ncap) const noexcept{
            if(_m_capacity<ncap){
                ncap--;
                ncap |= ncap>>1;
                ncap |= ncap>>2;
                ncap |= ncap>>4;
                ncap |= ncap>>8;
                ncap |= ncap>>16;
                if constexpr(sizeof(size_type)>4)
                    ncap |= ncap>>32;
                ncap++;
                reallocate(ncap);
            }
        }

        void shrink_to_fit(){
            size_type ncap = _m_len;
            ncap--;
            ncap |= ncap>>1;
            ncap |= ncap>>2;
            ncap |= ncap>>4;
            ncap |= ncap>>8;
            ncap |= ncap>>16;
            if constexpr(sizeof(size_type)>4)
                ncap |= ncap>>32;
            ncap++;
            reallocate(std::max(ncap,16));
        }

        iterator begin()noexcept{
            return _m_begin;
        }

        const_iterator begin()const noexcept{
            return _m_begin;
        }

        iterator cbegin()const noexcept{
            return _m_begin;
        }

        iterator end()noexcept{
            return _m_begin+_m_len;
        }

        const_iterator end()const noexcept{
            return _m_begin+_m_len;
        }

        const_iterator cend()const noexcept{
            return _m_begin+_m_len;
        }

        friend typename vector<T,Alloc>::iterator begin(vector<T,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename vector<T,Alloc>::const_iterator begin(const vector<T,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename vector<T,Alloc>::const_iterator cbegin(const vector<T,Alloc>& v) noexcept{
            return v.cbegin();
        }

        friend typename vector<T,Alloc>::iterator end(vector<T,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename vector<T,Alloc>::const_iterator end(const vector<T,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename vector<T,Alloc>::const_iterator cend(const vector<T,Alloc>& v) noexcept{
            return v.cend();
        }

        reverse_iterator rbegin()noexcept{
            return reverse_iterator{end()};
        }

        const_reverse_iterator rbegin()const noexcept{
            return const_reverse_iterator{end()};
        }

        const_reverse_iterator crbegin()const noexcept{
            return const_reverse_iterator{cend()};
        }
        
        reverse_iterator rend()noexcept{
            return reverse_iterator{begin()};
        }

        const_reverse_iterator rend()const noexcept{
            return const_reverse_iterator{begin()};
        }

        const_reverse_iterator crend()const noexcept{
            return const_reverse_iterator{cbegin()};
        }

        friend typename vector<T,Alloc>::reverse_iterator rbegin(vector<T,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator rbegin(const vector<T,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator crbegin(const vector<T,Alloc>& v) noexcept{
            return v.crbegin();
        }

        friend typename vector<T,Alloc>::reverse_iterator rend(vector<T,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator rend(const vector<T,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator crend(const vector<T,Alloc>& v) noexcept{
            return v.crend();
        }

        reference operator[](size_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](size_type sz) const noexcept{
            return _m_begin[sz];
        }

        reference operator[](difference_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](difference_type sz) const noexcept{
            return _m_begin[sz];
        }
    };

    template<typename T,typename=decltype(std::pointer_traits<T>::to_address(std::declval<T>()))>
        auto* to_address(T val){
            return std::pointer_traits<T>::to_address(std::move(val));
        }
    
    template<typename T>
        auto* to_address(T val){
            return lccc::to_address(val.operator->());
        }

    template<typename T> T* to_address(T* val){
        static_assert(!std::is_function_v<T>);
        return val;
    }

    template<typename CharT,typename Alloc=lccc::allocator<CharT>> struct basic_string{
    public:
        static_assert(std::is_same_v<CharT,typename Alloc::value_type>);
        static_assert(std::is_trivially_default_constructible_v<CharT>);
        static_assert(std::is_trivially_copyable_v<CharT>);
        using element_type = CharT;
        using value_type = CharT;
        using pointer = typename std::allocator_traits<Alloc>::pointer;
        using const_pointer = typename std::allocator_traits<Alloc>::const_pointer;
        using reference = CharT&;
        using const_reference = const CharT&;
        using size_type = typename std::allocator_traits<Alloc>::size_type;
        using difference_type = std::make_signed_t<size_type>;
        using iterator = pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    private:
        pointer _m_begin;
        size_type _m_len;
        size_type _m_capacity;
        Alloc _m_alloc;

        void reallocate(size_type ncap){
            pointer nbegin = std::allocator_traits<Alloc>::allocate(_m_alloc,ncap);
            std::memset(lccc::to_address(nbegin),0,ncap);
            for(size_t n = 0;n<_m_len;n++)
                std::allocator_traits<Alloc>::construct(_m_alloc,nbegin+n,std::move(_m_begin[n]));
            _m_capacity = ncap;
            std::swap(nbegin,_m_begin);
            std::allocator_traits<Alloc>::deallocate(_m_alloc,nbegin);
        }
    public:
        basic_string(const Alloc& alloc = Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc(alloc){
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            std::memset(lccc::to_address(_m_begin),0,_m_capacity);
        }

        ~basic_string(){
            if(_m_begin)
                std::allocator_traits<Alloc>::deallocate(_m_alloc,_m_begin);
        }

        basic_string(const basic_string& s) : _m_begin{}, _m_len{}, _m_capacity{s._m_capacity}, _m_alloc(std::allocator_traits<Alloc>::select_on_container_copy_construction(s._m_alloc)){
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<s._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,s._m_begin[_m_len]);
        }

        basic_string(basic_string&& s) noexcept : _m_begin(std::exchange(s._m_begin,nullptr)), _m_len{s._m_len}, _m_capacity{s._m_capacity}, _m_alloc{std::move(s._m_alloc)}{}

        template<typename Alloc2> basic_string(const basic_string<CharT,Alloc>& s,const Alloc& alloc) : _m_begin{}, _m_len{}, _m_capacity{s._m_capacity}, _m_alloc(alloc){
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<s._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,s._m_begin[_m_len]);
        }

        basic_string& operator=(basic_string&& s) noexcept(std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value){
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value){
                std::allocator_traits<Alloc>::dealloc(_m_alloc,_m_begin);
                _m_alloc = std::move(s._m_alloc);
                _m_begin = std::exchange(s._m_begin);
                _m_len = std::exchange(s._m_len);
                _m_capacity = std::move(s._m_capacity);
            }else{
                _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,s._m_capacity);
                _m_capacity = s._m_capacity;
                std::memset(lccc::to_address(_m_begin),0,s._m_capacity);
                for(_m_len=0;_m_len<s._m_len;_m_len++)
                    std::allocator_traits<Alloc>::construct(_m_alloc,s._m_begin[_m_len]);
            }
            return *this;
        }

        void reserve(size_type ncap) const noexcept{
            if(_m_capacity<ncap){
                if(ncap&(-ncap)==ncap)
                    reallocate(ncap);
                else{
                    size_type n = 1;
                    while(ncap&(-ncap)!=ncap)
                        ncap/=2,n++;
                    ncap<<=n;
                    reallocate(ncap<<1);
                }
            }
        }

        void shrink_to_fit(){
            size_type ncap = _m_len+1;
            if(ncap&(-ncap)==ncap)
                reallocate(ncap);
            else{
                size_type n = 1;
                while(ncap&(-ncap)!=ncap)
                    ncap/=2,n++;
                ncap<<=n;
                reallocate(ncap);
            }
        }
        
        basic_string& operator+=(CharT c){
            if((_m_len+2)>_m_capacity)
                reserve(_m_len+2);
            std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin[_m_len++],c);         
            return *this;   
        }

        basic_string& operator+=(lccc::basic_string_view<CharT> c){
            if((_m_len+c.length()+1)>_m_capacity)
                reserve(_m_len+c.length()+1);
            for(size_type i = 0;i<c.length();i++,_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin[_m_len],c[i]);

            return *this;
        }

        friend basic_string<CharT,Alloc> operator+(basic_string<CharT,Alloc> s,lccc::basic_string_view<CharT> sv){
            s+=sv;
            return s;
        }

        friend basic_string<CharT,Alloc> operator+(basic_string<CharT,Alloc> s,CharT c){
            s+=c;
            return s;
        }

        pointer data() noexcept{
            return _m_begin;
        }

        friend basic_string<CharT,Alloc>::pointer data(basic_string<CharT,Alloc>& v) noexcept{
            return v.data();
        }

        const_pointer data()const noexcept{
            return _m_begin;
        }

        const_pointer c_str()const noexcept{
            return _m_begin;
        }

        friend basic_string<CharT,Alloc>::pointer data(const basic_string<CharT,Alloc>& v) noexcept{
            return v.data();
        }

        size_type size() const noexcept{
            return _m_len;
        }

        friend basic_string<CharT,Alloc>::size_type size(const basic_string<CharT,Alloc>& v) noexcept{
            return v.size();
        }

        size_type capacity()const noexcept{
            return _m_capacity;
        }


        iterator begin()noexcept{
            return _m_begin;
        }

        const_iterator begin()const noexcept{
            return _m_begin;
        }

        iterator cbegin()const noexcept{
            return _m_begin;
        }

        iterator end()noexcept{
            return _m_begin+_m_len;
        }

        const_iterator end()const noexcept{
            return _m_begin+_m_len;
        }

        const_iterator cend()const noexcept{
            return _m_begin+_m_len;
        }

        friend typename basic_string<CharT,Alloc>::iterator begin(basic_string<CharT,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename basic_string<CharT,Alloc>::const_iterator begin(const basic_string<CharT,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename basic_string<CharT,Alloc>::const_iterator cbegin(const basic_string<CharT,Alloc>& v) noexcept{
            return v.cbegin();
        }

        friend typename basic_string<CharT,Alloc>::iterator end(basic_string<CharT,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename basic_string<CharT,Alloc>::const_iterator end(const basic_string<CharT,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename basic_string<CharT,Alloc>::const_iterator cend(const basic_string<CharT,Alloc>& v) noexcept{
            return v.cend();
        }

        reverse_iterator rbegin()noexcept{
            return reverse_iterator{end()};
        }

        const_reverse_iterator rbegin()const noexcept{
            return const_reverse_iterator{end()};
        }

        const_reverse_iterator crbegin()const noexcept{
            return const_reverse_iterator{cend()};
        }
        
        reverse_iterator rend()noexcept{
            return reverse_iterator{begin()};
        }

        const_reverse_iterator rend()const noexcept{
            return const_reverse_iterator{begin()};
        }

        const_reverse_iterator crend()const noexcept{
            return const_reverse_iterator{cbegin()};
        }

        friend typename basic_string<CharT,Alloc>::reverse_iterator rbegin(basic_string<CharT,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename basic_string<CharT,Alloc>::const_reverse_iterator rbegin(const basic_string<CharT,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename basic_string<CharT,Alloc>::const_reverse_iterator crbegin(const basic_string<CharT,Alloc>& v) noexcept{
            return v.crbegin();
        }

        friend typename basic_string<CharT,Alloc>::reverse_iterator rend(basic_string<CharT,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename basic_string<CharT,Alloc>::const_reverse_iterator rend(const basic_string<CharT,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename basic_string<CharT,Alloc>::const_reverse_iterator crend(const basic_string<CharT,Alloc>& v) noexcept{
            return v.crend();
        }

        reference operator[](size_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](size_type sz) const noexcept{
            return _m_begin[sz];
        }

        reference operator[](difference_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](difference_type sz) const noexcept{
            return _m_begin[sz];
        }

        operator basic_string_view<CharT>()const noexcept{
            return basic_string_view<CharT>{begin(*this),end(*this)};
        }

        template<typename CharTraits>
            operator std::basic_string_view<CharT,CharTraits>()const noexcept{
                return std::basic_string_view<CharT,CharTraits>{begin(*this),end(*this)};
            }
    };

    using string = basic_string<char>;
    using wstring = basic_string<wchar_t>;
    using u16string = basic_string<char16_t>;
    using u32string = basic_string<char32_t>;

    namespace _detail{
        template<typename T> struct _option_storage{
            bool _engaged;
            alignas(T) unsigned char _m_storage[sizeof(T)];
            
            template<typename... Ts> void construct_from(Ts&&... ts){
                ::new(_m_storage) T(std::forward<Ts>(ts)...);
            }

            T* get_storage()noexcept{
                return reinterpret_cast<T*>(_m_storage);
            }

            const T* get_storage()const noexcept{
                return reinterpret_cast<const T*>(_m_storage);
            }
            // SAFETY: this->_m_storage shall provide storage for an object of type T for which the lifetime has begun, and not ended.
            void set() noexcept{
                
                this->_engaged = true;
            }

            bool engaged() const noexcept{
                return _engaged;
            }

            T& get_unchecked() noexcept{
                return *get_storage();
            }

            const T& get_unchecked() const noexcept{
                return *get_storage();
            }
            void clear(){
                if(std::exchange(_engaged,false))
                    reinterpret_cast<T*>(_m_storage)->~T();
            }
        };

        template<typename T,bool=std::is_trivially_destructible_v<T>> struct _option_destructor : _option_storage<T>{
            _option_destructor()noexcept=default;
            ~_option_destructor() noexcept(std::is_nothrow_destructible_v<T>){
                this->clear();
            }
        };

        template<typename T> struct _option_destructor<T,true> : _option_storage<T>{};


        template<typename T,bool=std::is_trivially_move_constructible_v<T>,bool=std::is_trivially_move_assignable_v<T>>
            struct _option_move : _option_destructor<T>{
                _option_move()noexcept=default;
                _option_move(_option_move&& m) {
                    if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }
                }

                _option_move& operator=(_option_move&& m) noexcept{
                    if(this->engaged()&&m.engaged())
                        this->get_unchecked() = std::move(m.get_unchecked());
                    else if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }else{
                        this->clear();
                    }
                    return *this;

                }
            };

        template<typename T> struct _option_move<T,true,false> : _option_destructor<T>{
            _option_move()noexcept=default;
            _option_move(_option_move&&) noexcept=default;

            _option_move& operator=(_option_move&& m) noexcept{
                    if(this->engaged()&&m.engaged())
                        this->get_unchecked() = std::move(m.get_unchecked());
                    else if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }else{
                        this->clear();
                    }
                    return *this;

                }
        };

        template<typename T> struct _option_move<T,true,true>: _option_destructor<T>{};

        template<typename T,bool has_copy_ctor=std::is_copy_constructible_v<T>,bool trivial_copy_ctor=std::is_trivially_copy_constructible_v<T>,bool is_copy_assignable=std::is_copy_assignable_v<T>,bool is_trivial_copy_assignment=std::is_trivially_copy_assignable_v<T>>
        struct _option_copy : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy&)=delete;
            _option_copy& operator=(const _option_copy&)=delete;
        };


        template<typename T> struct _option_copy<T,true,false,false,false> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy& o){
                if(o.engaged()){
                    ::new(this->get_storage()) T{o.get_unchecked()};
                    this->set();
                }
            }
            _option_copy& operator=(const _option_copy&)=delete;
        };

        template<typename T> struct _option_copy<T,true,true,false,false> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy&)=default;
            _option_copy& operator=(const _option_copy&)=delete;
        };

        template<typename T,bool trivially_copy_assignable> struct _option_copy<T,true,false,true,trivially_copy_assignable> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy& o){
                if(o.engaged()){
                    ::new(this->get_storage()) T{o.get_unchecked()};
                    this->set();
                }
            }

            _option_copy& operator=(_option_copy&& m) noexcept{
                if(this->engaged()&&m.engaged())
                    this->get_unchecked() = m.get_unchecked();
                else if(m.engaged()){
                    ::new(this->get_storage()) T{m.get_unchecked()};
                    this->set();
                    
                }else{
                    this->clear();
                }
                return *this;

            }
        };

        template<typename T> struct _option_copy<T,true,true,true,true> : _option_move<T>{};
    }

    struct nullopt_t{
        constexpr explicit nullopt_t()noexcept=default;
    };

    constexpr inline nullopt_t nullopt{};

    template<typename T> struct optional;

    template<typename T> struct _flatten_option {};
    template<typename T> struct _flatten_option<lccc::optional<T>> {
        using type = T;
    };

    template<typename T> struct _flatten_option<std::optional<T>> {
        using type = T;
    };


    template<typename T> struct optional: private _detail::_option_copy<T>{
    public:
        optional()noexcept=default;
        optional(const optional&)=default;
        optional(optional&&)noexcept=default;
        optional& operator=(const optional&)=default;
        optional& operator=(optional&&)noexcept=default;
        ~optional()noexcept=default;

        optional(const nullopt_t&) noexcept : optional{}{} 
        
        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
            optional(const T& t) {
                ::new(this->get_storage()) T{t};
                this->set();
            }
        
        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
            optional(T&& t) {
                ::new(this->get_storage()) T{std::move(t)};
                this->set();
            }

        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
            optional(const std::optional<T>& t) {
                if(t){
                    ::new(this->get_storage()) T{*t};
                    this->set();
                }
            }

        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
            optional(std::optional<T>&& t) {
                if(t){
                    ::new(this->get_storage()) T{*t};
                    this->set();
                }
            }



        template<typename F,std::enable_if_t<std::is_invocable_v<F&&,const T&>>* =nullptr>
            void if_present(F&& f){
                if(this->engaged())
                    std::invoke(std::forward<F>(f),this->get_unchecked());
            }

        using _detail::_option_storage<T>::get_unchecked;



        explicit operator bool()const{
            return this->engaged();
        }

        T& operator*(){
            return this->get_unchecked();
        }

        const T& operator*()const{
            return this->get_unchecked();
        }

        template<typename F> lccc::optional<std::invoke_result_t<F&&,const T&>>
            map(F&& f)  const{
                if(*this)
                    return {std::invoke(f,**this)};
                else
                    return nullopt;
            }

        template<typename F> lccc::optional<std::invoke_result_t<F&&,T&&>>
            map(F&& f) &&{
                if(*this)
                    return {std::invoke(f,std::move(**this))};
                else
                    return nullopt;
            }

        template<typename _T = typename _flatten_option<T>::type,std::enable_if_t<std::is_copy_constructible_v<_T>>* =nullptr>
            lccc::optional<_T> flatten() const{
                if(*this)
                    return **this;
                else
                    return nullopt;
            }

        template<typename _T = typename _flatten_option<T>::type,std::enable_if_t<std::is_move_constructible_v<_T>>* =nullptr>
            lccc::optional<_T> flatten() &&{
                if(*this)
                    return std::move(**this);
                else
                    return nullopt;
            }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&,const T&>>* =nullptr>
            lccc::optional<T> and_then(F&& f) const{
                if(*this)
                    return std::invoke(f,this->get_unchecked());
                else
                    return nullopt;
            }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&,T&&>>* =nullptr>
            lccc::optional<T> and_then(F&& f) &&{
                if(*this)
                    return std::invoke(f,std::move(this->get_unchecked()));
                else
                    return nullopt;
            }

       template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&>>* =nullptr,std::enable_if_t<std::is_copy_constructible_v<T>>* =nullptr>
             lccc::optional<T> or_else(F&& f) const{
                 if(*this)
                    return *this;
                else
                    return {std::invoke(f)};
             }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&>>* =nullptr,std::enable_if_t<std::is_move_constructible_v<T>>* =nullptr>
             lccc::optional<T> or_else(F&& f) &&{
                 if(*this)
                    return std::move(**this);
                else
                    return {std::invoke(f)};
             }


        template<std::enable_if_t<std::is_move_constructible_v<T>>* =nullptr>
            lccc::optional<T> take(){
                if(*this){
                    lccc::optional<T> opt{**this};
                    this->clear();
                    return opt;
                }else{
                    return nullopt;
                }
            }

        
    };
}

#endif //LCCC_LAYOUT_H
