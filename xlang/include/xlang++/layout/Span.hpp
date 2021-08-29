#ifndef XLANG_LAYOUT_SPAN_HPP_2021_08_22_19_39_38
#define XLANG_LAYOUT_SPAN_HPP_2021_08_22_19_39_38


#include <cstddef>
#include <iterator>
#include <array>
#include <xlang++/layout/TypeTraits.hpp>

namespace lccc{
    constexpr inline std::ptrdiff_t dynamic_extent{-1};

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
                                              std::negation<_detail::is_array_or_span_specialization<lccc::remove_cvref_t<Container>>>,
                                              std::negation<std::is_array<lccc::remove_cvref_t<Container>>>,
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
}

#endif /* XLANG_LAYOUT_SPAN_HPP_2021_08_22_19_39_38 */