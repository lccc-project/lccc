#ifndef XLANG_LAYOUT_STRINGVIEW_HPP_2021_08_22_17_14_10
#define XLANG_LAYOUT_STRINGVIEW_HPP_2021_08_22_17_14_10

#include <xlang++/Properties.h>
#include <xlang++/layout/Primitives.hpp>

#include <type_traits>
#include <iterator>

namespace lccc{
    /**
     * @brief [layout.basic_string_view] Implementation of std::basic_string_view for layout purposes. 
     * 
     * basic_stirng_view contains two members of type const CharT*, though neither member is accessible.
     * The first member pointers to the beginning of the span, the second to the end.
     * basic_string_view is a Trivially Copyable, Standard-Layout type. 
     * 
     * This implementation is safe to use accross module bounderies.
     * @tparam CharT The type of the characters in the string_view. Shall be equality_comparable.
     */
    template <typename CharT>
    struct basic_string_view
    {
    private:
        const CharT *_begin;
        const CharT *_end;
    public:
        /**
         * @brief (1) Constructs an empty string_view, spanning no characters.
         * 
         * *Postconditions*:
         * - this->begin()==this->end()
         * - this->size()==0
         */
        constexpr basic_string_view() noexcept : _begin(), _end() {}

        
        /**
         * @brief (2) Constructs a string_view that spans a C String of CharT.
         * 
         * *Constraints*: CharT is not std::size_t
         * *Preconditions*:
         * - There must exist a value of std::size_t, n, such that [string,string+n] is a valid range, and string+n points to a value of type CharT that compares equal to sentinel
         * *Postconditions*:
         * - this->begin()==string
         * - Given n, the least value of type std::size_t such that string+n points to a value of type CharT that compares equal to sentinel:
         *      - this->size()==n
         *      - this->end()==string+n
         * 
         * @param string The C string to span over, terminated by sentinel (typically NUL)
         * @param sentinel The sentinel character (which defaults to a value-initialized value of CharT)
         */
        template<std::enable_if_t<!std::is_same_v<CharT,std::size_t>>* =nullptr> basic_string_view(const CharT *string,const CharT& sentinel=CharT()) noexcept : _begin{string} {
            while(*string++ != sentinel);
            _end = string;
        }

        /**
         * @brief (3) Deleted overload of (2). Passing nullptr to (2) would otherwise cause undefined behaviour unconditionally.
         * 
         */
        basic_string_view(std::nullptr_t,CharT)=delete;
        /**
         * @brief (4) Deleted Overload of (2). Passing nullptr to (2) would otherwise cause undefined behaviour unconditionally.
         */
        basic_string_view(std::nullptr_t)=delete;

        /**
         * @brief (5) Constructs a basic_string_view spaning the range [begin,end)
         * 
         * *Preconditions*:
         * - [begin,end) must be a valid range
         * *Postconditions*:
         * - this->begin()==begin
         * - this->size()==(end-begin)
         * - this->end()==end
         */
        constexpr basic_string_view(const CharT *begin, const CharT *end) noexcept : _begin(limit(begin, end - begin)), _end(end) {}
        /**
         * @brief (6) Constructs a basic_string_view spaning the range [begin,begin+len)
         * 
         * *Preconditions*:
         * - [begin,begin+len) must be a valid range
         * *Postconditions*:
         * - this->begin()==begin
         * - this->size()==len
         * - this->end()==begin+len
         */
        constexpr basic_string_view(const CharT *begin, std::size_t len) noexcept : _begin(limit(begin, len)), _end(begin + len) {}


        /**
         * @brief (8) Constructs a basic_string_view spanning [begin(s),end(s))
         * *Postconditions*:
         * - this->begin()==s.data()
         * - this->size()==s.size()
         * - this->end()==s.data()+s.size()
         */
        template <typename CharTraits, typename Allocator>
        basic_string_view(const std::basic_string<CharT, CharTraits, Allocator> &s) noexcept : _begin(s.data()), _end(s.data() + s.size()) {}

        /**
         * @brief (9) Constructs a basic_string_view spanning [begin(s),end(s))
         * *Postconditions*:
         * - this->begin()==s.data()
         * - this->size()==s.size()
         * - this->end()==s.data()+s.size()
         */
        template <typename CharTraits>
        constexpr basic_string_view(std::basic_string_view<CharT, CharTraits> sv) noexcept : _begin(limit(sv.data(), sv.size())), _end(sv.data() + sv.size()) {}

        /**
         * @brief (10) 
         */
        constexpr basic_string_view(const basic_string_view &) noexcept = default;
        constexpr basic_string_view(basic_string_view &&) noexcept = default;
        basic_string_view &operator=(const basic_string_view &) = default;
        basic_string_view &operator=(basic_string_view &&) noexcept = default;

        /**
         * The value_type
         */
        using value_type = CharT;
        using iterator = const CharT *;
        using reference = const CharT &;
        using const_iterator = iterator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        constexpr static inline size_type npos = static_cast<size_type>(-1);

        /**
         * @brief Returns an iterator pointing to the beginning of the data
         */
        [[nodiscard]] constexpr iterator begin() const noexcept
        {
            return _begin;
        }
        [[nodiscard]] constexpr iterator end() const noexcept
        {
            return _end;
        }
        [[nodiscard]] constexpr const_iterator cbegin() const noexcept
        {
            return _begin;
        }
        [[nodiscard]] constexpr const_iterator cend() const noexcept
        {
            return _end;
        }

        [[nodiscard]] constexpr size_type size() const noexcept
        {
            return _end - _begin;
        }

        [[nodiscard]] constexpr const value_type *data() const noexcept
        {
            return limit(_begin, _end - _begin);
        }

        template <typename CharTraits>
        constexpr operator std::basic_string_view<CharT, CharTraits>() const noexcept
        {
            return std::basic_string_view<CharT, CharTraits>(_begin, _end - _begin);
        }

        [[nodiscard]] constexpr reference operator[](difference_type dif) const noexcept
        {
            return _begin[dif];
        }

        constexpr basic_string_view subview(size_type begin, size_type end = npos) const noexcept
        {
            if (end == npos)
                return basic_string_view{this->_begin + begin, this->_end};
            else
                return basic_string_view{this->_begin + begin, this->_begin + end};
        }

        constexpr bool starts_with(CharT c) const noexcept{
            return this->size()>0&&*_begin==c;
        }

        constexpr bool starts_with(basic_string_view span)const noexcept{
            if(this->size()<span.size())
                return false;
            for(size_type s = 0;s<span.size();s++)
                if((*this)[s]!=span[s])
                    return false;
            return true;
        }

        constexpr bool ends_with(CharT c) const noexcept{
            return this->size()>0&&_end[-1]==c;
        }

        constexpr bool ends_with(basic_string_view span)const noexcept{
            if(this->size()<span.size())
                return false;
            for(difference_type s = -1;s>=-span.size();s--)
                if(this->end()[s]!=span.end()[s])
                    return false;
            return true;
        }

        constexpr size_type find(CharT c) const noexcept{
            for(size_type s = 0;s<this->size();s++)
                if((*this)[s]==c)
                    return s;

            return npos;
        }

        constexpr size_type rfind(CharT c) const noexcept{
            for(difference_type s = -1;s>=-this->size();s--)
                if(this->end()[s]==c)
                    return -s;
            
            return npos;
        }

        constexpr size_type find(basic_string_view span)const noexcept{
            if(this->size()<span.size())
                return npos;
            for(size_type s = 0;s<this->size()-span.size();s++)
                if(this->subview(s).starts_with(span))
                    return s;
            return npos;
        }

        constexpr size_type rfind(basic_string_view span)const noexcept{
            if(this->size()<span.size())
                return npos;
            for(size_type s = size()-1;s>=span.size();s--)
                if(this->subview(0,s).ends_with(span))
                    return s;
            return npos;
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
}

#endif /* XLANG_LAYOUT_STRINGVIEW_HPP_2021_08_22_17_14_10 */