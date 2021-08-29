#ifndef XLANG_LAYOUT_STRING_HPP_2021_08_28_20_45_52
#define XLANG_LAYOUT_STRING_HPP_2021_08_28_20_45_52

#include <xlang++/layout/Allocator.hpp>
#include <xlang++/layout/Vector.hpp>
#include <xlang++/layout/StringView.hpp>

#include <cstring>

namespace lccc {
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
            std::swap(ncap,_m_capacity);
            std::swap(nbegin,_m_begin);
            std::allocator_traits<Alloc>::deallocate(_m_alloc,nbegin,ncap);
        }
    public:
        basic_string(const Alloc& alloc = Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc(alloc){
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            std::memset(lccc::to_address(_m_begin),0,_m_capacity);
        }

        ~basic_string(){
            if(_m_begin)
                std::allocator_traits<Alloc>::deallocate(_m_alloc,_m_begin,_m_capacity);
        }

        basic_string(const basic_string& s) : _m_begin{}, _m_len{}, _m_capacity{s._m_capacity}, _m_alloc(std::allocator_traits<Alloc>::select_on_container_copy_construction(s._m_alloc)){
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<s._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,&s._m_begin[_m_len]);
        }

        basic_string(basic_string&& s) noexcept : _m_begin(std::exchange(s._m_begin,nullptr)), _m_len{s._m_len}, _m_capacity{s._m_capacity}, _m_alloc{std::move(s._m_alloc)}{}

        template<typename BasicStringView,typename=std::enable_if_t<std::is_convertible_v<BasicStringView,lccc::basic_string_view<CharT>>>> 
            basic_string(BasicStringView&& s) : basic_string{} {
                lccc::basic_string_view<CharT> sv = std::forward<BasicStringView>(s);
                this->reserve(sv.size());
                for(_m_len=0;_m_len<sv.size();_m_len++)
                    std::allocator_traits<Alloc>::construct(_m_alloc,&s[_m_len]);
            }

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
                    std::allocator_traits<Alloc>::construct(_m_alloc,&s._m_begin[_m_len]);
            }
            return *this;
        }

        void reserve(size_type ncap) noexcept{
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
            std::allocator_traits<Alloc>::construct(_m_alloc,&_m_begin[_m_len++],c);         
            return *this;   
        }

        basic_string& operator+=(lccc::basic_string_view<CharT> c){
            if((_m_len+c.length()+1)>_m_capacity)
                reserve(_m_len+c.length()+1);
            for(size_type i = 0;i<c.length();i++,_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,&_m_begin[_m_len],c[i]);

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
            return basic_string_view<CharT>{this->begin(),this->end()};
        }

        template<typename CharTraits>
            operator std::basic_string_view<CharT,CharTraits>()const noexcept{
                return std::basic_string_view<CharT,CharTraits>{this->begin(),this->end()};
            }
    };

    using string = basic_string<char>;
    using wstring = basic_string<wchar_t>;
    using u16string = basic_string<char16_t>;
    using u32string = basic_string<char32_t>;
}

#endif 