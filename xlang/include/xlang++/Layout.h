//
// Created by chorm on 2020-10-19.
//

#ifndef LCCC_LAYOUT_H
#define LCCC_LAYOUT_H

#include <cstring>
#include <cstddef>
#include <memory>
#include <string>
#include <type_traits>
#include <string_view>
#include <functional>

#include <xlang++/Properties.h>


namespace lccc{
    /**
     * @brief Implementation of std::basic_string_view for layout purposes. 
     * 
     * basic_stirng_view contains two members of type const CharT, though niether member is accessible.
     * basic_string_view is a Trivially Copyable, Standard-Layout type. 
     * 
     * This implementation is safe to use accross module bounderies.
     * @tparam CharT The type of the characters in the string_view
     */
    template<typename CharT> struct basic_string_view{
    private:
        const CharT* _begin;
        const CharT* _end;
    public:
        basic_string_view() : _begin(), _end(){}
        basic_string_view(const CharT* string) : _begin(limit(string,std::strlen(string))), _end(string+std::strlen(string)){}
        basic_string_view(const CharT* begin,const CharT* end) : _begin(limit(begin,end-begin)), _end(end) {}
        template<typename CharTraits,typename Allocator>
            basic_string_view(const std::basic_string<CharT,CharTraits,Allocator>& s) : _begin(s.data()), _end(s.data()+s.size()){}

        template<typename CharTraits>
            basic_string_view(std::basic_string_view<CharT,CharTraits> sv) :
                _begin(sv.data()), _end(sv.data()+sv.size()){}

        basic_string_view(const basic_string_view&)=default;
        basic_string_view(basic_string_view&&) noexcept =default;
        basic_string_view& operator=(const basic_string_view&)=default;
        basic_string_view& operator=(basic_string_view&&) noexcept =default;


        using value_type = CharT;
        using iterator = const CharT*;
        using reference = const CharT&;
        using const_iterator = iterator;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;

        [[nodiscard]] iterator begin()const{
            return _begin;
        }
        [[nodiscard]] iterator end()const{
            return _end;
        }
        [[nodiscard]] const_iterator cbegin()const{
            return _begin;
        }
        [[nodiscard]] const_iterator cend()const{
            return _end;
        }

        [[nodiscard]] size_type size()const{
            return _end-_begin;
        }


        [[nodiscard]] const value_type* data()const{
            return limit(_begin,_end-_begin);
        }

        template<typename CharTraits> operator std::basic_string_view<CharT,CharTraits>()const{
            return std::basic_string_view(_begin,_end);
        }

        [[nodiscard]] const CharT& operator[](difference_type dif)const{
            return _begin[dif];
        }
    };

    using string_view = basic_string_view<char>;
    using wstring_view = basic_string_view<wchar_t>;
    using u16string_view = basic_string_view<char16_t>;
    using u32string_view = basic_string_view<char32_t>;

    struct bad_function_call{};

    template<typename Fn> struct function;

    /// A type Erased invocable object.
    /// Compatible with the layout presented in Rust RFC 2955, and by MIA.
    /// Contains one virtual member, call(Self*,Args&&...)->R
    template<typename R,typename... Args> struct function<R(Args...)>{
    private:
        struct vtable{
        public:
            std::size_t size;
            std::size_t align;
            void(*destroy)(void*);
            void(*dealloc)(void*);
            R(*call)(void*,Args...);
        };

        

        template<typename T> static const vtable* vtable_for(){
            static constexpr vtable vtbl = {
                sizeof(T),
                alignof(T),
                [](void* v){static_cast<T*>(v)->~T();},
                [](void* v){::operator delete(v);},
                [](void* v,Args... args){
                    return std::invoke(*static_cast<T*>(v),std::forward<Args>(args)...);
                }
            };
            return &vtbl;
        }
        
        void* allocation;
        const vtable* vtbl;

    public:

        function(): allocation{},vtbl{}{}
        function(std::nullptr_t) : function{}{}

        template<typename T, typename=std::enable_if_t<std::is_invocable_r_v<R,T,Args&&...>>>
             function(T&& t) : allocation{new T{t}} , vtbl {vtable_for<T>()}{}

        function(function&& f)  noexcept : allocation{std::exchange(f.allocation,nullptr)}, vtbl{f.vtbl}{}

        function(const function&)=delete;

        void swap(function& f){
            using std::swap;
            swap(allocation,f.allocation);
            swap(vtbl,f.vtbl);
        }

        friend void swap(function& f1,function& f2){
            f1.swap(f2);
        }

        function& operator=(function&& f) noexcept {
            this->swap(f);
            return *this;
        }

        ~function(){
            if(allocation){
                // invariant: if allocation is non-null, vtbl is as well.
                this->vtbl->destroy(allocation);
                this->vtbl->dealloc(allocation);
            }
        }


        R operator()(Args... args){
            if(!allocation)
                throw bad_function_call{}; // Shut up clang-tidy, I can't use stdlib types that cross module bounderies
            return this->vtbl->call(allocation,std::forward<Args>(args)...);
        }
    };

    constexpr inline std::ptrdiff_t dynamic_extent{-1};

    template<typename T> struct type_identity{
        using type = T;
    };

    template<typename T> using type_identity_t = typename type_identity<T>::type;

    template<typename ElementT,std::ptrdiff_t=dynamic_extent> struct span;

    namespace _detail{
        template<typename T> struct is_array_or_span_specialization : std::false_type{};
        template<typename T,std::size_t N> struct is_array_or_span_specialization<std::array<T,N>> : std::true_type{};
        template<typename T,std::ptrdiff_t N> struct is_array_or_span_specialization<span<T,N>> : std::true_type{};

        template<typename T> struct remove_cvref : std::remove_cv<std::remove_reference_t<T>>{};

        template<typename T> using remove_cvref_t = typename remove_cvref<T>::type;

        namespace adl{
            namespace _ {
                using std::size;
                using std::data;
                using std::begin;
                using std::end;
                using std::swap;

                constexpr auto size_ = [](auto &&a) -> decltype(size(std::forward<decltype(a)>(a))) { return size(a); };
                constexpr auto data_ = [](auto &&a) -> decltype(data(std::forward<decltype(a)>(a))) { return data(a); };
                constexpr auto begin_ = [](auto &&a) -> decltype(begin(std::forward<decltype(a)>(a))) { return begin(a); };
                constexpr auto end_ = [](auto &&a)  -> decltype(end(std::forward<decltype(a)>(a))) { return end(a); };
                constexpr auto swap_ = [](auto &a,auto &b)  -> decltype(swap(a,b)) {return swap(a,b);};
            }
            constexpr auto size = _::size_;
            constexpr auto data = _::data_;
            constexpr auto begin = _::begin_;
            constexpr auto end = _::end_;
            constexpr auto swap = _::swap_;
        }
    }

    template<typename ElementT,std::ptrdiff_t Extent> struct span{
    private:
        static_assert(Extent>=0,"extent must be positive for primary template");
        ElementT* _m_begin;
    public:
        using element_type = ElementT;
        using value_type = std::remove_cv_t<element_type>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using pointer = element_type*;
        using const_pointer = const element_type*;
        using reference = element_type&;
        using const_reference = const element_type&;
        using iterator = element_type*;
        using reverse_iterator = std::reverse_iterator<iterator>;
        static constexpr size_type extent{Extent};

        template<typename=std::enable_if_t<Extent==0>> constexpr span() : _m_begin(nullptr){}

        template<typename Iter,
            typename=std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,typename std::iterator_traits<Iter>::iterator_category>>>
             constexpr explicit span(Iter begin,std::size_t size) : _m_begin(&*begin){}

        template<typename Iter,
            typename=std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,typename std::iterator_traits<Iter>::iterator_category>>>
             constexpr explicit span(Iter begin,Iter end) : _m_begin(&*begin){}

        constexpr span(type_identity_t<element_type>(&arr)[Extent]) : _m_begin(limit(arr,Extent)){}

        template<typename T, typename=std::enable_if_t<
            std::is_convertible_v<T(*)[],element_type(*)[]>>>
            constexpr span(std::array<T,Extent>& arr) : _m_begin(std::data(arr)){}
        template<typename T, typename=std::enable_if_t<
                std::is_convertible_v<const T(*)[],element_type(*)[]>>>
        constexpr span(const std::array<T,Extent>& arr) : _m_begin(std::data(arr)){}

        template<typename Container,typename=std::enable_if_t<
                std::conjunction_v<
                    std::negation<_detail::is_array_or_span_specialization<_detail::remove_cvref_t<Container>>>,
                    std::negation<std::is_array<_detail::remove_cvref_t<Container>>>,
                    std::is_convertible<std::remove_pointer_t<decltype(_detail::adl::data(std::declval<Container>()))>(*)[],
                        element_type(*)[]>,
                    std::disjunction<std::is_const<element_type>,std::is_lvalue_reference<Container>>
                >>> explicit constexpr span(Container&& a) :
                    _m_begin(limit(_detail::adl::data(std::forward<Container>(a)),Extent)){}

        constexpr span(const span&) noexcept = default;

        template<typename OtherElementType,
                typename =std::enable_if_t<
                        std::is_convertible_v<OtherElementType(*)[],element_type(*)[]>
                >> constexpr span(const span<OtherElementType,Extent>& s) : _m_begin(s._m_begin){}
        template<typename OtherElementType,
                typename =std::enable_if_t<
                        std::is_convertible_v<OtherElementType(*)[],element_type(*)[]>
                >> constexpr explicit span(const span<OtherElementType,dynamic_extent>& s) : _m_begin(limit(s._m_begin,extent)){}
        [[nodiscard]] constexpr pointer data()const{
            return _m_begin;
        }

        [[nodiscard]] constexpr size_type size()const{
            return Extent;
        }

        constexpr size_type size_bytes()const{
            return Extent*sizeof(ElementT);
        }

        constexpr iterator begin()const{
            return _m_begin;
        }
        constexpr iterator end()const{
            return _m_begin+extent;
        }

        constexpr reverse_iterator rbegin()const{
            return reverse_iterator{end()};
        }

        constexpr reverse_iterator rend()const{
            return reverse_iterator{begin()};
        }
    };

    template<typename ElementT> struct span<ElementT,dynamic_extent>{
    private:
        ElementT* _m_ptr;
        std::size_t _m_size;
    public:
        using element_type = ElementT;
        using value_type = std::remove_cv_t<element_type>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using pointer = element_type*;
        using const_pointer = const element_type*;
        using reference = element_type&;
        using const_reference = const element_type&;
        using iterator = element_type*;
        using reverse_iterator = std::reverse_iterator<iterator>;

        constexpr span() : _m_ptr{}, _m_size{}{}

        template<typename Iter,
            typename=std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,typename std::iterator_traits<Iter>::iterator_category>>>
             constexpr span(Iter begin,std::size_t size) : _m_ptr(&*begin), _m_size(size){}

        template<typename Iter,std::size_t N,
            typename=std::enable_if_t<std::is_base_of_v<std::random_access_iterator_tag,typename std::iterator_traits<Iter>::iterator_category>>>
             constexpr span(Iter begin,Iter end) : _m_ptr(&*begin), _m_size(end-begin){}

        template<std::size_t N> constexpr span(type_identity_t<element_type>(&arr)[N]) : _m_ptr{arr}, _m_size{N}{}

        template<typename T,std::size_t N,
                typename = std::enable_if_t<std::is_convertible_v<T(*)[],element_type(*)[]>>>
                constexpr span(std::array<T,N>& arr) : _m_ptr{data(arr)}, _m_size(N){}

        template<typename T,std::size_t N,
                typename = std::enable_if_t<std::is_convertible_v<const T(*)[],element_type(*)[]>>>
        constexpr span(const std::array<T,N>& arr) : _m_ptr{data(arr)}, _m_size(N){}

        template<typename Container,typename = 
            std::enable_if_t<
            std::is_convertible_v<std::remove_pointer_t<decltype(_detail::adl::data(std::declval<Container>()))>(*)[],
                element_type(*)[]>>,
                typename = decltype(_detail::adl::size(std::declval<Container>()))>
                constexpr span(Container&& c) : _m_ptr{_detail::adl::data(c)}, _m_size(_detail::adl::size(c)){}

        template<typename OtherElementType,std::ptrdiff_t OtherExtent,
            typename = std::enable_if_t<std::is_convertible_v<OtherElementType(*)[],element_type(*)[]>>>
            constexpr span(const span<OtherElementType,OtherExtent>& s): _m_ptr(s.data()), _m_size(s.size()){}

        constexpr span(const span& s)noexcept =default;

        constexpr std::size_t size()const{
            return _m_size;
        }

        constexpr std::size_t size_bytes()const{
            return _m_size*sizeof(element_type);
        }

        constexpr element_type* data()const{
            return _m_ptr;
        }

        constexpr iterator begin()const{
            return _m_ptr;
        }
        constexpr iterator end()const{
            return _m_ptr+_m_size;
        }

        constexpr reverse_iterator rbegin()const{
            return reverse_iterator{end()};
        }

        constexpr reverse_iterator rend()const{
            return reverse_iterator{begin()};
        }

    };

    template<typename T, std::ptrdiff_t N>
        span<const std::byte,N==dynamic_extent?dynamic_extent:N*sizeof(T)> as_bytes(const span<T,N>& s){
            return span{reinterpret_cast<const std::byte*>(s.data()),s.size_bytes()};
        }

    template<typename T, std::ptrdiff_t N,typename=std::enable_if_t<!std::is_const_v<T>>>
        span<std::byte,N==dynamic_extent?dynamic_extent:N*sizeof(T)> as_writable_bytes(const span<T,N>& s){
            return span{reinterpret_cast<std::byte*>(s.data()),s.size_bytes()};
        }

}

#endif //LCCC_LAYOUT_H
