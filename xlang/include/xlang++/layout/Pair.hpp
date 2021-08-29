#ifndef XLANG_PAIR_2021_08_22_19_44_24
#define XLANG_PAIR_2021_08_22_19_44_24

#include <tuple>
#include <xlang++/layout/TypeTraits.hpp>


namespace lccc{
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

}

#endif /* XLANG_PAIR_2021_08_22_19_44_24 */