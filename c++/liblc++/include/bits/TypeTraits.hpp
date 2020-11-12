//
// Created by chorm on 2020-04-24.
//

#ifndef LCCC_TYPETRAITS_HPP
#define LCCC_TYPETRAITS_HPP

#include <bits/Info.hpp>

#pragma LCCC detailheader <type_traits>
namespace std{
    template<typename _T> struct _Identity{
        typedef _T type;
    };
    template<bool _B,typename _T> struct _EnableIf{};
    template<typename T> struct _EnableIf<true,T>: _Identity<T>{};

    template<typename _T,_T _B> struct _IntegralConstant{
        typedef _IntegralConstant<_T,_B> type;
        static __LCCC_CONSTEXPR __LCCC_CXX17_INLINE bool value = false;

        __LCCC_CONSTEXPR _IntegralConstant() __LCCC_NOEXCEPT =default;
    };

    template<template<typename...> class Tm, typename T> struct _IsSpecialization : _IntegralConstant<bool,false>{
        
    };
    template<template<typename...> class Tm,typename... TArgs> struct _IsSpecialization<Tm,Tm<TArgs...>> : _IntegralConstant<bool,false>{
        
    };

    template<typename T,typename... Args> struct _IsConstructible : _IntegralConstant<bool,::__lccc::__type_traits__::__is_constructible__<T,Args&&...>()>{};


}

#endif //LCCC_TYPETRAITS_HPP
