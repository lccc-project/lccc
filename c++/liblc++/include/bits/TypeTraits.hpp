//
// Created by chorm on 2020-04-24.
//

#ifndef LCCC_TYPETRAITS_HPP
#define LCCC_TYPETRAITS_HPP

#include <bits/Constexpr.hpp>

#pragma LCCC detailheader <type_traits>
namespace std{
    template<typename _T> struct _Identity{
        typedef _T type;
    };
    template<bool _B,typename _T> struct _EnableIf{};
    template<typename T> struct _EnableIf<true,T>: _Identity<T>{};


    template<template<typename...> class Tm, typename T> struct _IsSpecialization{
        static __LCCC_CONSTEXPR bool value = false;
    };
    template<template<typename...> class Tm,typename... TArgs> struct _IsSpecialization<Tm,Tm<TArgs...>>{
        static __LCCC_CONSTEXPR bool value = true;
    };


}

#endif //LCCC_TYPETRAITS_HPP
