//
// Created by chorm on 2020-04-24.
//

#ifndef LCCC_TYPETRAITS_HPP
#define LCCC_TYPETRAITS_HPP
#pragma LCCC detailheader <type_traits>
namespace std{
    template<typename _T> struct _Identity{
        typedef _T type;
    };
    template<bool _B,typename _T> struct _EnableIf{};
    template<typename T> struct _EnableIf<true,T>: _Identity<T>{};
}

#endif //LCCC_TYPETRAITS_HPP
