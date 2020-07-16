//
// Created by chorm on 2020-07-15.
//

#ifndef __LCCC_CONSTEXPR_HPP
#define __LCCC_CONSTEXPR_HPP

#ifdef __cplusplus>=201103L
#define __LCCC_CXX11_CONSTEXPR constexpr
#define __LCCC_CONSTEXPR constexpr
#else
#define __LCCC_CXX11_CONSTEXPR
#define __LCCC_CONSTEXPR const
#endif

#ifdef __cplusplus>=201402L
#define __LCCC_CXX14_CONSTEXPR constexpr
#else
#define __LCCC_CXX14_CONSTEXPR
#endif

#ifdef __cplusplus>=201703L
#define __LCCC_CXX17_CONSTEXPR constexpr
#define __LCCC_CXX17_INLINE inline
#else
#define __LCCC_CXX17_CONSTEXPR
#define __LCCC_CXX17_INLINE
#endif

#ifdef __cplusplus>=202002L
#define __LCCC_CXX20_CONSTEXPR constexpr
#else
#define __LCCC_CXX20_CONSTEXPR
#endif

#endif //__LCCC_CONSTEXPR_HPP
