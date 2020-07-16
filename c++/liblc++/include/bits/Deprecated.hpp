//
// Created by chorm on 2020-07-15.
//

#ifndef __LCCC_DEPRECATED_HPP
#define __LCCC_DEPRECATED_HPP

#if __cplusplus>=201103L
#define __LCCC_DEPRECATED_CXX11(msg) [[deprecated(msg)]]
#else
#define __LCCC_DEPRECATED_CXX11(msg)
#endif

#if __cplusplus>=201402L
#define __LCCC_DEPRECATED_CXX14(msg) [[deprecated(msg)]]
#else
#define __LCCC_DEPRECATED_CXX14(msg)
#endif

#if __cplusplus>=201703L
#define __LCCC_DEPRECATED_CXX17(msg) [[deprecated(msg)]]
#else
#define __LCCC_DEPRECATED_CXX17(msg)
#endif

#if __cplusplus>=202002L
#define __LCCC_DEPRECATED_CXX20(msg) [[deprecated(msg)]]
#else
#define __LCCC_DEPRECATED_CXX20(msg)
#endif

#endif //__LCCC_DEPRECATED_HPP
