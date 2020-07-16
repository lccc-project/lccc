//
// Created by chorm on 2020-07-15.
//

#ifndef __LCCC_XLANGATTRIBUTES_HPP
#define __LCCC_XLANGATTRIBUTES_HPP

#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#ifndef __cpp_attribute
#if __cplusplus<201103L
#define __cpp_attribute(ns,x) __attribute__(x)
#else
#define __cpp_attribute(ns,x) [[ns::x]]
#endif
#endif

#if __has_cpp_attribute(xlang::pointer_attributes)
#define _XLANG_POINTER_ATTRIBUTES(...) __cpp_attribute(xlang,pointer_attributes(__VA_ARG__))
#else
#define _XLANG_POINTER_ATTRIBUTES(...)
#endif

#if __has_cpp_attribute(xlang::scalar_attributes)
#define _XLANG_SCALAR_ATTRIBUTES(...) __cpp_attribute(xlang,scalar_attributes(__VA_ARG__))
#else
#define _XLANG_SCALAR_ATTRIBUTES(...)
#endif

#if __has_cpp_attribute(xlang::niche_optimization)
#define _XLANG_NICHE_OPTIMIZE __cppattribute(xlang,niche_optimization)
#else
#define _XLANG_NICHE_OPTIMIZE
#endif

#if __has_cpp_attribute(xlang::layout)
#define _XLANG_LAYOUT(layout) __cppattribute(xlang,layout)
#else
#define _XLANG_LAYOUT(layout)
#endif

#if __has_cpp_attribute(xlang::hint_bounds)
#define _XLANG_BOUNDS_HINT(bounds) __cppattribute(xlang,hint_bounds)
#else
#define _XLANG_BOUNDS_HINT(bounds)
#endif

#endif //__LCCC_XLANGATTRIBUTES_HPP
