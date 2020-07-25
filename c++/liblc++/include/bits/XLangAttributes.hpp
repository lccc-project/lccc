//
// Created by chorm on 2020-07-15.
//

#ifndef __LCCC_XLANGATTRIBUTES_HPP
#define __LCCC_XLANGATTRIBUTES_HPP

#ifndef __has_cpp_attribute
#define __has_cpp_attribute(x) 0
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
#define _XLANG_HAS_POINTER_ATTRIBUTES_ 1
#else
#define _XLANG_POINTER_ATTRIBUTES(...)
#endif

#if __has_cpp_attribute(xlang::scalar_attributes)
#define _XLANG_SCALAR_ATTRIBUTES(...) __cpp_attribute(xlang,scalar_attributes(__VA_ARG__))
#define _XLANG_HAS_SCALAR_ATTRIBUTES_ 1
#else
#define _XLANG_SCALAR_ATTRIBUTES(...)
#endif

#if __has_cpp_attribute(xlang::niche_optimization)
#define _XLANG_NICHE_OPTIMIZE __cpp_attribute(xlang,niche_optimization)
#define _XLANG_HAS_NICHE_OPTIMIZE_ 1
#else
#define _XLANG_NICHE_OPTIMIZE 
#endif

#if __has_cpp_attribute(xlang::layout)
#define _XLANG_LAYOUT(layout) __cpp_attribute(xlang,layout)
#define _XLANG_HAS_LAYOUT_ 1
#else
#define _XLANG_LAYOUT(layout)
#endif

#if __has_cpp_attribute(xlang::hint_bounds)
#define _XLANG_BOUNDS_HINT(bounds) __cpp_attribute(xlang,hint_bounds(bounds))
#define _XLANG_HAS_BOUNDS_HINT_ 1
#else
#define _XLANG_BOUNDS_HINT(bounds)
#endif

#if __has_cpp_attribute(xlang::allow_uninitialized)
#define _XLANG_ALLOW_UNINITIALIZED __cpp_attribute(xlang,allow_uninitialized)
#define _XLANG_HAS_ALLOW_UNINITIALIZED_ 1
#else
#define _XLANG_ALLOW_UNINITIALIZED
#endif



#endif //__LCCC_XLANGATTRIBUTES_HPP
