/**
 * xlang++/Properties.h
 * This file is part of libxlang, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  libxlang is additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

#ifndef LCCC_PROPERTIES_H
#define LCCC_PROPERTIES_H

#ifndef __has_builtin
#define has_builtin(x) 0
#else
#define has_builtin(x) __has_builtin(x)
#endif


#if has_builtin(__builtin_unreachable)
#define unreachable_unchecked() __builtin_unreachable()
#else
#define unreachable_unchecked() (void)((int)(*((int*)0)))
#endif

#ifdef __lccc__
#if has_builtin(rust::__builtin_limit)
#define limit(ptr,size) ::__lccc::builtins::rust::__builtin_limit(ptr,size)
#endif
#endif

#ifndef limit
#define limit(ptr,size) (static_cast<void>(size),(ptr))
#endif

#ifdef _WIN32
#ifdef __GNUC__
#define XLANG_EXPORT __attribute__((dllexport))
#define XLANG_IMPORT __attribute__((dllimport))
#else
#define XLANG_EXPORT __declspec(dllexport)
#define XLANG_IMPORT __declspec(dllimport)
#endif
#else 
#define XLANG_EXPORT __attribute__((visibility("default")))
#define XLANG_IMPORT __attribute__((visibility("default")))
#endif 

#ifdef BUILD_LIBXLANG
#define XLANG_API XLANG_EXPORT
#else 
#define XLANG_API XLANG_IMPORT
#endif



#endif //LCCC_PROPERTIES_H
