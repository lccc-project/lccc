//
// Created by chorm on 2020-11-05.
//

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
#define unreachable_unchecked() (void)(*((int*)0))
#endif

#ifdef __lccc__
#if has_builtin(rust::__builtin_limit)
#define limit(ptr,size) ::__lccc::builtins::rust::__builtin_limit(ptr,size)
#endif
#endif

#ifndef limit
#define limit(ptr,size) ((size),(ptr))
#endif



#endif //LCCC_PROPERTIES_H
