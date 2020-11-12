//
// Created by chorm on 2020-08-31.
//

#ifndef LCCC_XLANGTYPES_H
#define LCCC_XLANGTYPES_H

#ifndef __has_xlang_builtin_type_category
#define __has_xlang_builtin_type_category(x) 0
#endif

#if __has_xlang_builtin_type_category(__tuple)
#define _HAS_XLANG_TUPLE_
#endif

#if __has_xlang_builtin_type_category(__sum)
#define _HAS_XLANG_SUM_
#endif

#endif //LCCC_XLANGTYPES_H
