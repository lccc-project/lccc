//
// Created by chorm on 2020-04-20.
//

#ifndef LCCC_REGISTER_H
#define LCCC_REGISTER_H

#include <lccc/Decl.h>
#include <lccc/Config.h>
#include <lccc/File.h>
#include <lccc/Type.h>

typedef struct{
    char _empty;
}Backend;

LCCC_CDECL void Register_backend(const char* name,const Backend* backend);

#endif //LCCC_REGISTER_H
