//
// Created by chorm on 2020-04-19.
//

#ifndef LCCC_FILE_H
#define LCCC_FILE_H

#include <lccc/Decl.h>
#include <lccc/Config.h>
#include <stdio.h>

typedef struct CodegenFile CodegenFile;

LCCC_CDECL CodegenFile* Codegen_createFile(const char* inputName,const char* inputLang,FILE* output);
LCCC_CDECL CodegenFile* Codegen_createFile_explicit(const char* inputName,const char* inputLang,FILE* output,const char* backendName);
LCCC_CDECL CodegenFile* Codegen_createJit(const char* inputLang);
LCCC_CDECL CodegenFile* Codegen_createJit_explicit(const char* inputLang,const char* backendName);

#endif //LCCC_FILE_H
