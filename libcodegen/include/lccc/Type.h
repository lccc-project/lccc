//
// Created by chorm on 2020-04-19.
//

#ifndef LCCC_TYPE_H
#define LCCC_TYPE_H

#include <stdint.h>

typedef struct CodegenType CodegenType;

CodegenType* Codegen_integerType(uint8_t bits);
CodegenType* Codegen_floatType(uint8_t bits);

#endif //LCCC_TYPE_H
