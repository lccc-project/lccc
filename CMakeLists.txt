cmake_minimum_required(VERSION 3.17)
project(lccc)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

if(NOT DEFINED LCCC_ENABLE_LANGUAGES)
    set(LCCC_ENABLE_LANGUAGES c++ java rust)
endif()

if(NOT DEFINED LCCC_ENABLE_BACKENDS)
    set(LCCC_ENABLE_BACKENDS llvm)
endif()
list(REMOVE_DUPLICATES LCCC_ENABLE_BACKENDS)
list(REMOVE_DUPLICATES LCCC_ENABLE_LANGUAGES)

include(CMakeCargo)
include(LCCCSelectors)

# MLIR is special, so do special stuff
list(FIND ${LCCC_ENABLE_BACKENDS} "mlir" mlir_found)
if(NOT (mlir_found EQUAL -1))
    set(_LCCC_LLVM_USE_MLIR ON)
    list(REMOVE_AT LCCC_ENABLE_BACKENDS ${mlir_found})
endif()

foreach(backend ${LCCC_ENABLE_BACKENDS})
    lccc_add_codegen(${backend})
endforeach()

add_subdirectory(libcodegen)

foreach(lang ${LCCC_ENABLE_LANGUAGES})
    lccc_add_language(${lang})
endforeach()