cmake_minimum_required(VERSION 3.17)
project(lccc)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

if(NOT DEFINED LCCC_ENABLE_LANGUAGES)
    set(LCCC_ENABLE_LANGUAGES c++ java rust)
endif()

if(NOT DEFINED LCCC_ENABLE_BACKENDS)
    set(LCCC_ENABLE_BACKENDS llvm)
endif()


include(CMakeCargo)
include(LCCCSelectors)

foreach(backend ${LCCC_ENABLE_BACKENDS})
    lccc_add_codegen(${backend})
endforeach()

foreach(lang ${LCCC_ENABLE_LANGUAGES})
    lccc_add_language(${lang})
endforeach()