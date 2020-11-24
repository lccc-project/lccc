cmake_minimum_required(VERSION 3.17)
project(lccc CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)



list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(LCCC_ENABLE_LANGUAGES c++;rust CACHE STRING "List of language frontends to build")

set(LCCC_ENABLE_BACKENDS llvm CACHE STRING "List of backends to build")


if(NOT DEFINED LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
    include(CheckCXXCompilerFlag)
    check_cxx_compiler_flag("-Wno-return-type-c-linkage" LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
endif()

if(LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
    add_compile_options("-Wno-return-type-c-linkage")
endif()

include(LCCCSelectors)
include(GNUInstallDirs)

set(LCCC_XLANG_PLUGIN_SEARCH_PATH lccc/xlang/plugins CACHE PATH "The Path to search for plugins")
set(LCCC_XLANG_PLUGIN_INSTALL_DIR lccc/xlang/plugins CACHE PATH "The path to install xlang plugins into")

if(IS_ABSOLUTE ${LCCC_XLANG_PLUGIN_SEARCH_PATH})
    set(LCCC_FULL_XLANG_PLUGIN_SEARCH_PATH ${LCCC_XLANG_PLUGIN_SEARCH_PATH})
else()
    set(LCCC_FULL_XLANG_PLUGIN_SEARCH_PATH ${CMAKE_INSTALL_FULL_LIBDIR}/${LCCC_XLANG_PLUGIN_SEARCH_PATH})
endif()

if(IS_ABSOLUTE ${LCCC_XLANG_PLUGIN_INSTALL_DIR})
    set(LCCC_FULL_XLANG_PLUGIN_INSTALL_DIR ${LCCC_XLANG_PLUGIN_INSTALL_DIR})
else()
    set(LCCC_FULL_XLANG_PLUGIN_INSTALL_DIR ${CMAKE_INSTALL_FULL_LIBDIR}/${LCCC_XLANG_PLUGIN_INSTALL_DIR})
endif()

message(STATUS "Searching for xlang plugins in directory: ${LCCC_FULL_XLANG_PLUGIN_SEARCH_PATH}")
message(STATUS "Installing xlang plugins into: ${LCCC_FULL_XLANG_PLUGIN_INSTALL_DIR}")

add_subdirectory(xlang)

foreach(backend ${LCCC_ENABLE_BACKENDS})
    lccc_add_codegen(${backend})
endforeach()


foreach(lang ${LCCC_ENABLE_LANGUAGES})
    lccc_add_language(${lang})
endforeach()