cmake_minimum_required(VERSION 3.17)
project(lccc CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

set(LCCC_VERSION 0.1)

message(STATUS "LCCC Version ${LCCC_VERSION}")

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

option(LCCC_CROSS_COMPILING "Set if lccc is being cross compiled" ${CMAKE_CROSSCOMPILING})

if(NOT DEFINED LCCC_DETECTED_HOST_TARGET)
    add_subdirectory(cmake/target) # Needed to get path to config.guess and be usable
endif()

set(LCCC_DEFAULT_TARGET ${LCCC_DETECTED_HOST_TARGET} CACHE STRING "The default target name for lccc")
set(LCCC_LIBRARIES_TARGETS ${LCCC_DEFAULT_TARGET} CACHE STRING "The target (or targets) to build standard libraries for")

list(JOIN LCCC_LIBRARIES_TARGETS ", " _LCCC_LIBRARIES_TARGETS)

message(STATUS "Targeting ${LCCC_DEFAULT_TARGET}")
message(STATUS "Building libraries for targets: ${_LCCC_LIBRARIES_TARGETS}")

set(LCCC_XLANG_PLUGIN_SEARCH_PATH lccc/xlang/plugins CACHE PATH "The Path to search for plugins")
set(LCCC_XLANG_PLUGIN_INSTALL_DIR lccc/xlang/plugins CACHE PATH "The path to install xlang plugins into")

if(DEFINED LCCC_${LCCC_DEFAULT_TARGET}_SYSROOT)
    set(LCCC_DEFAULT_SYSROOT_INIT ${LCCC_${LCCC_DEFAULT_TARGET}_SYSROOT})
elseif(LCCC_DEFAULT_TARGET STREQUAL LCCC_DETECTED_HOST_TARGET)
    set(LCCC_DEFAULT_SYSROOT_INIT "/")
endif()

set(LCCC_DEFAULT_SYSROOT ${LCCC_DEFAULT_SYSROOT_INIT} CACHE PATH "The default value for the sysroot")


set(LCCC_DEFAULT_CXXLIB "" CACHE STRING "Name of the default C++ Standard library")

set_property(CACHE LCCC_DEFAULT_CXXLIB PROPERTY STRINGS "" stdc++ libstdc++ c++ libc++ lc++ liblc++)

set(LCCC_DEFAULT_INCLUDE_DIRS include usr/include usr/local/include CACHE PATH "Default include paths to search for headers")

set(LCCC_DEFAULT_CXX_INCLUDE_DIRS ${CMAKE_FULL_INSTALL_INCLUDEDIR}/lccc/${LCCC_VERSION} CACHE PATH "Default include paths to search for c++ standard library headers (when using liblc++)")

message(STATUS "Using --sysroot=${LCCC_DEFAULT_SYSROOT} by default")

if(NOT IS_ABSOLUTE LCCC_DEFAULT_SYSROOT)
    message(FATAL "Sysroot is not an absolute path")
endif()

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

