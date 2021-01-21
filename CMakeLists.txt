cmake_minimum_required(VERSION 3.17)
project(lccc CXX)

set(CMAKE_MESSAGE_CONTEXT "lccc")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

set(LCCC_VERSION 0.1)

message(STATUS "LCCC Version ${LCCC_VERSION}")

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(LCCC_ENABLE_LANGUAGES c++;rust CACHE STRING "List of language frontends to build")

set(LCCC_ALL_BACKENDS llvm x86)

set(LCCC_ENABLE_BACKENDS all CACHE STRING "List of backends to build")

if(LCCC_ENABLE_BACKENDS STREQUAL "all")
   set(LCCC_ENABLE_BACKENDS ${LCCC_ALL_BACKENDS})
endif()

include(CheckCXXCompilerFlag)

if(NOT DEFINED LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
    check_cxx_compiler_flag("-Wno-return-type-c-linkage" LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
endif()


if(LCCC_HAS_WNO_RETURN_TYPE_C_LINKAGE)
    add_compile_options("-Wno-return-type-c-linkage")
endif()

include(LCCCSelectors)
include(GNUInstallDirs)

option(LCCC_CROSS_COMPILING "Set if lccc is being cross compiled" ${CMAKE_CROSSCOMPILING})

add_subdirectory(cmake/target)

set(LCCC_HOST_TARGET ${LCCC_DETECTED_HOST_TARGET} CACHE STRING "The host target name of lccc")
set(LCCC_DEFAULT_TARGET ${LCCC_DETECTED_HOST_TARGET} CACHE STRING "The default target name for lccc")
set(LCCC_LIBRARIES_TARGETS ${LCCC_DEFAULT_TARGET} CACHE STRING "The target (or targets) to build standard libraries for")

list(JOIN LCCC_LIBRARIES_TARGETS ", " _LCCC_LIBRARIES_TARGETS)

message(STATUS "Targeting ${LCCC_DEFAULT_TARGET}")
message(STATUS "Building libraries for targets: ${_LCCC_LIBRARIES_TARGETS}")

set(LCCC_DEFAULT_SYSROOT_DIR ${CMAKE_INSTALL_LIBDIR}/lccc/sysroot CACHE PATH "The path (relative to the prefix) to install target sysroots to")

GNUInstallDirs_get_absolute_install_dir(LCCC_FULL_DEFAULT_SYSROOT_DIR LCCC_DEFAULT_SYSROOT_DIR)

foreach(target ${LCCC_LIBRARIES_TARGETS})
    if(target STREQUAL LCCC_HOST_TARGET)
        set(LCCC_${target}_SYSROOT "" CACHE PATH "The sysroot for ${target}")
        if(LCCC_${target}_SYSROOT STREQUAL "")
            set(LCCC_${target}_FULL_SYSROOT "/")
        endif()
    else()
        set(LCCC_${target}_SYSROOT ${LCCC_DEFAULT_SYSROOT_DIR}/${target} CACHE PATH "The sysroot for ${target}")
        GNUInstallDirs_get_absolute_install_dir(LCCC_${target}_FULL_SYSROOT LCCC_${target}_SYSROOT)
    endif()
    
    message(STATUS "Installing libraries for target ${target} to ${LCCC_${target}_FULL_SYSROOT}")
    message(CHECK_START "Checking if hosted libraries are available for ${target}")
    if(NOT DEFINED LCCC_${target}_HOSTED)
        list(APPEND CMAKE_MESSAGE_INDENT "  ")
        message(CHECK_START "Searching for os libraries for target ${target} in ${LCCC_${target}_FULL_SYSROOT}")
        get_target_aliases(${target} LCCC_${target}_ALIASES)
        message(DEBUG "${LCCC_${target}_ALIASES}")
        set(LCCC_${target}_SEARCH_PATHS "")
        foreach(dir lib usr/lib)
            message(DEBUG "Within search path loop for ${dir}")
            list(APPEND LCCC_${target}_SEARCH_PATHS ${LCCC_${target}_FULL_SYSROOT}/${dir})
            foreach(t ${LCCC_${target}_ALIASES})
                list(APPEND LCCC_${target}_SEARCH_PATHS ${LCCC_${target}_FULL_SYSROOT}/${dir}/${t})
            endforeach()
        endforeach()
        message(DEBUG "${LCCC_${target}_SEARCH_PATHS}")

        find_library(_LCCC_${target}_SYSROOT_BASE_LIB
            NAMES libc.so libc.so.1 libc.so.2 libc.so.3 libc.so.4 libc.so.5 libc.so.6 libc.a NAMES_PER_DIR
            PATHS ${LCCC_${target}_SEARCH_PATHS}
            NO_DEFAULT_PATH)
        if(_LCCC_${target}_SYSROOT_BASE_LIB STREQUAL "_LCCC_${target}_SYSROOT_BASE_LIB-NOTFOUND")
            set(LCCC_${target}_HOSTED_INIT FALSE)
            message(CHECK_FAIL "Not Found")
        else()
            set(LCCC_${target}_HOSTED_INIT TRUE)
            message(CHECK_PASS "${_LCCC_${target}_SYSROOT_BASE_LIB}")
        endif()
        list(POP_BACK CMAKE_MESSAGE_INDENT)
        set(LCCC_${target}_HOSTED ${LCCC_${target}_HOSTED_INIT} CACHE BOOL "If ON, build all libraries for the target. Otherwise, only build freestanding libraries")
    endif()
    if(LCCC_${target}_HOSTED)
        message(CHECK_PASS "Yes")
    else()
        message(CHECK_FAIL "No")
    endif()
endforeach()

set(LCCC_XLANG_PLUGIN_SEARCH_PATH lccc/xlang/plugins CACHE PATH "The Path to search for plugins")
set(LCCC_XLANG_PLUGIN_INSTALL_DIR lccc/xlang/plugins CACHE PATH "The path to install xlang plugins into")

set(LCCC_DEFAULT_SYSROOT_INIT ${LCCC_${LCCC_DEFAULT_TARGET}_FULL_SYSROOT})

set(LCCC_DEFAULT_SYSROOT ${LCCC_DEFAULT_SYSROOT_INIT} CACHE PATH "The default value for the sysroot")


set(LCCC_DEFAULT_CXXLIB "" CACHE STRING "Name of the default C++ Standard library")

set_property(CACHE LCCC_DEFAULT_CXXLIB PROPERTY STRINGS "" stdc++ libstdc++ c++ libc++ lc++ liblc++)

set(LCCC_DEFAULT_INCLUDE_DIRS include usr/include usr/local/include CACHE PATH "Default include paths to search for headers, resolved within the sysroot")

set(LCCC_DEFAULT_CXX_INCLUDE_DIRS ${CMAKE_FULL_INSTALL_INCLUDEDIR}/lccc/${LCCC_VERSION} CACHE PATH "Default include paths to search for c++ standard library headers (when using liblc++)")

message(STATUS "Using --sysroot=${LCCC_DEFAULT_SYSROOT} by default")


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

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Definitions.hpp.in ${CMAKE_CURRENT_BINARY_DIR}/Definitions.hpp)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_executable(lccc ${CMAKE_CURRENT_BINARY_DIR}/Definitions.hpp src/main.cpp)

target_link_libraries(lccc xlang)

option(LCCC_BOOTSTRAP "Bootstrap lccc by recompiling after finishing the stage 1 build" OFF)
