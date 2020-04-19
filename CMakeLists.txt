cmake_minimum_required(VERSION 3.17)
project(lccc)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

if(NOT DEFINED LCCC_ENABLE_LANGUAGES)
    set(LCCC_ENABLE_LANGUAGES c++ java rust)
endif()



include(CMakeCargo)

foreach(lang ${LCCC_ENABLE_LANGUAGES})
    add_subdirectory(${lang})
endforeach()