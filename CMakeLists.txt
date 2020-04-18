cmake_minimum_required(VERSION 3.17)
project(lccc)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

enable_language(Rust)
include(CMakeCargo)

add_subdirectory(lcrust)
