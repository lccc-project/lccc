cmake_minimum_required(VERSION 3.15)
project(xlang CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include_directories(include)

add_library(xlang-objects OBJECT include/xlang++/Visit.hpp src/Visit.cpp)
set_target_properties(xlang-objects PROPERTIES POSITION_INDEPENDENT_CODE ON)

add_library(xlang-shared SHARED $<TARGET_OBJECTS:xlang-objects>)
add_library(xlang-static STATIC $<TARGET_OBJECTS:xlang-objects>)
