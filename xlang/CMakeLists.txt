cmake_minimum_required(VERSION 3.15)
project(xlang CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)



add_library(xlang SHARED include/xlang++/Visit.hpp src/Visit.cpp include/xlang++/Layout.h include/xlang++/Properties.h)
target_include_directories(xlang PUBLIC include)

