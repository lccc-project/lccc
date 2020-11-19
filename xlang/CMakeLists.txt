cmake_minimum_required(VERSION 3.15)
project(xlang CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.hpp.in ${CMAKE_CURRENT_BINARY_DIR}/Config.hpp)



add_library(xlang SHARED include/xlang++/Visit.hpp src/Visit.cpp include/xlang++/Layout.h include/xlang++/Properties.h src/IRWriter.cpp)
target_include_directories(xlang PUBLIC include)
install(TARGETS xlang EXPORT xlang)

add_executable(xlang-prg src/Main.cpp src/Plugins.h src/LinuxPlugins.cpp ${CMAKE_CURRENT_BINARY_DIR}/Config.hpp)
target_include_directories(xlang-prg PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(xlang-prg xlang dl)

set_target_properties(xlang PROPERTIES OUTPUT_NAME xlang)

install(TARGETS xlang-prg)
