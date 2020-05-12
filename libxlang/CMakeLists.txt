message(STATUS "Building xlang library")

add_library(xlang-objects OBJECT include/Layout.hpp src/Layout.cpp)
target_include_directories(xlang-objects PUBLIC include)
add_library(xlang-shared SHARED $<TARGET_OBJECTS:xlang-objects>)
set_target_properties(xlang-shared PROPERTIES OUTPUT_NAME xlang
        SOVERSION "0.1.0")
add_library(xlang-static STATIC $<TARGET_OBJECTS:xlang-objects>)
set_target_properties(xlang-static PROPERTIES OUTPUT_NAME xlang)

lccc_link_backends(xlang-objects)
