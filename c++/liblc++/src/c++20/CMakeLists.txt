
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_library(lc++20 OBJECT dummy.cpp)

target_compile_options(lc++20 PRIVATE "-fPIC")