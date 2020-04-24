
add_library(lc++11 STATIC dummy.cpp)
set_target_properties(lc++11 PROPERTIES CXX_STANDARD 11)
target_compile_options(lc++17 PRIVATE "-fPIC")