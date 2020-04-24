
add_library(lc++17 STATIC dummy.cpp)
set_target_properties(lc++17 PROPERTIES CXX_STANDARD 17)
target_compile_options(lc++17 PRIVATE "-fPIC")