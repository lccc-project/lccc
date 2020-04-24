
add_library(lc++98 STATIC dummy.cpp)
set_target_properties(lc++98 PROPERTIES CXX_STANDARD 98)
target_compile_options(lc++98 PRIVATE "-fPIC")