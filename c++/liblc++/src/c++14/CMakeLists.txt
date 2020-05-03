
add_library(lc++14 OBJECT dummy.cpp)
set_target_properties(lc++14 PROPERTIES CXX_STANDARD 14)
target_compile_options(lc++14 PRIVATE "-fPIC")