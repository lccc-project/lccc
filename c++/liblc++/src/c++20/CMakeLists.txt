
add_library(lc++20 OBJECT dummy.cpp)
# This is stupid, never do this.
# Hack to work arround C++20
target_compile_definitions(lc++20 PRIVATE -U__cplusplus -D__cplusplus=202002L)
target_compile_options(lc++20 PRIVATE "-fPIC")