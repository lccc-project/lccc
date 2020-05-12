enable_language(CXX)

set(CMAKE_CXX_STANDARD_REQUIRED 17)


add_library(template include/TemplateItem.hpp src/TemplateItem.cpp)

target_include_directories(template PUBLIC include)
