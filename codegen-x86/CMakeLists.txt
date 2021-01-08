
add_library(xlangcodegen-x86 MODULE src/main.cpp)

target_link_libraries(xlangcodegen-x86 PUBLIC xlang)

set_target_properties(xlangcodegen-x86 PROPERTIES PREFIX "")
