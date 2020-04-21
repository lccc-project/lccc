enable_language(C)

include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)
include_directories(include)

configure_file(include/lccc/Config.h.in include/lccc/Config.h)

message(STATUS "Building libcodegen")

add_library(codegen STATIC src/setup.c include/lccc/File.h include/lccc/Type.h include/lccc/Register.h include/lccc/Decl.h)

lccc_link_backends(codegen)