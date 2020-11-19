

add_subdirectory(llvm-project/llvm EXCLUDE_FROM_ALL)

add_library(codegen-llvm MODULE src/Main.cpp)

target_link_libraries(codegen-llvm xlang LLVMCore)

set_target_properties(codegen-llvm PROPERTIES
        PREFIX "xlang")

install(TARGETS codegen-llvm DESTINATION ${LCCC_FULL_XLANG_PLUGIN_INSTALL_DIR})
