


if(DEFINED LCCC_ENABLE_LLD)
    set(LLVM_ENABLE_LLD ${LCCC_ENABLE_LLD})
endif()
if(DEFINED LCCC_ENABLE_LIBCXX)
    set(LLVM_ENABLE_LIBCXX ${LCCC_ENABLE_LIBCXX})
endif()

if(LCCC_LLVM_USE_MLIR)
    set(LLVM_ENABLE_PROJECTS "mlir;ldd")
endif()

add_subdirectory(llvm-project/llvm)
message("-- Building Backend: llvm")