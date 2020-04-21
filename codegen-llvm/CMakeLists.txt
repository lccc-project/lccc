


if(DEFINED LCCC_ENABLE_LLD)
    set(LLVM_ENABLE_LLD ${LCCC_ENABLE_LLD})
endif()
if(DEFINED LCCC_ENABLE_LIBCXX)
    set(LLVM_ENABLE_LIBCXX ${LCCC_ENABLE_LIBCXX})
endif()

if(_LCCC_LLVM_USE_MLIR)
    set(LLVM_ENABLE_PROJECTS "mlir;lld")
    add_library(codegen-mlir-library ALIASN MLIRIR)
else()
    set(LLVM_ENABLE_PROJECTS "lld")
endif()

add_subdirectory(llvm-project/llvm)

set(LCCC_CODEGEN_LLVM "ON")

add_library(codegen-llvm-library ALIAS LLVMCore)