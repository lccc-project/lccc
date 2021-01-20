
set(LCCC_LCRUST_CURRENT_ABI_VERISON 0)
set(LCCC_LCRUST_ABI_VERSION ${LCCC_LCRUST_CURRENT_ABI_VERISON} CACHE STRING "The abi version number lccc compiles for")

if(LCCC_LCRUST_ABI_VERSION GREATER LCCC_LCRUST_CURRENT_ABI_VERISON)
    message(WARNING "Requested ABI Version ${LCCC_LCRUST_ABI_VERSION} is not yet in use."
                    "While this is valid, this can lead to breaking abi changes that affect rlib and dylib targets built" 
                    "with this compiler. ")
elseif(LCCC_LCRUST_ABI_VERSION LESS 0)
    message(FATAL_ERROR "Requested ABI Version ${LCCC_LCRUST_ABI_VERSION} is reserved.")
endif()

add_subdirectory(front_end)
