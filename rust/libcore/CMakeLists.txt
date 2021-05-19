
set(CMAKE_Rust_FLAGS_DEBUG "-C opt-level=g -C debug-info=2 -C debug-assertions=yes -C overflow-checks=yes" CACHE STRING "Flags to use when building in Debug configuration")
set(CMAKE_Rust_FLAGS_RELWITHDEBINFO "-C opt-level=2 -C debug-info=2" CACHE STRING "Flags to use when building in RelWithDebInfo configuration")
set(CMAKE_Rust_FLAGS_MINSIZEREL "-C opt-level=z" CACHE STRING "Flags to use when building in MinSizeRel configuration")
set(CMAKE_Rust_FLAGS_RELEASE "-C opt-level=3" CACHE STRING "Flags to use with building in release configuration")
set(CMAKE_Rust_STANDARD 2018)
set(CMAKE_Rust_STANDARD_REQUIRED TRUE)


cmake_minimum_required(VERSION 3.20)
project(rust-libcore Rust)

set(LCCC_STDLIB_ABI 1)

set(CMAKE_Rust_FLAGS "${CMAKE_Rust_FLAGS} -Z unstable-options -Z extra-codegen-opts -Z extra-opt-levels -Z build-abi=${LCCC_STDLIB_ABI} --cfg bootstrap --cfg stdlib-abi=\"${LCCC_STDLIB_ABI}\"")



set(LCCC_STDLIB_TARGET_CPU OFF CACHE STRING "Off, or the cpu to generate code for")

if(LCCC_STDLIB_TARGET_CPU)
    set(CMAKE_Rust_FLAGS "${CMAKE_Rust_FLAGS} -C target-cpu=${LCCC_STDLIB_TARGET_CPU}")
endif()

set(LCCC_TUNE_STDLIB OFF CACHE STRING "Off, or a cpu to tune generated code to")

if(LCCC_TUNE_STDLIB)
    set(CMAKE_Rust_FLAGS_RELEASE "${CMAKE_Rust_FLAGS_RELEASE} -Z tune-cpu=${LCCC_TUNE_STDLIB}")
endif()

foreach(feature ${LCCC_STDLIB_FEATURES})
    set(CMAKE_Rust_FLAGS "--cfg feature=\"${feature}\"")
endforeach()

get_property(_LCCC_STDLIB_MULTICONFIG GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)

add_library(core-static STATIC IMPORTED GLOBAL)

set_target_properties(core-static 
    PREFIX ${CMAKE_STATIC_LIBRARY_PREFIX}
    SUFFIX ${CMAKE_STATIC_LIBRARY_SUFFIX}
    VERSION ${LCCC_STDLIB_ABI}
    OUTPUT_NAME core
    Rust_STANDARD 2018
)

if(_LCCC_STDLIB_MULTICONFIG)
    set_target_properties(libcore Rust_FLAGS ${CMAKE_Rust_FLAGS})
    foreach(config ${CMAKE_CONFIGURATION_TYPES})
        string(TOUPPER "${config}" config_upper)
        set_target_properties(libcore IMPORTED_LOCATION_${confg} ${CMAKE_CURRENT_BINARY_DIR}/${config}/$<TARGET_PROPERTY:PREFIX>$<TARGET_PROPERTY:OUTPUT_NAME>$<TARGET_PROPERTY:SUFFIX>
            LIBRARY_OUTPUT_DIRECTORY_${config} ${CMAKE_CURRENT_BINARY_DIR}/${config}
            Rust_FLAGS_${config_upper} ${CMAKE_Rust_FLAGS_${config_upper}}
        )
    endforeach()
else()
    string(TOUPPER "${CMAKE_BUILD_TYPE}" _config_upper)
    set_target_properties(libcore Rust_FLAGS ${CMAKE_Rust_FLAGS} ${CMAKE_Rust_FLAGS_${_config_upper}}
        IMPORTED_LOCATION ${CMAKE_CURRENT_BINARY_DIR}/$<TARGET_PROPERTY:PREFIX>$<TARGET_PROPERTY:OUTPUT_NAME>$<TARGET_PROPERTY:SUFFIX>
        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
endif()

add_custom_target(libcore ALL 
        ${CMAKE_Rust_COMPILER} --edition=$<TARGET_PROPERTY:core-static,Rust_STANDARD>
        --crate-type rlib --crate-name $<TARGET_PROPERTY:core-static,OUTPUT_NAME>
        --out-dir $<$<IF:$<BOOL:${_LCCC_STDLIB_MULTICONFIG}>,$<GENEX_EVAL:$<TARGET_PROPERTY:core-static,OUTPUT_DIRECTORY_$<UPPER_CASE:$<CONFIG>>>>,$<TARGET_PROPERTY:core-static,OUTPUT_DIRECTORY>,
        $<TARGET_PROPERTY:core-static,Rust_FLAGS>
        $<$<BOOL:${_LCCC_STDLIB_MULTICONFIG}>:$<GENEX_EVAL:$<TARGET_PROPERTY:core-static,Rust_FLAGS_$<UPPER_CASE:$<CONFIG>>>>>

        --target ${CMAKE_Rust_COMPILER_TARGET}
        -D lccc::unprefixed_features
        -A lccc::rustc_incomplete_features
        -F lccc::stdlib_no_abi_tag
        -Z build-outdated
        -Z parallel-compile
        ${CMAKE_CURRENT_SOURCE_DIR}/src/lib.rs
        COMMENT Building Rust library libcore
    )
add_dependencies(core-static libcore)

if(_LCCC_BUILDSTD_EXPORT)
    add_library(core ALIAS core-static)
endif()
