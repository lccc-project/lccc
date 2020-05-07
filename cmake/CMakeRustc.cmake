function(add_rust_library name type)
    add_library(${name} IMPORTED)
    set(rustc_src_dir ${CMAKE_CURRENT_SOURCE_DIR})
    set(rustc_out_dir ${CMAKE_CURRENT_BINARY_DIR}/${name}.dir/)
    set(rustc_incremental_dir ${rustc_out_dir}/incremental)
    set(rustc_depends ${ARGN})
    if(LCCC_RUST_COMPILER_OVERRIDE)
        set(RUSTC ${LCCC_RUST_COMPILER_OVERRIDE})
    else()
        set(RUSTC ${CMAKE_Rust_Compiler})
    endif()
    set_target_properties(${name} PROPERTIES
            RUSTC_ROOT_FILE src/lib.rs)
    if(type STREQ LIB OR type STREQ RLIB)
        set_target_properties(${name} PROPERTIES
                PREFIX ${CMAKE_STATIC_LIBRARY_PREFIX}
                SUFFIX ".rlib"
                OUTPUT_NAME ${name}
                RUSTC_CRATE_TYPE "rlib")
    elseif(type STREQ STATIC)
        set_target_properties(${name} PROPERTIES
                PREFIX ${CMAKE_STATIC_LIBRARY_PREFIX}
                SUFFIX ${CMAKE_STATIC_LIBRARY_SUFFIX}
                OUTPUT_NAME ${name}
                RUSTC_CRATE_TYPE "staticlib")
    elseif(type STREQ DYNLIB)
        set_target_properties(${name} PROPERTIES
                PREFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                SUFFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                OUTPUT_NAME ${name}
                RUSTC_CRATE_TYPE "dynlib")
    elseif(type STREQ SHARED OR type STREQ CDYNLIB)
        set_target_properties(${name} PROPERTIES
                PREFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                SUFFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                OUTPUT_NAME ${name}
                RUSTC_CRATE_TYPE "cdynlib")
    elseif(type STREQ PROCMACRO)
        set_target_properties(${name} PROPERTIES
                PREFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                SUFFIX ${CMAKE_SHARED_LIBRARY_PREFIX}
                OUTPUT_NAME ${name}
                RUSTC_CRATE_TYPE "procmacro")
    elseif(type STREQ EXECUTABLE OR type STREQ BINARY)
        message(FATAL_ERROR "executable crates are not supported by add_rust_library. Used add_rust_executable instead")
    else()
        list(APPEND rustc_depends ${type})
    endif()
    if(LCCC_RUST_STANDARD)
        set_target_properties(${name} PROPERTIES RUST_STANDARD ${LCCC_RUST_STANDARD})
    else()
        set_target_properties(${name} PROPERTIES RUST_STANDARD 2018)
    endif()
    file(MAKE_DIRECTORY ${rustc_out_dir})
    set(rustc_libfile $<TARGET_PROPERTIES:${name},PREFIX>$<TARGET_PROPERTIES:${name},OUTPUT_NAME>$<TARGET_PROPERTIES:${name},SUFFIX>)
    add_custom_command(OUTPUT ${rustc_libfile}
            DEPENDS ${rustc_depends} ${rustc_out_dir} $<TARGET_PROPERTIES:${name},RUSTC_LINK_TARGETS>
            WORKING_DIRECTORY ${rustc_src_dir}
            COMMAND ${RUSTC} --crate-name ${name} --edition=$<TARGET_PROPERTIES:${name},RUST_STANDARD>
            $<TARGET_PROPERTIES:${name},RUSTC_ROOT_FILE>
            --crate-type $<TARGET_PROPERTIES:${name},RUSTC_CRATE_TYPE>
            $<TARGET_PROPERTIES:${name},COMPILE_OPTIONS>
            -C incremental=${rustc_incremental_dir}
            --outdir ${rustc_out_dir}
            $<TARGET_PROPERTIES:${name},LINK_OPTIONS>
            $<TARGET_PROPERTIES:${name},RUSTC_DEPS>
            )
    add_custom_target(_${name}_internal ALL DEPENDS ${rustc_out_dir}/${rustc_libfile})
    add_dependencies(${name} _${name}_internal)
    set_target_properties(${name} PROPERTIES IMPORTED_LOCATION ${rustc_out_dir}/${rustc_libfile})
endfunction()

function(rust_target_link_libraries target)
    set(libraries ${ARGN})
    get_target_property(TEST_IS_RUST_TARGET ${target} RUSTC_CRATE_TYPE)
    if(TEST_IS_RUST_TARGET STREQ "")
        message(WARNING "rust_target_link_libraries expects a rust target. Falling back to target_link_libraries")
        target_link_libraries(${target} ${libraries})
    else()
        get_target_property(OLD_LINK_OPTIONS ${target} LINK_OPTIONS)
        get_target_property(OLD_RUSTC_DEPS ${target} RUSTC_DEPS)
        get_target_property(OLD_RUSTC_LINK_TARGETS ${target} RUSTC_LINK_TARGETS)
        foreach(lib ${libraries})
            if(lib MATCHES "^-.+$")
                # Link Option, just append to LINK_OPTIONS
                list(APPEND OLD_LINK_OPTIONS ${lib})
            elseif(NOT TARGET ${lib})
                # Assume this is just a regular library, add it as a -l parameter
                list(APPEND OLD_LINK_OPTIONS -l ${lib})
            else()
                get_target_property(TEST_IS_RUST_TARGET ${lib} RUSTC_CRATE_TYPE)
                if(TEST_IS_RUST_TARGET STREQ "")
                    # Not a rust target, so don't add it to extern
                    # TODO: Add support for non-rust object libraries
                    list(APPEND OLD_LINK_OPTIONS -L $<TARGET_FILE_DIR:${lib}> -l $<TARGET_PROPERTIES:${lib},OUTPUT_NAME>)
                    list(APPEND OLD_RUSTC_DEPS ${lib})
                else()
                    list(APPEND OLD_RUSTC_DEPS ${lib})
                    list(APPEND OLD_RUSTC_LINK_TARGETS --extern $<TARGET_FILE:${lib}>)
                endif()
            endif()
        endforeach()
        set_target_properties(${target} PROPERTIES
                LINK_OPTIONS ${OLD_LINK_OPTIONS}
                RUSTC_DEPS ${OLD_RUSTC_DEPS}
                RUSTC_LINK_TARGETS ${OLD_RUSTC_LINK_TARGETS})
    endif()
endfunction()
