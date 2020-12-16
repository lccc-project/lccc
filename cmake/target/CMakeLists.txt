

if(NOT LCCC_CROSS_COMPILING)
if(WIN32)
    if(MSVC)
        set(LCCC_DETECTED_HOST_TARGET ${CMAKE_SYSTEM_PROCESSOR}-pc-windows-msvc CACHE INTERNAL "")
    elseif(MSYS OR MINGW)
        if(CMAKE_SIZEOF_VOID_P EQUAL 8)
            set(LCCC_DETECTED_HOST_TARGET ${CMAKE_SYSTEM_PROCESSOR}-w64-mingw32 CACHE INTERNAL "")
        else()
            set(LCCC_DETECTED_HOST_TARGET ${CMAKE_SYSTEM_PROCESSOR}-pc-mingw32 CACHE INTERNAL "")
        endif()
    elseif(CYGWIN)
        find_program(SHELL NAMES sh bash zsh cash)
        if(NOT SHELL)
            set(LCCC_DETECTED_HOST_TARGET ${CMAKE_SYSTEM_PROCESSOR}-pc-cygwin CACHE INTERNAL "") # Fallback to x86_64-pc-cygwin
        else()
            execute_process(
                    COMMAND ${SHELL} ${CMAKE_CURRENT_SOURCE_DIR}/config.guess
                    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                    OUTPUT_VARIABLE _LCCC_DETECTED_HOST_TARGET
            )
            string(STRIP "${_LCCC_DETECTED_HOST_TARGET}" _LCCC_DETECTED_HOST_TARGET)
            set(LCCC_DETECTED_HOST_TARGET ${_LCCC_DETECTED_HOST_TARGET} CACHE INTERNAL "")
        endif()
    else()
        # Fallback to msvc
        set(LCCC_DETECTED_HOST_TARGET ${CMAKE_SYSTEM_PROCESSOR}-pc-windows-msvc CACHE INTERNAL "")
    endif()
else()
    execute_process(
            COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/config.guess
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            OUTPUT_VARIABLE _LCCC_DETECTED_HOST_TARGET
    )
    string(STRIP "${_LCCC_DETECTED_HOST_TARGET}" _LCCC_DETECTED_HOST_TARGET)
    set(LCCC_DETECTED_HOST_TARGET ${_LCCC_DETECTED_HOST_TARGET} CACHE INTERNAL "")
endif()

if(NOT DEFINED LCCC_DETECTED_HOST_TARGET)
    message(FATAL "Could not detect lccc host target")
endif()
endif()