

set(_LCCC_LIBLCXX_FILES include/type_traits src/dummy.cpp include/bits/TypeTraits.hpp include/functional
        include/bits/XLangAttributes.hpp include/bits/Constexpr.hpp include/bits/Deprecated.hpp)



add_library(lc++-objects OBJECT ${_LCCC_LIBLCXX_FILES})

foreach(langv ${LCCC_ENABLE_CXX_STANDARDS})
    add_subdirectory(src/c++${langv})
    target_link_libraries(lc++-objects lc++${langv})
endforeach()

target_include_directories(lc++-objects SYSTEM PUBLIC include)

target_compile_definitions(lc++-objects PRIVATE -D_LCCC_BOOTSTRAP)
target_compile_options(lc++-objects PUBLIC -nostdinc++)

add_library(lc++-shared SHARED $<TARGET_OBJECTS:lc++-objects>)
add_library(lc++-static STATIC $<TARGET_OBJECTS:lc++-objects>)

if(LCCC_USE_INTERNAL_SYSLIBS)
    target_link_options(lc++-objects PUBLIC "-nostdlib")
    target_link_libraries(lc++-objects PUBLIC -Wl,--start-group c m gcc -Wl,--end-group)
else()
    target_link_options(lc++-objects PRIVATE "-nostdlib++")
endif()


message(STATUS "Enabling liblc++")