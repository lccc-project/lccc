


add_library(lc++ SHARED include/type_traits src/dummy.cpp include/bits/TypeTraits.hpp)

target_include_directories(lc++ SYSTEM PUBLIC include)

target_compile_definitions(lc++ PRIVATE -D_LCCC_BOOTSTRAP)
target_compile_options(lc++ PUBLIC -nostdinc)

foreach(langv ${LCCC_ENABLE_CXX_STANDARDS})
    add_subdirectory(src/c++${langv})
    target_link_libraries(lc++ lc++${langv})
endforeach()


set_target_properties(lc++ PROPERTIES LINK_FLAGS "-nodefaultlibs -fPIC")
set_target_properties(lc++ PROPERTIES LINK_LANGUAGE CXX)

message(STATUS "Enabling liblc++")