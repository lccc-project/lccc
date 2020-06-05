enable_language(CXX)
enable_language(C)

set(CMAKE_CXX_STANDARD_REQUIRED 17)

# To Build lccc's C/++ compiler, you need C++17.
set(_LCCC_ENABLE_CXX_STANDARDS 98 11 14 17 20)


if(NOT LCCC_NO_CXXLIB)
add_subdirectory(liblc++)
endif()
