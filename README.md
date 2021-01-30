# Lightning Creations Compiler Collection

A collection of compilers for various languages, including C, C++, and Rust,
 all using a uniform intermediate representation and architecture. 

Currently work in progress for all frontends and backends.

## License

Copyright (C) 2020-2021, Lightning Creations

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

All libraries contained within are dual licensed under the MIT License and the Apache v2 License. 
  See License-MIT.txt and License-APACHE.txt under the rust language folder. 
 They are additionally licensed under the terms of the GNU Lesser General Public License,
  at Version 3, or (at your option) any later version. When dealing in the software, you may choose to use the terms of the GNU Lesser General Public License, or the above dual license, or both.  

The llvm-project, included by codegen-llvm is Copyright (C) The LLVM project,
 licensed under the Apache v2.

## Frontends

The following language frontends are available or planned (A check indicates the frontend is complete):

- [ ] C
- [ ] C++
- [ ] Rust

## Backends

The following code generators are available or planned (A check indicates the backend is complete):

- [ ] x86 (direct codegen)
- [ ] llvm 

## Building and Installing with CMake

lccc can be configured and built using cmake. This is the most complete and most supported method for building lccc. To build with cmake, simply enter a new directory for the build (typically a `build` subdirectory under the source tree), and perform the following build steps:
* `cmake [OPTIONS]... <path/to/lccc/source>`
* `cmake --build .`
* `cmake --install .` (with sudo as necessary)

Building lccc in-tree is not supported. However, building in a directory (such as `build`) within the source tree is supported. 

Dependencies:
- cmake 3.17
- Either make or ninja
- A C++ compiler that supports C++17
- A C compiler that support C11
- (optional) cargo (when building the `rust` frontend and libraries)

Standard Options:
- `CMAKE_C_COMPILER`, `CMAKE_CXX_COMPILER`: Sets the compilers to use to build the program. MUST support C11 and C++17 respectively.
- `CMAKE_INSTALL_PREFIX`: Sets the prefix to install lccc to. The GNUInstallDirs Options also provide finer control over the destinanations of various components.
- `LCCC_ENABLE_LANGUAGES`: Sets the language frontends to build. Defaults to `all`.
- `LCCC_ENABLE_BACKENDS`: Sets the code generator backends to build. Defaults to `all`.
- `LCCC_USE_CXXLIB`: Sets the C++ Standard Library to use when building the compiler if the host compiler supports the `-stdlib` option. Supported options are `libstdc++`, `libc++`, and `liblc++`
- `LCCC_DEFAULT_CXXLIB`: Sets the default C++ Standard Library to build with, instead of the platform default. 
- `LCCC_USE_LINKER`: Sets the linker to use when building the compiler if the host compiler supports the `-fuse-ld` option. 
- `LCCC_INSTALL_LIBSRC`: Installs the source code for the standard libraries.
- `LCCC_INSTALL_SRCDIR`: Directory to install source code for the standard libraries into. Defaults to `${CMAKE_INSTALL_LIBDIR}/lccc/libraries/src`

Cross Compiler Options:
- `LCCC_DEFAULT_TARGET`: If lccc is being built as a cross compiler, set this to the default target to build with when it's not detected from the program name and not set with the `--target` option. Defaults to the host target
- `LCCC_LIBRARY_TARGETS`: If lccc is being built for use as a cross compiler, set this to each target you want to build standard libraries for. By default, this includes the default target. 
- `LCCC_DEFAULT_SYSROOT_DIR`: The directory to use to store sysroots by default
- `LCCC_<TARGET>_SYSROOT`: if `<TARGET>` is a library target specified above, set to the directory sysroot to install libraries into, and to search for host libraries when building standard libraries. Defaults to `/` for the host target, and `${LCCC_DEFAULT_SYSROOT_DIR}/<TARGET>`. 
- `LCCC_<TARGET>_HOSTED`: If `<TARGET>` is a library target specified above, set to ON if the target is a hosted operating system target, and OFF otherwise. Defaults to ON if one of the following files can be found by a search in `/lib` or `/usr/lib`, as well as `/lib/<TARGET>` (with all variations on target), and `/usr/lib/<TARGET>` (likewise), and OFF if not: `libc.a`, `libc.so`, `libc.so.{1..6}`, `libcygwin1.dll.a`, `libucrt.lib`, `ucrt.lib`. 
    - If your operating system uses a different naming scheme, setting `LCCC_<TARGET>_HOSTED` likely will not work, as the standard 
    - For ELF targets, creating a symbolic link from libc.so to the OS Standard shared C library will generally work for cross compilation (or renaming/linking the static library to libc.a), provided the library has the symbols from the C standard library.
    - For other targets, or for formal support, please open an issue at <https://github.com/LightningCreations/lccc/issues>
    - If your target is not hosted, but one of those libraries are included in the sysroot (for example, a freestanding libc.a is provided), you would need to set this variable explicitly. 
    - Note that not just any library can be defined with these forms. It is expected those libraries contain all functions defined by C99, except for the builtin functions `memcpy`, `memmove`, and `memcmp`. Additionally, other operating system specific components are required for building certain parts of libraries, though an incomplete version of rust `libstd` and `libc++` can be built with only C99 apis.

Cross Compiling lccc:
- `LCCC_CROSSCOMPILING`: Set if lccc is being cross compiled. This prevents detection of the host target. Set by default if CMAKE_CROSSCOMPILING is defined. 
- `LCCC_HOST_TARGET`: The host target for lccc.
- `LCCC_DEFAULT_SYSROOT`: The Sysroot to use by default when targetting the host set above
- `CMAKE_CROSSCOMPILING_EMULATOR`: When building libraries or bootstrapping, use this program to run the first stage compiler with. 

## Building and Installing with autotools

*Work In Progress, autotools is not yet complete*

Dependencies:
* A Posix compliant shell
* make
* A C++ compiler that supports C++17
* A C compiler that supports C11
* (optional) cargo (when building the rust frontend)


