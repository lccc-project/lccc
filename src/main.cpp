
#include <string_view>
#include <cstdlib>
#include <vector>
#include <map>
/*
    The file is part of the lccc project. 
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
*/

#include <iostream>

#include <Definitions.hpp>

using namespace std::string_view_literals;

enum class OptimizationLevel : std::uint8_t{
    

    FirstReserved = 240,

    // -Og
    Debug = 250,
    // -Ofast, not necessarily compliant
    Fast = 252,
    // -Os
    Size = 253,
    // -Oz
    Zize = 254,
    // -Oextra, not stacked borrows compliant for rust
    Extra = 255 
};

enum class DebugLevel: std::uint8_t{
    Off = 0,
    LineTables = 1,
    Full = 2,
};

enum class LTOLevel : std::uint8_t{
    Off = 0,
    Thin = 1,
    Full = 2
};

enum class WarningLevel {
    Off,
    Warn,
    Error,
    Forbid
};

enum class TargetStage{
    Preprocess,
    TypeCheck,
    XIR,
    Assembly,
    Compile,
    CompileAndLink,
    RLib,
    Dependencies,
    ModuleServer
};

enum class DependencyStyle{
    Makefile,
    Ninjafile,
    CannonModules,
    XIRModules,
    // Yes, this produces a <name>.xmanifest file
    Manifest,
};

enum class OutputFormat{
    Executable,
    Object,
    PositionIndependentExecutable,
    Shared,
    SharedWithManifest,
    SharedWithRustManifest,
    StaticWithManifest,
    StaticWithRustManifest,
};

int main(int argc, char** argv){
    std::string_view sysroot{LCCC_DEFAULT_SYSROOT};
    std::string_view target{LCCC_DEFAULT_TARGET};
    std::string_view cxxlib{LCCC_DEFAULT_CXXLIB};
    std::vector<std::string_view> input_files{};
    std::vector<std::string_view> include_dirs{};
    std::vector<std::string_view> system_include_dirs{};
    std::vector<std::string_view> preprocessor_options{};
    std::vector<std::string_view> linker_options{};
    std::map<std::string_view,std::string_view> defines{};
    std::string_view output_file{};
    OptimizationLevel opt_lvl{};
    DebugLevel dbg_level{};
    LTOLevel lto_level{};
    std::map<std::string_view,bool> codegen_options{};
    std::map<std::string_view,WarningLevel> warnings{};
    WarningLevel global_warning_level{};
    bool opts_done{};
    std::string_view use_linker{};
    TargetStage target_stage{};
    DependencyStyle dep_style{};
    OutputFormat output_format{};
    

    if(argc<1)
        std::abort();
    std::string_view prg_name{argv[0]};

    


    std::string_view lang_name{};

    if(prg_name.substr(prg_name.rfind("rustc"sv))=="rustc"sv){
        lang_name = "rust"sv;
        std::cerr << "rustc CLI is not implemented yet\n";
        return 0;
    }else{
        argv++; 
        for(;*argv;argv++){
             
        }
    }
}
