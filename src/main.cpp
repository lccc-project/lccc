
#include <string>
#include <cstdlib>
#include <vector>
#include <map>
#include <cstdio>
#include <optional>
#include <cstdio>
#include <set>
#include <list>

#include <xlang++/Layout.h>
#include <xlang++/Plugins.h>
#include <xlang++/Visit.hpp>

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
using namespace std::string_literals;

enum class OptimizationLevel : std::uint8_t
{

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

enum class DebugLevel : std::uint8_t
{
    Off = 0,
    LineTables = 1,
    Full = 2,
};

enum class LTOLevel : std::uint8_t
{
    Off = 0,
    Thin = 1,
    Full = 2
};

enum class WarningLevel
{
    Off,
    Warn,
    Error,
    Forbid
};

enum class TargetStage
{
    Preprocess,
    TypeCheck,
    XIR,
    Assembly,
    Compile,
    CompileAndLink,
    Dependencies,
    ModuleServer
};

enum class DependencyStyle
{
    Makefile,
    Ninjafile,
    CannonModules,
    XIRModules,
    // Yes, this produces a <name>.xmanifest file
    Manifest,
    // This produces .rmanifest file
    RustManifest,
    // This produces lang.rtable
    RustLangItems,
    // These produce the above outputs, but as object files.
    SharedManfiest,
    SharedRustManfiest,
    SharedRustLangItems
};

enum class OutputFormat
{
    Executable,
    Object,
    PositionIndependentExecutable,
    Shared,
    SharedWithManifest,
    SharedWithRustManifest,
    StaticWithManifest,
    StaticWithRustManifest,
};

int main(int argc, char **argv)
{
    std::set<std::string_view> known_extensions{
        ".c",".C",".s",".S",".rs",".cpp",".cxx",".cc",
        ".i",".ixx",".ipp",".h",".hpp",".hxx", ".lch", ".xir",
        ".xmanifest", ".rmanifest", ".rast", ".rlib", ".xbc",
        ".rtable", 
        };
    std::optional<std::string_view> sysroot{};
    lccc::string_view target{LCCC_DEFAULT_TARGET};
    lccc::string_view cxxlib{LCCC_DEFAULT_CXXLIB};
    std::vector<lccc::string_view> input_files{};
    std::vector<std::string> link_files{};
    std::vector<lccc::string_view> preprocessor_options{};
    std::vector<lccc::string_view> linker_options{};
    std::map<lccc::string_view, std::string> source_file_map{};
    std::string_view output_file{};
    OptimizationLevel opt_lvl{};
    DebugLevel dbg_level{};
    LTOLevel lto_level{};
    std::map<std::string, bool> codegen_options{};
    std::map<std::string, WarningLevel> warnings{};
    WarningLevel global_warning_level{};
    std::string_view use_linker{};
    lccc::string_view codegen{LCCC_DEFAULT_BACKEND};
    TargetStage target_stage{TargetStage::CompileAndLink};
    DependencyStyle dep_style{};
    OutputFormat output_format{};

    if (argc < 1)
        std::abort();
    std::string_view prg_name{argv[0]};

    std::string_view lang_name{};

    if (auto i = prg_name.rfind("rustc"sv);i!=std::string::npos && prg_name.substr(i) == "rustc"sv)
    {
        lang_name = "rust"sv;
        std::cerr << "rustc CLI is not implemented yet\n";
        return 0;
    }
    else
    {
        if (auto i = prg_name.rfind("c++"sv);i!=std::string::npos && prg_name.substr(i) == "c++"sv)
            lang_name = "c++"sv;
        argv++;
        for (; *argv; argv++)
        {
            std::string_view arg = *argv;
            if (arg[0] != '-')
            {
                input_files.emplace_back(arg);
                auto dot_pos = arg.rfind("."sv);
                auto suffix = dot_pos!=std::string::npos ? arg.substr(dot_pos):""sv;
                auto main = arg.substr(0, dot_pos);
                if (known_extensions.count(suffix)!=0)
                    linker_options.emplace_back(arg);
                else
                {
                    FILE *f;
                    do
                    {
                        std::tmpnam((source_file_map[arg] = std::string(L_tmpnam, ' ')).data());
                    } while ((f = std::fopen(source_file_map[arg].c_str(), "wx")));
                    linker_options.emplace_back(source_file_map[arg]);
                }
            }
            else if (arg == "--version")
            {
                std::cout << "lccc v" LCCC_VERSION "\n"
                          << "Copyright (C) 2020 Lightning Creations. This program is a free software released under the terms of the GNU General Public License\n"
                          << "This program comes AS-IS, with absolutely NO WARRANTY\n";
                return 0;
            }
            else if (arg == "--help")
            {
                //TODO
                return 0;
            }
            else if (arg == "--sysroot")
            {
                if (!argv[1])
                {
                    return 1;
                }
                sysroot = *++argv;
            }
            else if (arg == "--target")
            {
                if (!argv[1])
                {
                    return 1;
                }
                target = *++argv;
            }

            else
            {
                arg = arg.substr(1);
                for (; arg.length() != 0; arg = arg.substr(1))
                {
                    if (arg[0] == 'c')
                        target_stage = TargetStage::Compile;
                    else if (arg[0] == 'S')
                        target_stage = TargetStage::Assembly;
                    else if (arg[0] == 'E')
                        target_stage = TargetStage::Preprocess;
                    else if (arg[0] == 'x')
                    {
                        arg = arg.substr(1);
                        if (arg == ""sv)
                        {
                            if (argv[1])
                                lang_name = *++argv;
                            else
                                return 1;
                        }
                        else
                            lang_name = arg;
                        break;
                    }else if (arg[0]=='o'){
                        arg = arg.substr(1);
                        if (arg == ""sv)
                        {
                            if (argv[1])
                                output_file = *++argv;
                            else
                                return 1;
                        }
                        else
                            output_file = arg;
                        break;
                    }else if (arg[0] == 'W'){
                        std::string_view warning{};
                        arg = arg.substr(1);
                        if (arg == ""sv)
                        {
                            if (argv[1])
                                warning = *++argv;
                            else
                                return 1;
                        }
                        else
                            warning = arg;
                        
                        break;
                    }
                }
            }
        }
    }
    std::list<lccc::Plugin> plugins;
    std::string frontend_name{"frontend-"s};
    lccc::Target tgt{target};
    frontend_name += lang_name;
    auto& frontend = plugins.front();
    plugins.emplace_back(frontend_name,lccc::span<lccc::string_view>{});
    for (auto && [input,output] : source_file_map)
    {
        lccc::xlang::FileVisitor* visitor{};
        for(auto it = rbegin(plugins);it!=rend(plugins);it++)
            visitor = it->load(visitor);
        FILE* inf = fopen(std::string{input}.c_str(),"rb");
        FILE* of  = fopen(output.c_str(),"wb");
        visitor->visitInputFile(inf);
        visitor->visitOutputFile(of);
        visitor->visitSourceFile(input);
        visitor->visitDiagnosticFile(stderr);
        visitor->visitTarget(tgt);
        visitor->visitEnd();
        
    }
}
