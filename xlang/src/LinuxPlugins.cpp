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

#include <Config.hpp>
#include <xlang++/Plugins.h>
#include <dlfcn.h>
#include <system_error>
#include <stdexcept>
#include <string>
#include <filesystem>
#include <unistd.h>
#include <mutex>
#include <algorithm>

using namespace std::string_literals;


namespace lccc
{
    [[noreturn]] XLANG_API void throw_symbol_not_found(lccc::string_view sv)
    {
        throw std::runtime_error{std::string{sv}};
    }
    struct PluginImpl
    {
        void *hdl;
        std::string name;
        lccc::unique_ptr<xlang::FileVisitor> (*entry)(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args);
        std::vector<std::string> args;
    };

    Plugin::Plugin(lccc::string_view name, lccc::span<lccc::string_view> args) : impl{lccc::make_unique<PluginImpl>()}{
        static std::string_view path{LCCC_XLANG_PLUGIN_SEARCH_PATH};

        std::string iname{"xlang_"s+std::string{name}+"_main"s};
        impl->name = name;
        if(void* v = dlsym(RTLD_DEFAULT,iname.c_str());v){
            impl->hdl = RTLD_DEFAULT;
            impl->entry = reinterpret_cast<lccc::unique_ptr<xlang::FileVisitor> (*)(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args)>(v);
        }else{
            std::filesystem::path plugin_path{path};
            plugin_path /= "xlang"s+std::string{name}+".so"s;
            if(!(impl->hdl = dlopen(plugin_path.c_str(),RTLD_LAZY)))
                throw std::system_error{std::error_code{errno,std::system_category()},dlerror()};
            impl->entry = reinterpret_cast<lccc::unique_ptr<xlang::FileVisitor> (*)(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args)>(dlsym(impl->hdl,"xlang_plugin_main"));
        }

        std::transform(begin(args),end(args),std::back_insert_iterator<std::vector<std::string>>{impl->args},[](auto&& a){return std::string{a};});
    }

    Plugin::~Plugin(){
        if(impl->hdl!=RTLD_DEFAULT)
            dlclose(impl->hdl);
    }

    void * Plugin::find_sym(lccc::string_view name){
        std::string s_name{name};
        return dlsym(impl->hdl,s_name.c_str());
    }

    lccc::xlang::FileVisitor * Plugin::load(lccc::xlang::FileVisitor * parent){
        std::vector<lccc::string_view> args{};
        std::copy(begin(impl->args),end(impl->args),std::back_insert_iterator<std::vector<lccc::string_view>>{args});
        this->visitor = impl->entry(parent,args);
        return this->visitor.operator->();
    }


} // namespace lccc
