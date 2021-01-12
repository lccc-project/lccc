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
#include "Plugins.h"
#include <dlfcn.h>
#include <system_error>
#include <unistd.h>

using namespace std::string_literals;

lccc::Plugin::Plugin(std::string name, std::string args)
    :name{std::move(name)},args_stored{std::move(args)},handle{}{
    if(geteuid()==0)
        throw std::runtime_error{"Plugin loading is disabled with effective root"};
    std::string_view view{this->args_stored};
    std::string_view::size_type n;
    while((n=view.find(','))!=std::string_view::npos){
        this->args.emplace_back(view.substr(0,n));
        view = view.substr(n+1);
    }
    std::string ename{"xlang_"s+this->name+"_main"s};
    void* entry = dlsym(RTLD_DEFAULT,ename.c_str());
    if(entry){
        this->handle = RTLD_DEFAULT;
        this->entry_point = reinterpret_cast<lccc::unique_ptr<xlang::FileVisitor>(*)(xlang::FileVisitor*,lccc::span<lccc::string_view>)>(entry);
    }else{
        ename = LCCC_XLANG_PLUGIN_SEARCH_PATH;
        ename += "/xlang"s + name + ".so";
        handle = dlopen(ename.c_str(),RTLD_NOW);
        if(!handle)
            throw std::system_error{errno,std::system_category()};
        if((entry=dlsym(handle,"xlang_plugin_main")))
            throw std::system_error{errno,std::system_category()};
        this->entry_point = reinterpret_cast<lccc::unique_ptr<xlang::FileVisitor>(*)(xlang::FileVisitor*,lccc::span<lccc::string_view>)>(entry);
    }
}

lccc::Plugin::~Plugin() {

    this->visitor.reset();
    if(handle!=RTLD_DEFAULT)
        dlclose(handle);
}

lccc::xlang::FileVisitor *lccc::Plugin::load(lccc::xlang::FileVisitor *parent) {
    this->visitor = this->entry_point(parent,this->args);
    return &*this->visitor;
}
