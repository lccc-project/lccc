//
// Created by chorm on 2020-11-19.
//

#include <Config.hpp>
#include "Plugins.h"
#include <dlfcn.h>
#include <system_error>

using namespace std::string_literals;

lccc::Plugin::Plugin(std::string name, std::string args)
    :name{std::move(name)},args_stored{std::move(args)},handle{}{
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
