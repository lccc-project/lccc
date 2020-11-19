//
// Created by chorm on 2020-11-19.
//

#ifndef LCCC_PLUGINS_H
#define LCCC_PLUGINS_H

#include <xlang++/Layout.h>
#include <xlang++/Visit.hpp>
#include <vector>
#include <string>

namespace lccc{
    struct Plugin{
    private:
        std::string args_stored;
        std::vector<lccc::string_view> args;
        lccc::unique_ptr<xlang::FileVisitor> visitor;
        std::string name;
        lccc::unique_ptr<xlang::FileVisitor>(*entry_point)(xlang::FileVisitor*,lccc::span<lccc::string_view>);
        void* handle;
    public:
        Plugin(std::string name,std::string args);
        ~Plugin();
        xlang::FileVisitor* load(xlang::FileVisitor* parent);
    };



}

#endif //LCCC_PLUGINS_H
