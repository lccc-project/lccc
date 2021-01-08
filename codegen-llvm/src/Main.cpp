//
// Created by chorm on 2020-11-18.
//

#include <xlang++/Layout.h>
#include <xlang++/Visit.hpp>

#include "XIRFile.hpp"


extern "C" {
    XLANG_EXPORT
    lccc::unique_ptr<lccc::xlang::FileVisitor>
    xlang_plugin_main(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args) {

        return {};
    }

}

