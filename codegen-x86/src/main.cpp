
#include <xlang++/Visit.hpp>

extern"C"{
    XLANG_EXPORT
    lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_plugin_main(lccc::xlang::FileVisitor* parent,lccc::span<lccc::string_view> args){
        lccc::unique_ptr<lccc::xlang::FileVisitor> ptr{};

        return ptr;
    }
}