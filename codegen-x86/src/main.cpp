
#include <xlang++/Visit.hpp>
#include <xlang++/layout/UniquePtr.hpp>

#include <stdio.h>
#include <optional>


struct X86AssemblyOutputVisitor : lccc::xlang::FileVisitor{
private:
   FILE* output;
   FILE* diagnostic;
   std::optional<lccc::Target> target;
public:
    X86AssemblyOutputVisitor(FileVisitor* parent) : FileVisitor{parent}, output{},diagnostic{}{}
    virtual void visitOutputFile(FILE* file) override{
        output = file;
    }
    virtual void visitDiagnosticFile(FILE* file) override{
        diagnostic = file;
    }
};

extern"C"{
    XLANG_EXPORT
    lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_plugin_main(lccc::xlang::FileVisitor* parent,lccc::span<lccc::string_view> args){
        lccc::unique_ptr<lccc::xlang::FileVisitor> ptr{new X86AssemblyOutputVisitor{parent}};

        return ptr;
    }
}