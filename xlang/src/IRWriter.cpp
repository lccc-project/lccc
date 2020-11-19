//
// Created by chorm on 2020-11-16.
//

#include <xlang++/Layout.h>
#include <xlang++/Visit.hpp>

using namespace lccc::xlang;

struct IRScopeWriter : ScopeVisitor{

};

enum class IROutputType{
    Binary,
    Text
};


struct IRFileWriter: FileVisitor{
private:
    FILE* output_file{};
    FILE* diagnostic_file{};
    std::string file_name;
    IRScopeWriter inner;
public:
    IRFileWriter()=default;

    void visitOutputFile(FILE *output)final;
    void visitDiagnosticFile(FILE *diag)final;

    void visitSourceFile(lccc::string_view)final;
    void visitDiagnostic(lccc::string_view)final;
    void visitEnd()override;

};


[[gnu::used]]
extern"C" lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_irwriter_main(lccc::xlang::FileVisitor* parent,lccc::span<lccc::string_view> args){

    return {};
}

void IRFileWriter::visitOutputFile(FILE *output) {
    this->output_file = output;
}

void IRFileWriter::visitDiagnosticFile(FILE *diag) {
    this->diagnostic_file = diag;
}

void IRFileWriter::visitSourceFile(lccc::string_view name) {
    this->file_name = name;
}

void IRFileWriter::visitDiagnostic(lccc::string_view sv) {
    fwrite(sv.data(),1,sv.size(),this->diagnostic_file);
    fputs("",this->diagnostic_file);
}

void IRFileWriter::visitEnd() {
    fclose(this->output_file);
    fclose(this->diagnostic_file);
}
