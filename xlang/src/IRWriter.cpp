/**
 * xlang++/IRWriter.cpp
 * This file is part of libxlang, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  libxlang is additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

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


extern "C" {
    XLANG_EXPORT
    lccc::unique_ptr<lccc::xlang::FileVisitor>
        xlang_irwriter_main(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args) {

        return {};
    }
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
