#include <string>
#include <xlang++/Visit.hpp>
#include <xlang++/layout/UniquePtr.hpp>

#include "parse.hpp"
#include "preprocess.hpp"
#include "semantic.hpp"

using namespace std::string_view_literals;

namespace lccc::cc {

class CVisitor : public lccc::xlang::FileVisitor {
  private:
    std::unique_ptr<program> m_program;
  public:
    using lccc::xlang::FileVisitor::FileVisitor;
    virtual void visitInputFile(FILE *file) override;
    virtual void visitDiagnosticFile(FILE *file) override;
    virtual void visitEnd() override;
};

void CVisitor::visitInputFile(FILE *file) {
    std::string processed = preprocess(file); // Phase 1-6
    ast tree = parse(processed); // Phase 7 part 1
    m_program = std::make_unique<program>(analyze(std::move(tree))); // Phase 7 part 2
}

void CVisitor::visitDiagnosticFile(FILE *file) {

}

void CVisitor::visitEnd() {
    for(auto &&item : m_program->items()) {
        p_function &function = *dynamic_cast<p_function*>(&*item);
        lccc::xlang::ScopeMemberVisitor *sm = visitScopeMember();
        lccc::xlang::FunctionVisitor *fn = sm->visitFunction();
        lccc::xlang::FunctionTypeVisitor *types = fn->visitType();
        lccc::xlang::TypeVisitor *rt = types->visitReturnType();
        lccc::xlang::ScalarTypeVisitor *st = rt->visitScalarType();
        lccc::xlang::IntegerTypeVisitor *it = st->visitIntegerType();
        it->visitSigned(); it->visitEnd();
        st->visitBitSize(32); st->visitEnd();
        rt->visitEnd();
        types->visitEnd();
        fn->visitEnd(); // TODO: Actually have a body
        lccc::xlang::IdentifierVisitor *nm = sm->visitName();
        nm->visitComponent("main"sv); nm->visitEnd();
        sm->visitEnd();
    }
    this->lccc::xlang::FileVisitor::visitEnd();
}

extern "C" {
    XLANG_EXPORT lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_plugin_main(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args) {
        return lccc::make_unique<CVisitor>();
    }
}

}

