#include <xlang++/Layout.h>
#include <xlang++/Visit.hpp>
#include <vector>
#include <memory>
#include <variant>

namespace lccc::xlang{

    struct IRScopeWriter : ScopeVisitor{
        std::vector<std::unique_ptr<struct IRScopeMember>> members;

        using ScopeVisitor::ScopeVisitor;
        ~IRScopeWriter();

        ScopeMemberVisitor* visitScopeMember();
    };
    struct IRScopeMember : ScopeMemberVisitor{
        using ScopeMemberVisitor::ScopeMemberVisitor;
        
    };

    IRScopeWriter::~IRScopeWriter()=default;

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
}
