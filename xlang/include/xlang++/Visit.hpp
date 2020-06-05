//
// Created by chorm on 2020-06-05.
//

#ifndef XLANG_VISIT_HPP
#define XLANG_VISIT_HPP

#include <string>
#include <string_view>
#include <optional>
#include <memory>
#include <cstdint>
#include <functional>

namespace lccc::xlang{
    class Visitor{
    protected:
        Visitor()=default;
    public:
        virtual ~Visitor()=default;
        virtual void visitEnd()=0;
        virtual void visitDiagnostic(std::string_view)=0;
    };

    struct IdentifierVisitor : Visitor{
        virtual void visitRoot()=0;
        virtual void visitComponent(std::string_view)=0;
        virtual void visitSpecialComponent(std::string_view)=0;
    };

    struct AnnotationVisitor : Visitor{
        virtual AnnotationVisitor* visitMeta()=0;
        virtual IdentifierVisitor* visitIdentifier()=0;
        virtual void visitItem(std::string_view)=0;
        virtual void visitItem(std::uint64_t)=0;
    };

    struct AnnotatedElementVisitor : Visitor{
        virtual AnnotationVisitor* visitAnnotation()=0;
    };
    struct ScopeMemberVisitor;


    struct ScopeVisitor : AnnotatedElementVisitor{
        virtual ScopeMemberVisitor* visitScopeMember()=0;
    };
    enum class Visibility : std::uint16_t {
        Public,
        Origin,
        Module,
        Private,
        None
    };

    struct GenericDeclarationVisitor;
    struct TypeAliasVisitor;
    struct GenericParameterVisitor;
    struct ExprVisitor;

    struct ScopeMemberVisitor : AnnotatedElementVisitor{
        virtual void visitVisibility(Visibility)=0;
        virtual IdentifierVisitor* visitName()=0;
        virtual void visitStaticAssertion(std::function<void(ExprVisitor&)>,std::string_view)=0;
        virtual TypeAliasVisitor* visitTypeAlias(std::function<void(IdentifierVisitor&)>)=0;
        virtual GenericDeclarationVisitor* visitGenericDeclaration()=0;
    };

    struct GenericDeclarationVisitor : ScopeMemberVisitor{
        virtual GenericParameterVisitor* visitGenericParameter()=0;
    };
}

#endif //XLANG_VISIT_HPP
