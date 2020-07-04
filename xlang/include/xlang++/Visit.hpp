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
#include <type_traits>

namespace lccc::xlang{
    class Visitor{
    private:
        Visitor* other;
    protected:
        template<typename T,std::enable_if_t<std::is_base_of_v<Visitor,std::remove_cv_t<T>>>* = nullptr>
            T* get_parent(){
                return dynamic_cast<T*>(other);
            }
    public:
        explicit Visitor(Visitor* other=nullptr);
        virtual ~Visitor()=default;
        virtual void visitEnd();
        virtual void visitDiagnostic(std::string_view);
    };

    struct IdentifierVisitor : virtual Visitor{
        explicit IdentifierVisitor(IdentifierVisitor* other=nullptr);
        virtual void visitRoot();
        virtual void visitComponent(std::string_view);
        virtual void visitSpecialComponent(std::string_view);
    };

    struct AnnotationVisitor : virtual Visitor{
        explicit AnnotationVisitor(AnnotationVisitor* other=nullptr);
        virtual AnnotationVisitor* visitMeta();
        virtual IdentifierVisitor* visitIdentifier();
        virtual void visitItem(std::string_view);
        virtual void visitItem(std::uint64_t);
    };

    struct AnnotatedElementVisitor : virtual Visitor{
        explicit AnnotatedElementVisitor(AnnotatedElementVisitor* other=nullptr);
        virtual AnnotationVisitor* visitAnnotation();
    };
    struct ScopeMemberVisitor;


    struct ScopeVisitor : virtual AnnotatedElementVisitor{
        explicit ScopeVisitor(ScopeVisitor* other=nullptr);
        virtual ScopeMemberVisitor* visitScopeMember();
    };
    enum class Visibility : std::uint16_t {
        Public,
        Origin,
        Module,
        Private,
        None
    };

    struct GenericDeclarationVisitor;
    struct TypeVisitor;
    struct TypeAliasVisitor;
    struct GenericParameterVisitor;
    struct ExprVisitor;
    struct GenericMemberVisitor;
    struct GenericItemVisitor;

    struct ScopeMemberVisitor : virtual AnnotatedElementVisitor{
    public:
        explicit ScopeMemberVisitor(ScopeMemberVisitor* other=nullptr);
        virtual void visitVisibility(Visibility);
        virtual IdentifierVisitor* visitName();
        virtual void visitStaticAssertion(const std::function<void(ExprVisitor&)>&,std::string_view);
        virtual TypeAliasVisitor* visitTypeAlias();
        virtual GenericMemberVisitor* visitGenericDeclaration();
    };

    struct GenericDeclarationVisitor : virtual Visitor{
        explicit GenericDeclarationVisitor(GenericDeclarationVisitor* parent=nullptr);
        virtual GenericParameterVisitor* visitGenericParameter();
    };

    struct GenericMemberVisitor : virtual GenericDeclarationVisitor, virtual ScopeMemberVisitor{
        explicit GenericMemberVisitor(GenericMemberVisitor* parent=nullptr);
    };

    struct TypeGenericParameterVisitor;
    struct ConstGenericParameterVisitor;

    struct GenericParameterVisitor : virtual AnnotatedElementVisitor{
        explicit GenericParameterVisitor(GenericParameterVisitor* parent=nullptr);
        virtual TypeGenericParameterVisitor* visitTypeParameter();
        virtual TypeVisitor* visitConstParameter();
        virtual GenericDeclarationVisitor* visitGenericType();
        virtual void visitParameterPack();
        virtual IdentifierVisitor* visitName();
        virtual TypeVisitor* visitDefaultType();
        virtual ExprVisitor* visitDefaultValue();
        virtual GenericItemVisitor* visitDefaultGenericType();
    };
}

#endif //XLANG_VISIT_HPP
