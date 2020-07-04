//
// Created by chorm on 2020-06-05.
//

#include <xlang++/Visit.hpp>

namespace lccc::xlang{
    Visitor::Visitor(Visitor* other):other{other}{}

    void Visitor::visitEnd() {
        if(other)
            other->visitEnd();
    }
    void Visitor::visitDiagnostic(std::string_view sv) {
        if(other)
            other->visitDiagnostic(sv);
    }

    IdentifierVisitor::IdentifierVisitor(IdentifierVisitor *other) : Visitor{other} {

    }

    void IdentifierVisitor::visitRoot() {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitRoot();
    }

    void IdentifierVisitor::visitComponent(std::string_view name) {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitComponent(name);
    }

    void IdentifierVisitor::visitSpecialComponent(std::string_view name) {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitSpecialComponent(name);
    }

    AnnotationVisitor::AnnotationVisitor(AnnotationVisitor *other) : Visitor{other} {

    }

    IdentifierVisitor *AnnotationVisitor::visitIdentifier() {
        if(auto* id = this->get_parent<AnnotationVisitor>();id)
            return id->visitIdentifier();
        else
            return nullptr;
    }

    AnnotationVisitor *AnnotationVisitor::visitMeta() {
        if(auto* id = this->get_parent<AnnotationVisitor>();id)
            return id->visitMeta();
        else
            return nullptr;
    }

    void AnnotationVisitor::visitItem(std::string_view value) {
        if(auto* id = this->get_parent<AnnotationVisitor>();id)
            id->visitItem(value);
    }

    void AnnotationVisitor::visitItem(std::uint64_t value) {
        if(auto* id = this->get_parent<AnnotationVisitor>();id)
            id->visitItem(value);
    }

    AnnotatedElementVisitor::AnnotatedElementVisitor(AnnotatedElementVisitor *other) : Visitor{other} {}

    AnnotationVisitor *AnnotatedElementVisitor::visitAnnotation() {
        if(auto* id = this->get_parent<AnnotatedElementVisitor>();id)
            return id->visitAnnotation();
        else
            return nullptr;
    }

    ScopeVisitor::ScopeVisitor(ScopeVisitor *other) : Visitor{other} {

    }

    ScopeMemberVisitor *ScopeVisitor::visitScopeMember() {
        if(auto* id = this->get_parent<ScopeVisitor>();id)
            return id->visitScopeMember();
        else
            return nullptr;
    }

    ScopeMemberVisitor::ScopeMemberVisitor(ScopeMemberVisitor *other) : Visitor{other} {}

    void ScopeMemberVisitor::visitVisibility(Visibility visibility) {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            id->visitVisibility(visibility);
    }

    IdentifierVisitor *ScopeMemberVisitor::visitName() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitName();
        else
            return nullptr;
    }

    void ScopeMemberVisitor::visitStaticAssertion(const std::function<void(ExprVisitor &)>& fn, std::string_view diagnostic) {
        if(auto* member = this->get_parent<ScopeMemberVisitor>();member)
            member->visitStaticAssertion(fn,diagnostic);
    }

    TypeAliasVisitor *ScopeMemberVisitor::visitTypeAlias() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitTypeAlias();
        else
            return nullptr;
    }

    GenericMemberVisitor *ScopeMemberVisitor::visitGenericDeclaration() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitGenericDeclaration();
        else
            return nullptr;
    }


    GenericDeclarationVisitor::GenericDeclarationVisitor(GenericDeclarationVisitor *parent) : Visitor{parent} {}

    GenericParameterVisitor *GenericDeclarationVisitor::visitGenericParameter() {
        if(auto* id = this->get_parent<GenericDeclarationVisitor>();id)
            return id->visitGenericParameter();
        else
            return nullptr;
    }

    GenericMemberVisitor::GenericMemberVisitor(GenericMemberVisitor *parent) : Visitor(parent) {}

    GenericParameterVisitor::GenericParameterVisitor(GenericParameterVisitor *parent) {

    }
}
