/**
 * xlang++/Visit.cpp
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

#include <xlang++/Visit.hpp>

namespace lccc::xlang{
    Visitor::Visitor(lccc::optional<Visitor&> other):other{other}{}

    void Visitor::visitEnd() {
        if(other)
            other->visitEnd();
    }
    void Visitor::visitDiagnostic(lccc::string_view sv) {
        if(other)
            other->visitDiagnostic(sv);
    }

    void Visitor::visitLine(std::uint64_t ln) {
        if(other)
            other->visitLine(ln);
    }

    void Visitor::visitSourceFile(lccc::string_view name) {
        if(other)
            other->visitSourceFile(name);
    }

    PathVisitor::PathVisitor(lccc::optional<PathVisitor&> other) : Visitor{other} {

    }

    void PathVisitor::visitRoot() {
        if(auto id = this->get_parent<PathVisitor>();id)
            id->visitRoot();
    }

    void PathVisitor::visitComponent(lccc::string_view name) {
        if(auto id = this->get_parent<PathVisitor>();id)
            id->visitComponent(name);
    }

    void PathVisitor::visitSpecialComponent(lccc::string_view name) {
        if(auto id = this->get_parent<PathVisitor>();id)
            id->visitSpecialComponent(name);
    }

    lccc::optional<TypeVisitor&> PathVisitor::visitDependentName() {
        if(auto parent = this->get_parent<PathVisitor>();parent)
            return parent->visitDependentName();
        else
            return lccc::nullopt;
    }

    lccc::optional<GenericInstantiationVisitor&> PathVisitor::visitGenericArgs() {
        if(auto parent = this->get_parent<PathVisitor>();parent)
            return parent->visitGenericArgs();
        else
            return lccc::nullopt;
    }

    AnnotationVisitor::AnnotationVisitor(lccc::optional<AnnotationVisitor&> other) : Visitor{other} {

    }

    lccc::optional<PathVisitor&> AnnotationVisitor::visitIdentifier() {
        if(auto id = this->get_parent<AnnotationVisitor>();id)
            return id->visitIdentifier();
        else
            return lccc::nullopt;
    }

    lccc::optional<AnnotationVisitor&> AnnotationVisitor::visitMeta() {
        if(auto id = this->get_parent<AnnotationVisitor>();id)
            return id->visitMeta();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> AnnotationVisitor::visitItem() {
        if(auto parent = this->get_parent<AnnotationVisitor>();parent)
            return parent->visitItem();
        else
            return lccc::nullopt;
    }


    AnnotatedElementVisitor::AnnotatedElementVisitor(lccc::optional<AnnotatedElementVisitor&> other) : Visitor{other} {}

    lccc::optional<AnnotationVisitor&> AnnotatedElementVisitor::visitAnnotation() {
        if(auto id = this->get_parent<AnnotatedElementVisitor>();id)
            return id->visitAnnotation();
        else
            return lccc::nullopt;
    }

    ScopeVisitor::ScopeVisitor(lccc::optional<ScopeVisitor&> other) : AnnotatedElementVisitor{other} {

    }

    lccc::optional<ScopeMemberVisitor&> ScopeVisitor::visitScopeMember() {
        if(auto id = this->get_parent<ScopeVisitor>();id)
            return id->visitScopeMember();
        else
            return lccc::nullopt;
    }

    lccc::optional<GenericInstantiationVisitor&> ScopeVisitor::visitExplicitInstantiation() {
        if(auto parent = this->get_parent<ScopeVisitor>();parent)
            return parent->visitExplicitInstantiation();
        else
            return lccc::nullopt;
    }

    ScopeMemberVisitor::ScopeMemberVisitor(lccc::optional<ScopeMemberVisitor&> other) : AnnotatedElementVisitor{other} {}

    void ScopeMemberVisitor::visitVisibility(Visibility visibility) {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            id->visitVisibility(visibility);
    }

    lccc::optional<PathVisitor&> ScopeMemberVisitor::visitName() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitName();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor &> ScopeMemberVisitor::visitStaticAssertion(lccc::string_view diagnostic) {
        if(auto member = this->get_parent<ScopeMemberVisitor>();member)
            return member->visitStaticAssertion(diagnostic);
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> ScopeMemberVisitor::visitTypeAlias() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitTypeAlias();
        else
            return lccc::nullopt;
    }

    lccc::optional<GenericMemberVisitor&> ScopeMemberVisitor::visitGenericDeclaration() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitGenericDeclaration();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeDefinitionVisitor&> ScopeMemberVisitor::visitTypeDefinition() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitTypeDefinition();
        else
            return lccc::nullopt;
    }

    lccc::optional<GlobalVariableVisitor&> ScopeMemberVisitor::visitGlobalVariable() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitGlobalVariable();
        else
            return lccc::nullopt;
    }

    lccc::optional<FunctionVisitor&> ScopeMemberVisitor::visitFunction() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitFunction();
        else
            return lccc::nullopt;
    }

    lccc::optional<ScopeVisitor&> ScopeMemberVisitor::visitScope() {
        if(auto id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitScope();
        else
            return lccc::nullopt;
    }


    void ScopeMemberVisitor::visitExternalScope() {
        if(auto parent = this->get_parent<ScopeMemberVisitor>();parent)
            this->visitExternalScope();
    }


    GenericDeclarationVisitor::GenericDeclarationVisitor(lccc::optional<GenericDeclarationVisitor&> parent) : Visitor{parent} {}

    lccc::optional<GenericParameterVisitor&> GenericDeclarationVisitor::visitGenericParameter() {
        if(auto id = this->get_parent<GenericDeclarationVisitor>();id)
            return id->visitGenericParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> GenericDeclarationVisitor::visitRequiresClause() {
        if(auto id = this->get_parent<GenericDeclarationVisitor>();id)
            return id->visitRequiresClause();
        else
            return lccc::nullopt;
    }

    GenericMemberVisitor::GenericMemberVisitor(lccc::optional<GenericMemberVisitor&> parent) : GenericDeclarationVisitor(parent) {}

    lccc::optional<ScopeMemberVisitor&> GenericMemberVisitor::visit() {
        if(auto id = this->get_parent<GenericMemberVisitor>();id)
            return id->visit();
        else
            return lccc::nullopt;
    }

    GenericParameterVisitor::GenericParameterVisitor(lccc::optional<GenericParameterVisitor&> parent) : AnnotatedElementVisitor(parent) {}

    lccc::optional<TypeGenericParameterVisitor&> GenericParameterVisitor::visitTypeParameter() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitTypeParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<ConstGenericParameterVisitor&> GenericParameterVisitor::visitConstParameter() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitConstParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<BoundGenericParameterVisitor&> GenericParameterVisitor::visitBoundParameter() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitBoundParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<GenericDeclarationVisitor&> GenericParameterVisitor::visitGenericType() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitGenericType();
        else
            return lccc::nullopt;
    }

    void GenericParameterVisitor::visitParameterPack() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            id->visitParameterPack();
    }

    lccc::optional<TypeVisitor&> GenericParameterVisitor::visitDefaultType() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultType();
        else
            return lccc::nullopt;
    }

    // Why is this an ExprVisitor?
    lccc::optional<ExprVisitor&> GenericParameterVisitor::visitDefaultValue() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultValue();
        else
            return lccc::nullopt;
    }

    lccc::optional<PathVisitor&> GenericParameterVisitor::visitDefaultGenericType() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultGenericType();
        else
            return lccc::nullopt;
    }

    lccc::optional<BoundVisitor&> GenericParameterVisitor::visitDefaultBound() {
        if(auto id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultBound();
        else
            return lccc::nullopt;
    }


    TypeVisitor::TypeVisitor(lccc::optional<TypeVisitor&> parent) : AnnotatedElementVisitor{parent} {}

    lccc::optional<ScalarTypeVisitor&> TypeVisitor::visitScalarType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitScalarType();
        else
            return lccc::nullopt;
    }

    lccc::optional<PointerTypeVisitor&> TypeVisitor::visitPointerType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitPointerType();
        else
            return lccc::nullopt;
    }

    lccc::optional<PathVisitor&> TypeVisitor::visitNamedType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitNamedType();
        else
            return lccc::nullopt;
    }

    lccc::optional<GenericInstantiationVisitor&> TypeVisitor::visitGenericType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitGenericType();
        else
            return lccc::nullopt;
    }

    void TypeVisitor::visitGenericParameter(uint32_t pnum) {
        if(auto id = this->get_parent<TypeVisitor>();id)
            id->visitGenericParameter(pnum);
    }

    lccc::optional<ValueVisitor&> TypeVisitor::visitAlignedAs() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitAlignedAs();
        else
            return lccc::nullopt;
    }

    lccc::optional<ProductTypeVisitor&> TypeVisitor::visitProductType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitProductType();
        else
            return lccc::nullopt;
    }

    lccc::optional<SumTypeVisitor&> TypeVisitor::visitSumType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitSumType();
        else
            return lccc::nullopt;
    }

    lccc::optional<FunctionTypeVisitor&> TypeVisitor::visitFunctionType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitFunctionType();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeDefinitionVisitor&> TypeVisitor::visitElaboratedType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitElaboratedType();
        else
            return lccc::nullopt;
    }

    lccc::optional<ConceptVisitor&> TypeVisitor::visitErasedConceptType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitErasedConceptType();
        else
            return lccc::nullopt;
    }

    lccc::optional<ConceptVisitor&> TypeVisitor::visitReifiedConceptType() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitReifiedConceptType();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> TypeVisitor::visitSlice() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitSlice();
        else
            return lccc::nullopt;
    }

    lccc::optional<ArrayTypeVisitor&> TypeVisitor::visitArray() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitArray();
        else
            return lccc::nullopt;
    }

    lccc::optional<ExprVisitor&> TypeVisitor::visitDecltype() {
        if(auto id = this->get_parent<TypeVisitor>();id)
            return id->visitDecltype();
        else
            return lccc::nullopt;
    }

    void TypeVisitor::visitVoid(){
        if(auto id = this->get_parent<TypeVisitor>();id)
            id->visitVoid();
    }

    GenericInstantiationVisitor::GenericInstantiationVisitor(lccc::optional<GenericInstantiationVisitor&> visitor) : Visitor(visitor) {}

    lccc::optional<PathVisitor&> GenericInstantiationVisitor::visitGenericItem() {
        if(auto id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitGenericItem();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> GenericInstantiationVisitor::visitTypeParameter() {
        if(auto id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitTypeParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<PathVisitor&> GenericInstantiationVisitor::visitGenericParameter() {
        if(auto id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitGenericParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> GenericInstantiationVisitor::visitConstParameter() {
        if(auto id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitConstParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<BoundVisitor&> GenericInstantiationVisitor::visitBoundParameter() {
        if(auto id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitBoundParameter();
        else
            return lccc::nullopt;
    }

    PointerTypeVisitor::PointerTypeVisitor(lccc::optional<PointerTypeVisitor&> parent) : Visitor{parent} {}

    lccc::optional<TypeVisitor&> PointerTypeVisitor::visitPointeeType() {
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitPointeeType();
        else
            return lccc::nullopt;
    }

    void PointerTypeVisitor::visitAliasingRule(PointerAliasingRule aliasingRule) {
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            visitAliasingRule(aliasingRule);
    }

    lccc::optional<ValueVisitor&> PointerTypeVisitor::visitValidRange(ValidRangeType validRange) {
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitValidRange(validRange);
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> PointerTypeVisitor::visitAligned() {
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitAligned();
        else
            return lccc::nullopt;
    }

    lccc::optional<BoundVisitor&> PointerTypeVisitor::visitRequiredBounds() {
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitRequiredBounds();
        else
            return lccc::nullopt;
    }

    void PointerTypeVisitor::visitDefinitionType(PointerDefinitionType type){
        if(auto id = this->get_parent<PointerTypeVisitor>();id)
            id->visitDefinitionType(type);
    }

    ScalarTypeVisitor::ScalarTypeVisitor(lccc::optional<ScalarTypeVisitor&> parent) : Visitor{parent} {}

    lccc::optional<IntegerTypeVisitor&> ScalarTypeVisitor::visitIntegerType() {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            return id->visitIntegerType();
        else
            return lccc::nullopt;
    }

    lccc::optional<FloatTypeVisitor&> ScalarTypeVisitor::visitFloatingPointType() {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            return id->visitFloatingPointType();
        else
            return lccc::nullopt;
    }

    lccc::optional<FixedTypeVisitor&> ScalarTypeVisitor::visitFixedPointType() {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            return id->visitFixedPointType();
        else
            return lccc::nullopt;
    }

    void ScalarTypeVisitor::visitBitSize(uint16_t bits) {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitBitSize(bits);
    }

    void ScalarTypeVisitor::visitVectorSize(uint16_t vector) {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitVectorSize(vector);
    }

    IntegerTypeVisitor::IntegerTypeVisitor(lccc::optional<IntegerTypeVisitor&> parent) : Visitor{parent} {}

    void IntegerTypeVisitor::visitSigned() {
        if(auto id = this->get_parent<IntegerTypeVisitor>();id)
            id->visitSigned();
    }

    lccc::optional<ValueVisitor&> IntegerTypeVisitor::visitMinimumValue() {
        if(auto id = this->get_parent<IntegerTypeVisitor>();id)
            return id->visitMinimumValue();
        else
            return lccc::nullopt;
    }
    lccc::optional<ValueVisitor&> IntegerTypeVisitor::visitMaximumValue() {
        if(auto id = this->get_parent<IntegerTypeVisitor>();id)
            return id->visitMaximumValue();
        else
            return lccc::nullopt;
    }

    ProductTypeVisitor::ProductTypeVisitor(lccc::optional<ProductTypeVisitor&> parent) : Visitor{parent} {}

    lccc::optional<TypeVisitor&> ProductTypeVisitor::visitType() {
        if(auto id = this->get_parent<ProductTypeVisitor>();id)
            return id->visitType();
        else
            return lccc::nullopt;
    }

    SumTypeVisitor::SumTypeVisitor(lccc::optional<SumTypeVisitor&> parent) : Visitor{parent} {}

    lccc::optional<TypeVisitor&> SumTypeVisitor::visitType() {
        if(auto id = this->get_parent<SumTypeVisitor>();id)
            return id->visitType();
        else
            return lccc::nullopt;
    }

    FloatTypeVisitor::FloatTypeVisitor(lccc::optional<FloatTypeVisitor&> vparent) : Visitor{vparent} {}

    void FloatTypeVisitor::visitDecimalFloat() {
        if(auto id = this->get_parent<FloatTypeVisitor>();id)
            id->visitDecimalFloat();
    }

    FixedTypeVisitor::FixedTypeVisitor(lccc::optional<FixedTypeVisitor&> vparent) : Visitor{vparent}{}

    void FixedTypeVisitor::visitFractBits(std::uint16_t fbits) {
        if(auto id = this->get_parent<FixedTypeVisitor>(); id)
            id->visitFractBits(fbits);
    }

    void ScalarTypeVisitor::visitComplex() {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitComplex();
    }

    void ScalarTypeVisitor::visitValueValidity(ScalarValidity validity) {
        if(auto id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitValueValidity(validity);
    }

    FunctionTypeVisitor::FunctionTypeVisitor(lccc::optional<FunctionTypeVisitor&> parent) : Visitor{parent} {

    }

    lccc::optional<TypeVisitor&> FunctionTypeVisitor::visitReturnType() {
        if(auto visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitReturnType();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> FunctionTypeVisitor::visitParameterType() {
        if(auto visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitParameterType();
        else
            return lccc::nullopt;
    }

    void FunctionTypeVisitor::visitVarargs() {
        if(auto visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            visitor->visitVarargs();
    }

    void FunctionTypeVisitor::visitLinkage(lccc::string_view lit) {
        if(auto visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            visitor->visitLinkage(lit);

    }


    BoundVisitor::BoundVisitor(lccc::optional<BoundVisitor&> vparent) : Visitor{vparent} {

    }

    void BoundVisitor::visitGenericBound(std::uint32_t param) {
        if(auto visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitGenericBound(param);

    }

    void BoundVisitor::visitStatic() {
        if(auto visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitStatic();
    }

    void BoundVisitor::visitToken(std::uint64_t token) {
        if(auto visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitToken(token);
    }

    BoundGenericParameterVisitor::BoundGenericParameterVisitor(lccc::optional<BoundGenericParameterVisitor&> visitor) : Visitor{visitor} {

    }

    lccc::optional<BoundVisitor&> BoundGenericParameterVisitor::visitEnclosedBy() {
        if(auto visitor=this->get_parent<BoundGenericParameterVisitor>();visitor)
            return visitor->visitEnclosedBy();
        else
            return lccc::nullopt;
    }

    ValueVisitor::ValueVisitor(lccc::optional<ValueVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<ConstantVisitor&> ValueVisitor::visitConstantValue() {
        if(auto parent =this->get_parent<ValueVisitor>();parent)
            return parent->visitConstantValue();
        else
            return lccc::nullopt;
    }

    lccc::optional<ExprVisitor&> ValueVisitor::visitExpression() {
        if(auto parent =this->get_parent<ValueVisitor>();parent)
            return parent->visitExpression();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> ValueVisitor::visitUndefined(UndefinedValueKind kind) {
        if(auto parent = this->get_parent<ValueVisitor>();parent)
            return parent->visitUndefined(kind);
        else
            return lccc::nullopt;
    }

    void ValueVisitor::visitParameter(std::uint32_t num){
        if(auto parent = this->get_parent<ValueVisitor>();parent)
            return parent->visitParameter(num);

    }


    ConstantVisitor::ConstantVisitor(lccc::optional<ConstantVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<TypeVisitor&> ConstantVisitor::visitInteger(std::intmax_t value) {
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitInteger(value);
        else
            return lccc::nullopt;
    }

    lccc::optional<StringLiteralVisitor&> ConstantVisitor::visitStringLiteral() {
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitStringLiteral();
        else
            return lccc::nullopt;
    }


    lccc::optional<PointerConstantVisitor&> ConstantVisitor::visitPointerConstant() {
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitPointerConstant();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> ConstantVisitor::visitExcessValueInteger(lccc::span<const uint8_t> bytes) {
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitExcessValueInteger(bytes);
        else
            return lccc::nullopt;
    }

    lccc::optional<ArrayConstantVisitor&> ConstantVisitor::visitConstantArray() {
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitConstantArray();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> ConstantVisitor::visitSizeOf(){
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitSizeOf();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> ConstantVisitor::visitAlignOf(){
        if(auto parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitAlignOf();
        else
            return lccc::nullopt;
    }

    PointerConstantVisitor::PointerConstantVisitor(lccc::optional<PointerConstantVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<PointerTypeVisitor&> PointerConstantVisitor::visitType() {
        if(auto parent = this->get_parent<PointerConstantVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    void PointerConstantVisitor::visitNullPointer() {
        if(auto parent = this->get_parent<PointerConstantVisitor>();parent)
            parent->visitNullPointer();
    }

    lccc::optional<PathVisitor&> PointerConstantVisitor::visitGlobalAddress() {
        if(auto parent = this->get_parent<PointerConstantVisitor>();parent)
            return parent->visitGlobalAddress();
        else
            return lccc::nullopt;
    }

    void PointerConstantVisitor::visitLabelAddress(std::uint32_t label) {
        if(auto parent = this->get_parent<PointerConstantVisitor>();parent)
            parent->visitLabelAddress(label);
    }

    StringLiteralVisitor::StringLiteralVisitor(lccc::optional<StringLiteralVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<TypeVisitor&> StringLiteralVisitor::visitType() {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    void StringLiteralVisitor::visitByteString(lccc::string_view value) {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitByteString(value);
    }
    void StringLiteralVisitor::visitUTF8String(lccc::string_view value) {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF8String(value);
    }
    void StringLiteralVisitor::visitUTF16String(lccc::u16string_view value) {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF16String(value);
    }
    void StringLiteralVisitor::visitUTF32String(lccc::u32string_view value) {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF32String(value);
    }
    void StringLiteralVisitor::visitWideString(lccc::wstring_view value) {
        if(auto parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitWideString(value);
    }

    TypeDefinitionVisitor::TypeDefinitionVisitor(lccc::optional<TypeDefinitionVisitor&> visitor) : AnnotatedElementVisitor{visitor} {

    }

    lccc::optional<StructTypeVisitor&> TypeDefinitionVisitor::visitStruct() {
        if(auto parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitStruct();
        else
            return lccc::nullopt;
    }

    lccc::optional<EnumTypeVisitor&> TypeDefinitionVisitor::visitEnum() {
        if(auto parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitEnum();
        else
            return lccc::nullopt;
    }

    lccc::optional<StructTypeVisitor&> TypeDefinitionVisitor::visitUnion() {
        if(auto parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitUnion();
        else
            return lccc::nullopt;
    }


    StructTypeVisitor::StructTypeVisitor(lccc::optional<StructTypeVisitor&> vparent) : AnnotatedElementVisitor{vparent} {

    }

    lccc::optional<StructFieldVisitor&> StructTypeVisitor::visitStructField(Visibility vis) {
        if(auto parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitStructField(vis);
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> StructTypeVisitor::visitBaseClass(Visibility vis) {
        if(auto parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitBaseClass(vis);
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> StructTypeVisitor::visitVirtualBaseClass(Visibility vis) {
        if(auto parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitVirtualBaseClass(vis);
        else
            return lccc::nullopt;
    }

    EnumTypeVisitor::EnumTypeVisitor(lccc::optional<EnumTypeVisitor&> vparent) : AnnotatedElementVisitor{vparent} {

    }

    void EnumTypeVisitor::visitStrong() {
        if(auto parent = this->get_parent<EnumTypeVisitor>();parent)
            parent->visitStrong();
    }

    lccc::optional<TypeVisitor&> EnumTypeVisitor::visitUnderlyingType() {
        if(auto parent = this->get_parent<EnumTypeVisitor>();parent)
            return parent->visitUnderlyingType();
        else
            return lccc::nullopt;
    }

    lccc::optional<EnumeratorVisitor&> EnumTypeVisitor::visitEnumerator() {
        if(auto parent = this->get_parent<EnumTypeVisitor>();parent)
            return parent->visitEnumerator();
        else
            return lccc::nullopt;
    }

    StructFieldVisitor::StructFieldVisitor(lccc::optional<StructFieldVisitor&> vparent) : AnnotatedElementVisitor{vparent} {

    }

    lccc::optional<PathVisitor&> StructFieldVisitor::visitName() {
        if(auto parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitName();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> StructFieldVisitor::visitType() {
        if(auto parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> StructFieldVisitor::visitBitFieldLength() {
        if(auto parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitBitFieldLength();
        else
            return lccc::nullopt;
    }

    EnumeratorVisitor::EnumeratorVisitor(lccc::optional<EnumeratorVisitor&> vparent) : AnnotatedElementVisitor(vparent) {

    }

    lccc::optional<PathVisitor&> EnumeratorVisitor::visitName() {
        if(auto parent = this->get_parent<EnumeratorVisitor>();parent)
            return parent->visitName();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> EnumeratorVisitor::visitValue() {
        if(auto parent = this->get_parent<EnumeratorVisitor>();parent)
            return parent->visitValue();
        else
            return lccc::nullopt;
    }


    TypeGenericParameterVisitor::TypeGenericParameterVisitor(lccc::optional<TypeGenericParameterVisitor&> vparent) : AnnotatedElementVisitor{vparent} {

    }

    lccc::optional<ConceptVisitor&> TypeGenericParameterVisitor::visitConcept() {
        if(auto parent = this->get_parent<TypeGenericParameterVisitor>();parent)
            return parent->visitConcept();
        else
            return lccc::nullopt;
    }

    ConstGenericParameterVisitor::ConstGenericParameterVisitor(lccc::optional<ConstGenericParameterVisitor&> vparent) : AnnotatedElementVisitor{vparent} {

    }

    lccc::optional<TypeVisitor&> ConstGenericParameterVisitor::visitType() {
        if(auto parent = this->get_parent<ConstGenericParameterVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    ArrayTypeVisitor::ArrayTypeVisitor(lccc::optional<ArrayTypeVisitor&> parent) : Visitor{parent} {

    }

    lccc::optional<TypeVisitor&> ArrayTypeVisitor::visitComponentType() {
        if(auto parent = this->get_parent<ArrayTypeVisitor>();parent)
            return parent->visitComponentType();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> ArrayTypeVisitor::visitExtent() {
        if(auto parent = this->get_parent<ArrayTypeVisitor>();parent)
            return parent->visitExtent();
        else
            return lccc::nullopt;
    }

    ArrayConstantVisitor::ArrayConstantVisitor(lccc::optional<ArrayConstantVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<TypeVisitor&> ArrayConstantVisitor::visitType() {
        if(auto parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> ArrayConstantVisitor::visitRepeatLength() {
        if(auto parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitRepeatLength();
        else
            return lccc::nullopt;
    }

    lccc::optional<ValueVisitor&> ArrayConstantVisitor::visitValue() {
        if(auto parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitValue();
        else
            return lccc::nullopt;
    }

    ExprVisitor::ExprVisitor(lccc::optional<ExprVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<ValueVisitor&> ExprVisitor::visitConst() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitConst();
        else
            return lccc::nullopt;
    }

    lccc::optional<FunctionTypeVisitor&> ExprVisitor::visitFunctionCall() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitFunctionCall();
        else
            return lccc::nullopt;
    }

    lccc::optional<PathVisitor&> ExprVisitor::visitMember(MemberAccessType type) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitMember(type);
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitLocal(uint32_t var) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitLocal(var);
    }

    void ExprVisitor::visitAsRvalue(AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitAsRvalue(cl);
    }

    lccc::optional<TypeVisitor&> ExprVisitor::visitConversion(ConversionStrength st) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitConversion(st);
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitUnaryOperator(UnaryOperation op) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitUnaryOperator(op);
    }

    void ExprVisitor::visitBinaryOperator(BinaryOperation op) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitBinaryOperator(op);
    }

    void ExprVisitor::visitIndirection() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitIndirection();
    }

    lccc::optional<BoundVisitor&> ExprVisitor::visitLock(PointerSharing type) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitLock(type);
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitPointerTo() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitPointerTo();
    }

    lccc::optional<PointerTypeVisitor&> ExprVisitor::visitDerive() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitDerive();
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitSequence(AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitSequence(cl);
    }

    void ExprVisitor::visitFence(AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitFence(cl);
    }

    void ExprVisitor::visitAssignment(AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitAssignment(cl);
    }

    void ExprVisitor::visitCompoundAssignment(BinaryOperation op, AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitCompoundAssignment(op,cl);
    }

    void ExprVisitor::visitLValue(LValueOperation op, AccessClass cl) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitLValue(op,cl);
    }

    void ExprVisitor::visitDestroy() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitDestroy();
    }

    void ExprVisitor::pop(uint8_t cnt) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->pop(cnt);
    }

    void ExprVisitor::dup(uint8_t cnt) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->dup(cnt);
    }

    lccc::optional<BlockVisitor&> ExprVisitor::visitBlock() {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitBlock();
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitTupleExpression(uint16_t values) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitTupleExpression(values);
    }

    lccc::optional<TypeVisitor&> ExprVisitor::visitAggregateConstruction(uint16_t values) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitAggregateConstruction(values);
        else
            return lccc::nullopt;
    }

    void ExprVisitor::visitBlockExit(uint32_t blk, uint8_t values) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            parent->visitBlockExit(blk,values);
    }

    lccc::optional<LambdaVisitor&> ExprVisitor::visitLambdaExpression(uint16_t captures) {
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitLambdaExpression(captures);
        else
            return lccc::nullopt;
    }

    lccc::optional<StackItemsVisitor&> ExprVisitor::visitValidateStack(){
        if(auto parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitValidateStack();
        else
            return lccc::nullopt;
    }

    StackItemsVisitor::StackItemsVisitor(lccc::optional<StackItemsVisitor&> visitor) : Visitor{visitor} {

    }

    lccc::optional<TypeVisitor&> StackItemsVisitor::visitLvalue() {
        if(auto parent = this->get_parent<StackItemsVisitor>();parent)
            return parent->visitLvalue();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> StackItemsVisitor::visitRvalue() {
        if(auto parent = this->get_parent<StackItemsVisitor>();parent)
            return parent->visitRvalue();
        else
            return lccc::nullopt;
    }


    CaseVisitor::CaseVisitor(lccc::optional<CaseVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<ValueVisitor&> CaseVisitor::visitValue() {
        if(auto parent = this->get_parent<CaseVisitor>();parent)
            return parent->visitValue();
        else
            return lccc::nullopt;
    }

    void CaseVisitor::visitTarget(std::uint32_t item) {
        if(auto parent = this->get_parent<CaseVisitor>();parent)
            parent->visitTarget(item);
    }

    void SwitchVisitor::visitDefault(std::uint32_t item) {
        if(auto parent = this->get_parent<SwitchVisitor>();parent)
            parent->visitDefault(item);
    }

    lccc::optional<CaseVisitor&> SwitchVisitor::visitCase() {
        if(auto parent = this->get_parent<SwitchVisitor>();parent)
            return parent->visitCase();
        else
            return lccc::nullopt;
    }

    SwitchVisitor::SwitchVisitor(lccc::optional<SwitchVisitor&> vparent) : Visitor{vparent} {

    }

    BlockVisitor::BlockVisitor(lccc::optional<BlockVisitor&> parent) : ScopeVisitor{parent} {

    }

    lccc::optional<ExprVisitor&> BlockVisitor::visitExpression() {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitExpression();
        else
            return lccc::nullopt;
    }

    lccc::optional<StackItemsVisitor&> BlockVisitor::visitTarget(std::uint32_t item) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitTarget(item);
        else
            return lccc::nullopt;
    }

    void BlockVisitor::visitBeginTag(std::uint32_t item) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBeginTag(item);
    }

    void BlockVisitor::visitEndTag(std::uint32_t item) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            parent->visitEndTag(item);
    }

    void BlockVisitor::visitBranch(std::uint32_t item, Condition condition) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBranch(item,condition);
    }

    lccc::optional<SwitchVisitor&> BlockVisitor::visitSwitch() {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitSwitch();
        else
            return lccc::nullopt;
    }

    void BlockVisitor::visitBeginStorage(std::uint32_t local) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBeginStorage(local);
    }

    void BlockVisitor::visitEndStorage(std::uint32_t local) {
        if(auto parent = this->get_parent<BlockVisitor>();parent)
            parent->visitEndStorage(local);
    }

    FunctionVisitor::FunctionVisitor(lccc::optional<FunctionVisitor&> fnVisitor) : AnnotatedElementVisitor{fnVisitor} {

    }

    lccc::optional<FunctionTypeVisitor&> FunctionVisitor::visitType() {
        if(auto parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitType();
        else
            return lccc::nullopt;
    }

    lccc::optional<BlockVisitor&> FunctionVisitor::visitInitialBlock() {
        if(auto parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitInitialBlock();
        else
            return lccc::nullopt;
    }

    lccc::optional<TypeVisitor&> FunctionVisitor::visitLocalVariable() {
        if(auto parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitLocalVariable();
        else
            return lccc::nullopt;
    }

    FileVisitor::FileVisitor(lccc::optional<FileVisitor&> vparent) : ScopeVisitor{vparent} {

    }

    void FileVisitor::visitInputFile(FILE *file) {
        if(auto parent = this->get_parent<FileVisitor>();parent)
            parent->visitInputFile(file);
    }

    void FileVisitor::visitOutputFile(FILE *file) {
        if(auto parent = this->get_parent<FileVisitor>();parent)
            parent->visitOutputFile(file);
    }

    void FileVisitor::visitDiagnosticFile(FILE *file) {
        if(auto parent = this->get_parent<FileVisitor>();parent)
            parent->visitDiagnosticFile(file);
    }

    void FileVisitor::visitTarget(lccc::Target tgt){
        if(auto parent = this->get_parent<FileVisitor>();parent)
            parent->visitTarget(tgt);
    }

    GlobalVariableVisitor::GlobalVariableVisitor(lccc::optional<GlobalVariableVisitor&> parent) : AnnotatedElementVisitor{parent} {

    }

    lccc::optional<TypeVisitor&> GlobalVariableVisitor::visitVariableType() {
        if(auto parent = this->get_parent<GlobalVariableVisitor>();parent)
            return parent->visitVariableType();
        else
            return lccc::nullopt;
    }

    void GlobalVariableVisitor::visitStorageClass(StorageClass cl) {
        if(auto parent = this->get_parent<GlobalVariableVisitor>();parent)
            parent->visitStorageClass(cl);
    }

    LambdaVisitor::LambdaVisitor(lccc::optional<LambdaVisitor&> vparent) : Visitor{vparent} {

    }

    lccc::optional<GenericParameterVisitor&> LambdaVisitor::visitGenericParameter() {
        if(auto parent = this->get_parent<LambdaVisitor>();parent)
            return parent->visitGenericParameter();
        else
            return lccc::nullopt;
    }

    lccc::optional<FunctionVisitor&> LambdaVisitor::visitLambdaBody() {
        if(auto parent = this->get_parent<LambdaVisitor>();parent)
            return parent->visitLambdaBody();
        else
            return lccc::nullopt;
    }
}


