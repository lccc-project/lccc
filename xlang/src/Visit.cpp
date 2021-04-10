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
    Visitor::Visitor(Visitor* other):other{other}{}

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

    IdentifierVisitor::IdentifierVisitor(IdentifierVisitor *other) : Visitor{other} {

    }

    void IdentifierVisitor::visitRoot() {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitRoot();
    }

    void IdentifierVisitor::visitComponent(lccc::string_view name) {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitComponent(name);
    }

    void IdentifierVisitor::visitSpecialComponent(lccc::string_view name) {
        if(auto* id = this->get_parent<IdentifierVisitor>();id)
            id->visitSpecialComponent(name);
    }

    TypeVisitor *IdentifierVisitor::visitDependentName() {
        if(auto* parent = this->get_parent<IdentifierVisitor>();parent)
            return parent->visitDependentName();
        else
            return nullptr;
    }

    GenericInstantiationVisitor *IdentifierVisitor::visitGenericArgs() {
        if(auto* parent = this->get_parent<IdentifierVisitor>();parent)
            return parent->visitGenericArgs();
        else
            return nullptr;
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

    ValueVisitor *AnnotationVisitor::visitItem() {
        if(auto* parent = this->get_parent<AnnotationVisitor>();parent)
            return parent->visitItem();
        else
            return nullptr;
    }


    AnnotatedElementVisitor::AnnotatedElementVisitor(AnnotatedElementVisitor *other) : Visitor{other} {}

    AnnotationVisitor *AnnotatedElementVisitor::visitAnnotation() {
        if(auto* id = this->get_parent<AnnotatedElementVisitor>();id)
            return id->visitAnnotation();
        else
            return nullptr;
    }

    ScopeVisitor::ScopeVisitor(ScopeVisitor *other) : AnnotatedElementVisitor{other} {

    }

    ScopeMemberVisitor *ScopeVisitor::visitScopeMember() {
        if(auto* id = this->get_parent<ScopeVisitor>();id)
            return id->visitScopeMember();
        else
            return nullptr;
    }

    GenericInstantiationVisitor *ScopeVisitor::visitExplicitInstantiation() {
        if(auto* parent = this->get_parent<ScopeVisitor>();parent)
            return parent->visitExplicitInstantiation();
        else
            return nullptr;
    }

    ScopeMemberVisitor::ScopeMemberVisitor(ScopeMemberVisitor *other) : AnnotatedElementVisitor{other} {}

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

    void ScopeMemberVisitor::visitStaticAssertion(const lccc::function<void(ValueVisitor &)>& fn, lccc::string_view diagnostic) {
        if(auto* member = this->get_parent<ScopeMemberVisitor>();member)
            member->visitStaticAssertion(fn,diagnostic);
    }

    TypeVisitor *ScopeMemberVisitor::visitTypeAlias() {
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

    TypeDefinitionVisitor *ScopeMemberVisitor::visitTypeDefinition() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitTypeDefinition();
        else
            return nullptr;
    }

    GlobalVariableVisitor *ScopeMemberVisitor::visitGlobalVariable() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitGlobalVariable();
        else
            return nullptr;
    }

    FunctionVisitor *ScopeMemberVisitor::visitFunction() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitFunction();
        else
            return nullptr;
    }

    ScopeVisitor *ScopeMemberVisitor::visitScope() {
        if(auto* id = this->get_parent<ScopeMemberVisitor>();id)
            return id->visitScope();
        else
            return nullptr;
    }


    void ScopeMemberVisitor::visitExternalScope() {
        if(auto* parent = this->get_parent<ScopeMemberVisitor>();parent)
            this->visitExternalScope();
    }


    GenericDeclarationVisitor::GenericDeclarationVisitor(GenericDeclarationVisitor *parent) : Visitor{parent} {}

    GenericParameterVisitor *GenericDeclarationVisitor::visitGenericParameter() {
        if(auto* id = this->get_parent<GenericDeclarationVisitor>();id)
            return id->visitGenericParameter();
        else
            return nullptr;
    }

    ValueVisitor *GenericDeclarationVisitor::visitRequiresClause() {
        if(auto* id = this->get_parent<GenericDeclarationVisitor>();id)
            return id->visitRequiresClause();
        else
            return nullptr;
    }

    GenericMemberVisitor::GenericMemberVisitor(GenericMemberVisitor *parent) : GenericDeclarationVisitor(parent) {}

    ScopeMemberVisitor *GenericMemberVisitor::visit() {
        if(auto* id = this->get_parent<GenericMemberVisitor>();id)
            return id->visit();
        else
            return nullptr;
    }

    GenericParameterVisitor::GenericParameterVisitor(GenericParameterVisitor *parent) : AnnotatedElementVisitor(parent) {}

    TypeGenericParameterVisitor *GenericParameterVisitor::visitTypeParameter() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitTypeParameter();
        else
            return nullptr;
    }

    ConstGenericParameterVisitor *GenericParameterVisitor::visitConstParameter() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitConstParameter();
        else
            return nullptr;
    }

    BoundGenericParameterVisitor *GenericParameterVisitor::visitBoundParameter() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitBoundParameter();
        else
            return nullptr;
    }

    GenericDeclarationVisitor *GenericParameterVisitor::visitGenericType() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitGenericType();
        else
            return nullptr;
    }

    void GenericParameterVisitor::visitParameterPack() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            id->visitParameterPack();
    }

    TypeVisitor *GenericParameterVisitor::visitDefaultType() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultType();
        else
            return nullptr;
    }

    ExprVisitor *GenericParameterVisitor::visitDefaultValue() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultValue();
        else
            return nullptr;
    }

    IdentifierVisitor *GenericParameterVisitor::visitDefaultGenericType() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultGenericType();
        else
            return nullptr;
    }

    BoundVisitor *GenericParameterVisitor::visitDefaultBound() {
        if(auto* id = this->get_parent<GenericParameterVisitor>();id)
            return id->visitDefaultBound();
        else
            return nullptr;
    }


    TypeVisitor::TypeVisitor(TypeVisitor *parent) : AnnotatedElementVisitor{parent} {}

    ScalarTypeVisitor *TypeVisitor::visitScalarType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitScalarType();
        else
            return nullptr;
    }

    PointerTypeVisitor *TypeVisitor::visitPointerType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitPointerType();
        else
            return nullptr;
    }

    IdentifierVisitor *TypeVisitor::visitNamedType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitNamedType();
        else
            return nullptr;
    }

    GenericInstantiationVisitor *TypeVisitor::visitGenericType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitGenericType();
        else
            return nullptr;
    }

    void TypeVisitor::visitGenericParameter(uint32_t pnum) {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            id->visitGenericParameter(pnum);
    }

    ValueVisitor *TypeVisitor::visitAlignedAs() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitAlignedAs();
        else
            return nullptr;
    }

    ProductTypeVisitor *TypeVisitor::visitProductType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitProductType();
        else
            return nullptr;
    }

    SumTypeVisitor *TypeVisitor::visitSumType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitSumType();
        else
            return nullptr;
    }

    FunctionTypeVisitor *TypeVisitor::visitFunctionType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitFunctionType();
        else
            return nullptr;
    }

    TypeDefinitionVisitor *TypeVisitor::visitElaboratedType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitElaboratedType();
        else
            return nullptr;
    }

    ConceptVisitor *TypeVisitor::visitErasedConceptType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitErasedConceptType();
        else
            return nullptr;
    }

    ConceptVisitor *TypeVisitor::visitReifiedConceptType() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitReifiedConceptType();
        else
            return nullptr;
    }

    TypeVisitor *TypeVisitor::visitSlice() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitSlice();
        else
            return nullptr;
    }

    ArrayTypeVisitor *TypeVisitor::visitArray() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitArray();
        else
            return nullptr;
    }

    ExprVisitor *TypeVisitor::visitDecltype() {
        if(auto* id = this->get_parent<TypeVisitor>();id)
            return id->visitDecltype();
        else
            return nullptr;
    }

    void TypeVisitor::visitVoid(){
        if(auto* id = this->get_parent<TypeVisitor>();id)
            id->visitVoid();
    }

    GenericInstantiationVisitor::GenericInstantiationVisitor(GenericInstantiationVisitor *visitor) : Visitor(visitor) {}

    IdentifierVisitor *GenericInstantiationVisitor::visitGenericItem() {
        if(auto* id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitGenericItem();
        else
            return nullptr;
    }

    TypeVisitor *GenericInstantiationVisitor::visitTypeParameter() {
        if(auto* id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitTypeParameter();
        else
            return nullptr;
    }

    IdentifierVisitor *GenericInstantiationVisitor::visitGenericParameter() {
        if(auto* id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitGenericParameter();
        else
            return nullptr;
    }

    ValueVisitor *GenericInstantiationVisitor::visitConstParameter() {
        if(auto* id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitConstParameter();
        else
            return nullptr;
    }

    BoundVisitor *GenericInstantiationVisitor::visitBoundParameter() {
        if(auto* id = this->get_parent<GenericInstantiationVisitor>();id)
            return id->visitBoundParameter();
        else
            return nullptr;
    }

    PointerTypeVisitor::PointerTypeVisitor(PointerTypeVisitor *parent) : Visitor{parent} {}

    TypeVisitor *PointerTypeVisitor::visitPointeeType() {
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitPointeeType();
        else
            return nullptr;
    }

    void PointerTypeVisitor::visitAliasingRule(PointerAliasingRule aliasingRule) {
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            visitAliasingRule(aliasingRule);
    }

    ValueVisitor *PointerTypeVisitor::visitValidRange(ValidRangeType validRange) {
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitValidRange(validRange);
        else
            return nullptr;
    }

    ValueVisitor *PointerTypeVisitor::visitAligned() {
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitAligned();
        else
            return nullptr;
    }

    BoundVisitor *PointerTypeVisitor::visitRequiredBounds() {
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            return id->visitRequiredBounds();
        else
            return nullptr;
    }

    void PointerTypeVisitor::visitDefinitionType(PointerDefinitionType type){
        if(auto* id = this->get_parent<PointerTypeVisitor>();id)
            id->visitDefinitionType(type);
    }

    ScalarTypeVisitor::ScalarTypeVisitor(ScalarTypeVisitor *parent) : Visitor{parent} {}

    IntegerTypeVisitor *ScalarTypeVisitor::visitIntegerType() {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            return id->visitIntegerType();
        else
            return nullptr;
    }

    FloatTypeVisitor *ScalarTypeVisitor::visitFloatingPointType() {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            return id->visitFloatingPointType();
        else
            return nullptr;
    }

    void ScalarTypeVisitor::visitBitSize(uint16_t bits) {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitBitSize(bits);
    }

    void ScalarTypeVisitor::visitVectorSize(uint16_t vector) {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitVectorSize(vector);
    }

    IntegerTypeVisitor::IntegerTypeVisitor(IntegerTypeVisitor *parent) : Visitor{parent} {}

    void IntegerTypeVisitor::visitSigned() {
        if(auto* id = this->get_parent<IntegerTypeVisitor>();id)
            id->visitSigned();
    }

    ValueVisitor* IntegerTypeVisitor::visitMinimumValue() {
        if(auto* id = this->get_parent<IntegerTypeVisitor>();id)
            return id->visitMinimumValue();
        else
            return nullptr;
    }
    ValueVisitor* IntegerTypeVisitor::visitMaximumValue() {
        if(auto* id = this->get_parent<IntegerTypeVisitor>();id)
            return id->visitMaximumValue();
        else
            return nullptr;
    }

    ProductTypeVisitor::ProductTypeVisitor(ProductTypeVisitor *parent) : Visitor{parent} {}

    TypeVisitor *ProductTypeVisitor::visitType() {
        if(auto* id = this->get_parent<ProductTypeVisitor>();id)
            return id->visitType();
        else
            return nullptr;
    }

    SumTypeVisitor::SumTypeVisitor(SumTypeVisitor *parent) : Visitor{parent} {}

    TypeVisitor *SumTypeVisitor::visitType() {
        if(auto* id = this->get_parent<SumTypeVisitor>();id)
            return id->visitType();
        else
            return nullptr;
    }

    FloatTypeVisitor::FloatTypeVisitor(FloatTypeVisitor *vparent) : Visitor{vparent} {}

    void FloatTypeVisitor::visitDecimalFloat() {
        if(auto* id = this->get_parent<FloatTypeVisitor>();id)
            id->visitDecimalFloat();
    }

    void ScalarTypeVisitor::visitComplex() {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitComplex();
    }

    void ScalarTypeVisitor::visitValueValidity(ScalarValidity validity) {
        if(auto* id = this->get_parent<ScalarTypeVisitor>();id)
            id->visitValueValidity(validity);
    }

    FunctionTypeVisitor::FunctionTypeVisitor(FunctionTypeVisitor *parent) : Visitor{parent} {

    }

    TypeVisitor *FunctionTypeVisitor::visitReturnType() {
        if(auto* visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitReturnType();
        else
            return nullptr;
    }

    TypeVisitor *FunctionTypeVisitor::visitParameterType() {
        if(auto* visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitParameterType();
        else
            return nullptr;
    }

    void FunctionTypeVisitor::visitVarargs() {
        if(auto* visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            visitor->visitVarargs();
    }

    IdentifierVisitor *FunctionTypeVisitor::visitLinkage() {
        if(auto* visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitLinkage();
        else
            return nullptr;
    }

    IdentifierVisitor *FunctionTypeVisitor::visitTag() {
        if(auto* visitor=this->get_parent<FunctionTypeVisitor>();visitor)
            return visitor->visitTag();
        else
            return nullptr;
    }

    BoundVisitor::BoundVisitor(BoundVisitor *vparent) : Visitor{vparent} {

    }

    void BoundVisitor::visitGenericBound(std::uint32_t param) {
        if(auto* visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitGenericBound(param);

    }

    void BoundVisitor::visitStatic() {
        if(auto* visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitStatic();
    }

    void BoundVisitor::visitToken(std::uint64_t token) {
        if(auto* visitor=this->get_parent<BoundVisitor>();visitor)
            visitor->visitToken(token);
    }

    BoundGenericParameterVisitor::BoundGenericParameterVisitor(BoundGenericParameterVisitor *visitor) : Visitor{visitor} {

    }

    BoundVisitor *BoundGenericParameterVisitor::visitEnclosedBy() {
        if(auto* visitor=this->get_parent<BoundGenericParameterVisitor>();visitor)
            return visitor->visitEnclosedBy();
        else
            return nullptr;
    }

    ValueVisitor::ValueVisitor(ValueVisitor *vparent) : Visitor{vparent} {

    }

    ConstantVisitor *ValueVisitor::visitConstantValue() {
        if(auto* parent =this->get_parent<ValueVisitor>();parent)
            return parent->visitConstantValue();
        else
            return nullptr;
    }

    ExprVisitor *ValueVisitor::visitExpression() {
        if(auto* parent =this->get_parent<ValueVisitor>();parent)
            return parent->visitExpression();
        else
            return nullptr;
    }

    TypeVisitor* ValueVisitor::visitUndefined(UndefinedValueKind kind) {
        if(auto* parent = this->get_parent<ValueVisitor>();parent)
            return parent->visitUndefined(kind);
        else
            return nullptr;
    }


    ConstantVisitor::ConstantVisitor(ConstantVisitor *vparent) : Visitor{vparent} {

    }

    TypeVisitor *ConstantVisitor::visitInteger(std::intmax_t value) {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitInteger(value);
        else
            return nullptr;
    }

    StringLiteralVisitor *ConstantVisitor::visitStringLiteral() {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitStringLiteral();
        else
            return nullptr;
    }

    void ConstantVisitor::visitBooleanLiteral(bool value) {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            parent->visitBooleanLiteral(value);
    }

    PointerConstantVisitor *ConstantVisitor::visitPointerConstant() {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitPointerConstant();
        else
            return nullptr;
    }

    TypeVisitor *ConstantVisitor::visitExcessValueInteger(lccc::span<const uint8_t> bytes) {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitExcessValueInteger(bytes);
        else
            return nullptr;
    }

    ArrayConstantVisitor *ConstantVisitor::visitConstantArray() {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitConstantArray();
        else
            return nullptr;
    }

    TypeVisitor *ConstantVisitor::visitFloatingValue(long double val) {
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitFloatingValue(val);
        else
            return nullptr;
    }

    TypeVisitor *ConstantVisitor::visitSizeOf(){
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitSizeOf();
        else
            return nullptr;
    }

    TypeVisitor *ConstantVisitor::visitAlignOf(){
        if(auto* parent = this->get_parent<ConstantVisitor>();parent)
            return parent->visitAlignOf();
        else
            return nullptr;
    }

    PointerConstantVisitor::PointerConstantVisitor(PointerConstantVisitor *vparent) : Visitor{vparent} {

    }

    PointerTypeVisitor *PointerConstantVisitor::visitType() {
        if(auto* parent = this->get_parent<PointerConstantVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    void PointerConstantVisitor::visitNullPointer() {
        if(auto* parent = this->get_parent<PointerConstantVisitor>();parent)
            parent->visitNullPointer();
    }

    IdentifierVisitor *PointerConstantVisitor::visitGlobalAddress() {
        if(auto* parent = this->get_parent<PointerConstantVisitor>();parent)
            return parent->visitGlobalAddress();
        else
            return nullptr;
    }

    void PointerConstantVisitor::visitLabelAddress(std::uint32_t label) {
        if(auto* parent = this->get_parent<PointerConstantVisitor>();parent)
            parent->visitLabelAddress(label);
    }

    StringLiteralVisitor::StringLiteralVisitor(StringLiteralVisitor *vparent) : Visitor{vparent} {

    }

    TypeVisitor *StringLiteralVisitor::visitType() {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    void StringLiteralVisitor::visitByteString(lccc::string_view value) {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitByteString(value);
    }
    void StringLiteralVisitor::visitUTF8String(lccc::string_view value) {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF8String(value);
    }
    void StringLiteralVisitor::visitUTF16String(lccc::u16string_view value) {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF16String(value);
    }
    void StringLiteralVisitor::visitUTF32String(lccc::u32string_view value) {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitUTF32String(value);
    }
    void StringLiteralVisitor::visitWideString(lccc::wstring_view value) {
        if(auto* parent = this->get_parent<StringLiteralVisitor>();parent)
            parent->visitWideString(value);
    }

    TypeDefinitionVisitor::TypeDefinitionVisitor(TypeDefinitionVisitor *visitor) : AnnotatedElementVisitor{visitor} {

    }

    StructTypeVisitor *TypeDefinitionVisitor::visitStruct() {
        if(auto* parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitStruct();
        else
            return nullptr;
    }

    EnumTypeVisitor *TypeDefinitionVisitor::visitEnum() {
        if(auto* parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitEnum();
        else
            return nullptr;
    }

    StructTypeVisitor *TypeDefinitionVisitor::visitUnion() {
        if(auto* parent = this->get_parent<TypeDefinitionVisitor>();parent)
            return parent->visitUnion();
        else
            return nullptr;
    }


    StructTypeVisitor::StructTypeVisitor(StructTypeVisitor *vparent) : AnnotatedElementVisitor{vparent} {

    }

    StructFieldVisitor *StructTypeVisitor::visitStructField(Visibility vis) {
        if(auto* parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitStructField(vis);
        else
            return nullptr;
    }

    TypeVisitor *StructTypeVisitor::visitBaseClass(Visibility vis) {
        if(auto* parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitBaseClass(vis);
        else
            return nullptr;
    }

    TypeVisitor *StructTypeVisitor::visitVirtualBaseClass(Visibility vis) {
        if(auto* parent = this->get_parent<StructTypeVisitor>();parent)
            return parent->visitVirtualBaseClass(vis);
        else
            return nullptr;
    }

    EnumTypeVisitor::EnumTypeVisitor(EnumTypeVisitor *vparent) : AnnotatedElementVisitor{vparent} {

    }

    void EnumTypeVisitor::visitStrong() {
        if(auto* parent = this->get_parent<EnumTypeVisitor>();parent)
            parent->visitStrong();
    }

    TypeVisitor *EnumTypeVisitor::visitUnderlyingType() {
        if(auto* parent = this->get_parent<EnumTypeVisitor>();parent)
            return parent->visitUnderlyingType();
        else
            return nullptr;
    }

    EnumeratorVisitor *EnumTypeVisitor::visitEnumerator() {
        if(auto* parent = this->get_parent<EnumTypeVisitor>();parent)
            return parent->visitEnumerator();
        else
            return nullptr;
    }

    StructFieldVisitor::StructFieldVisitor(StructFieldVisitor *vparent) : AnnotatedElementVisitor{vparent} {

    }

    IdentifierVisitor *StructFieldVisitor::visitName() {
        if(auto* parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitName();
        else
            return nullptr;
    }

    TypeVisitor *StructFieldVisitor::visitType() {
        if(auto* parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    ValueVisitor *StructFieldVisitor::visitBitFieldLength() {
        if(auto* parent = this->get_parent<StructFieldVisitor>();parent)
            return parent->visitBitFieldLength();
        else
            return nullptr;
    }

    EnumeratorVisitor::EnumeratorVisitor(EnumeratorVisitor *vparent) : AnnotatedElementVisitor(vparent) {

    }

    IdentifierVisitor *EnumeratorVisitor::visitName() {
        if(auto* parent = this->get_parent<EnumeratorVisitor>();parent)
            return parent->visitName();
        else
            return nullptr;
    }

    ValueVisitor *EnumeratorVisitor::visitValue() {
        if(auto* parent = this->get_parent<EnumeratorVisitor>();parent)
            return parent->visitValue();
        else
            return nullptr;
    }


    TypeGenericParameterVisitor::TypeGenericParameterVisitor(TypeGenericParameterVisitor *vparent) : AnnotatedElementVisitor{vparent} {

    }

    ConceptVisitor *TypeGenericParameterVisitor::visitConcept() {
        if(auto* parent = this->get_parent<TypeGenericParameterVisitor>();parent)
            return parent->visitConcept();
        else
            return nullptr;
    }

    ConstGenericParameterVisitor::ConstGenericParameterVisitor(ConstGenericParameterVisitor *vparent) : AnnotatedElementVisitor{vparent} {

    }

    TypeVisitor *ConstGenericParameterVisitor::visitType() {
        if(auto* parent = this->get_parent<ConstGenericParameterVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    ArrayTypeVisitor::ArrayTypeVisitor(ArrayTypeVisitor *parent) : Visitor{parent} {

    }

    TypeVisitor *ArrayTypeVisitor::visitComponentType() {
        if(auto* parent = this->get_parent<ArrayTypeVisitor>();parent)
            return parent->visitComponentType();
        else
            return nullptr;
    }

    ValueVisitor *ArrayTypeVisitor::visitExtent() {
        if(auto* parent = this->get_parent<ArrayTypeVisitor>();parent)
            return parent->visitExtent();
        else
            return nullptr;
    }

    ArrayConstantVisitor::ArrayConstantVisitor(ArrayConstantVisitor *vparent) : Visitor{vparent} {

    }

    TypeVisitor *ArrayConstantVisitor::visitType() {
        if(auto* parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    ValueVisitor *ArrayConstantVisitor::visitRepeatLength() {
        if(auto* parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitRepeatLength();
        else
            return nullptr;
    }

    ValueVisitor *ArrayConstantVisitor::visitValue() {
        if(auto* parent = this->get_parent<ArrayConstantVisitor>();parent)
            return parent->visitValue();
        else
            return nullptr;
    }

    ExprVisitor::ExprVisitor(ExprVisitor *vparent) : Visitor{vparent} {

    }

    ValueVisitor *ExprVisitor::visitConst() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitConst();
        else
            return nullptr;
    }

    FunctionTypeVisitor *ExprVisitor::visitFunctionCall() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitFunctionCall();
        else
            return nullptr;
    }

    IdentifierVisitor *ExprVisitor::visitMember(MemberAccessType type) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitMember(type);
        else
            return nullptr;
    }

    void ExprVisitor::visitLocal(uint32_t var) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitLocal(var);
    }

    void ExprVisitor::visitAsRvalue(AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitAsRvalue(cl);
    }

    TypeVisitor *ExprVisitor::visitConversion(ConversionStrength st) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitConversion(st);
        else
            return nullptr;
    }

    void ExprVisitor::visitUnaryOperator(UnaryOperation op) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitUnaryOperator(op);
    }

    void ExprVisitor::visitBinaryOperator(BinaryOperation op) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitBinaryOperator(op);
    }

    void ExprVisitor::visitIndirection() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitIndirection();
    }

    BoundVisitor *ExprVisitor::visitLock(PointerSharing type) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitLock(type);
        else
            return nullptr;
    }

    void ExprVisitor::visitPointerTo() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitPointerTo();
    }

    PointerTypeVisitor *ExprVisitor::visitDerive() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitDerive();
        else
            return nullptr;
    }

    void ExprVisitor::visitSequence(AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitSequence(cl);
    }

    void ExprVisitor::visitFence(AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitFence(cl);
    }

    void ExprVisitor::visitAssignment(AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitAssignment(cl);
    }

    void ExprVisitor::visitCompoundAssignment(BinaryOperation op, AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitCompoundAssignment(op,cl);
    }

    void ExprVisitor::visitLValue(LValueOperation op, AccessClass cl) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitLValue(op,cl);
    }

    void ExprVisitor::visitDestroy() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitDestroy();
    }

    void ExprVisitor::pop(uint8_t cnt) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->pop(cnt);
    }

    void ExprVisitor::dup(uint8_t cnt) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->dup(cnt);
    }

    BlockVisitor *ExprVisitor::visitBlock() {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitBlock();
        else
            return nullptr;
    }

    void ExprVisitor::visitTupleExpression(uint16_t values) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitTupleExpression(values);
    }

    TypeVisitor *ExprVisitor::visitAggregateConstruction(uint16_t values) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitAggregateConstruction(values);
        else
            return nullptr;
    }

    void ExprVisitor::visitBlockExit(uint32_t blk, uint8_t values) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            parent->visitBlockExit(blk,values);
    }

    LambdaVisitor *ExprVisitor::visitLambdaExpression(uint16_t captures) {
        if(auto* parent = this->get_parent<ExprVisitor>();parent)
            return parent->visitLambdaExpression(captures);
        else
            return nullptr;
    }

    StackItemsVisitor::StackItemsVisitor(StackItemsVisitor *visitor) : Visitor{visitor} {

    }

    TypeVisitor *StackItemsVisitor::visitLvalue() {
        if(auto* parent = this->get_parent<StackItemsVisitor>();parent)
            return parent->visitLvalue();
        else
            return nullptr;
    }

    TypeVisitor *StackItemsVisitor::visitRvalue() {
        if(auto* parent = this->get_parent<StackItemsVisitor>();parent)
            return parent->visitRvalue();
        else
            return nullptr;
    }


    CaseVisitor::CaseVisitor(CaseVisitor *vparent) : Visitor{vparent} {

    }

    ValueVisitor *CaseVisitor::visitValue() {
        if(auto* parent = this->get_parent<CaseVisitor>();parent)
            return parent->visitValue();
        else
            return nullptr;
    }

    void CaseVisitor::visitTarget(std::uint32_t item) {
        if(auto* parent = this->get_parent<CaseVisitor>();parent)
            parent->visitTarget(item);
    }

    void SwitchVisitor::visitDefault(std::uint32_t item) {
        if(auto* parent = this->get_parent<SwitchVisitor>();parent)
            parent->visitDefault(item);
    }

    CaseVisitor *SwitchVisitor::visitCase() {
        if(auto* parent = this->get_parent<SwitchVisitor>();parent)
            return parent->visitCase();
        else
            return nullptr;
    }

    SwitchVisitor::SwitchVisitor(SwitchVisitor *vparent) : Visitor{vparent} {

    }

    BlockVisitor::BlockVisitor(BlockVisitor *parent) : ScopeVisitor{parent} {

    }

    ExprVisitor *BlockVisitor::visitExpression() {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitExpression();
        else
            return nullptr;
    }

    StackItemsVisitor *BlockVisitor::visitTarget(std::uint32_t item) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitTarget(item);
        else
            return nullptr;
    }

    void BlockVisitor::visitBeginTag(std::uint32_t item) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBeginTag(item);
    }

    void BlockVisitor::visitEndTag(std::uint32_t item) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            parent->visitEndTag(item);
    }

    void BlockVisitor::visitBranch(std::uint32_t item, Condition condition) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBranch(item,condition);
    }

    SwitchVisitor *BlockVisitor::visitSwitch() {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            return parent->visitSwitch();
        else
            return nullptr;
    }

    void BlockVisitor::visitBeginStorage(std::uint32_t local) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            parent->visitBeginStorage(local);
    }

    void BlockVisitor::visitEndStorage(std::uint32_t local) {
        if(auto* parent = this->get_parent<BlockVisitor>();parent)
            parent->visitEndStorage(local);
    }

    FunctionVisitor::FunctionVisitor(FunctionVisitor *fnVisitor) : AnnotatedElementVisitor{fnVisitor} {

    }

    FunctionTypeVisitor *FunctionVisitor::visitType() {
        if(auto* parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitType();
        else
            return nullptr;
    }

    BlockVisitor *FunctionVisitor::visitInitialBlock() {
        if(auto* parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitInitialBlock();
        else
            return nullptr;
    }

    TypeVisitor *FunctionVisitor::visitLocalVariable() {
        if(auto* parent = this->get_parent<FunctionVisitor>();parent)
            return parent->visitLocalVariable();
        else
            return nullptr;
    }

    FileVisitor::FileVisitor(FileVisitor *vparent) : ScopeVisitor{vparent} {

    }

    void FileVisitor::visitInputFile(FILE *file) {
        if(auto* parent = this->get_parent<FileVisitor>();parent)
            parent->visitInputFile(file);
    }

    void FileVisitor::visitOutputFile(FILE *file) {
        if(auto* parent = this->get_parent<FileVisitor>();parent)
            parent->visitOutputFile(file);
    }

    void FileVisitor::visitDiagnosticFile(FILE *file) {
        if(auto* parent = this->get_parent<FileVisitor>();parent)
            parent->visitDiagnosticFile(file);
    }

    void FileVisitor::visitTarget(lccc::Target tgt){
        if(auto* parent = this->get_parent<FileVisitor>();parent)
            parent->visitTarget(tgt);
    }

    GlobalVariableVisitor::GlobalVariableVisitor(GlobalVariableVisitor *parent) : AnnotatedElementVisitor{parent} {

    }

    TypeVisitor *GlobalVariableVisitor::visitVariableType() {
        if(auto* parent = this->get_parent<GlobalVariableVisitor>();parent)
            return parent->visitVariableType();
        else
            return nullptr;
    }

    void GlobalVariableVisitor::visitStorageClass(StorageClass cl) {
        if(auto* parent = this->get_parent<GlobalVariableVisitor>();parent)
            parent->visitStorageClass(cl);
    }

    LambdaVisitor::LambdaVisitor(LambdaVisitor *vparent) : Visitor{vparent} {

    }

    GenericParameterVisitor *LambdaVisitor::visitGenericParameter() {
        if(auto* parent = this->get_parent<LambdaVisitor>();parent)
            return parent->visitGenericParameter();
        else
            return nullptr;
    }

    FunctionVisitor *LambdaVisitor::visitLambdaBody() {
        if(auto* parent = this->get_parent<LambdaVisitor>();parent)
            return parent->visitLambdaBody();
        else
            return nullptr;
    }
}


