/**
 * xlang++/Visit.hpp
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

#ifndef XLANG_VISIT_HPP
#define XLANG_VISIT_HPP

#include <string>
#include <string_view>
#include <optional>
#include <memory>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <xlang++/layout/StringView.hpp>
#include <xlang++/Target.hpp>
#include <xlang++/layout/Function.hpp>
#include <xlang++/layout/Span.hpp>

namespace lccc::xlang{
    /**
     * Root of the visitor hierarchy. All visitors inherit from this type.
     *
     */
    class Visitor{
    private:
        lccc::optional<Visitor&> other;
    protected:
        /**
         * Returns the parent of this visitor, if one exists and inherits from T
         * @tparam T The type of the visitor to return. Does not participate in overload resolution unless T
         *  is a (potentially cv-qualified) class type that inherits from Visitor.
         * @return
         */
        template<typename T,std::enable_if_t<std::is_base_of_v<Visitor,std::remove_cv_t<T>>>* = nullptr>
            lccc::optional<T&> get_parent(){
                return other.downcast<T>();
            }
    public:
        /**
         * Constructs a new Visitor, with a given optional parent
         * @param other The parent of this visitor, or a null pointer. If omitted, treated as null
         */
        explicit Visitor(lccc::optional<Visitor&> other=lccc::nullopt);
        virtual ~Visitor()=default;
        /**
         * Ends the operation of this visitor, and releases any borrowed resources.
         * Further calls to visit methods,
         *  may have unpredicatable results.
         */
        virtual void visitEnd();
        /**
         * visits a diagnostic, which results in a compilation error
         */
        virtual void visitDiagnostic(lccc::string_view);

        /**
         * Visits a line Number
         */
        virtual void visitLine(std::uint64_t ln);

        /**
         * Visits a Source name
         */
        virtual void visitSourceFile(lccc::string_view name);
    };

    struct TypeVisitor;
    struct GenericInstantiationVisitor;

    /**
     * A visitor which visits a (potentially qualified) path.
     */
    struct PathVisitor : Visitor{
        /**
         * Constructs an IdentifierVisitor with a given optional parent
         */
        explicit PathVisitor(lccc::optional<PathVisitor&> other=lccc::nullopt);
        /**
         * visits the root component, that is, the component which resolves to the root of the namespace tree
         */
        virtual void visitRoot();
        /**
         * Visits an identifier component, resolved based on the previous components,
         *  which resolves to the namespace with the given name in the namespace provided by the previous components.
         */
        virtual void visitComponent(lccc::string_view);

        /**
         * @brief Visits a Dependent name, something that depends on one or more generic parameters
         * 
         * @return TypeVisitor* A visitor for the dependent type, or null if this visitor is not interested.
         */
        virtual lccc::optional<TypeVisitor&> visitDependentName();

        /**
         * @brief Visits generic args. 
         * 
         * @return GenericInstantiationVisitor* A visitor for the generic args, or null if this visitor is not interested.
         */
        virtual lccc::optional<GenericInstantiationVisitor&> visitGenericArgs();

        /**
         * Visits a special component, with a language-specific form.
         */
        virtual void visitSpecialComponent(lccc::string_view);
    };

    struct ValueVisitor;

    /**
     * A visitor which visits an annotation
     */
    struct AnnotationVisitor : Visitor{
        /**
         * Constructs a new AnnotationVisitor with an optional parent
         */
        explicit AnnotationVisitor(lccc::optional<AnnotationVisitor&> other=lccc::nullopt);
        /**
         * Visits a subcomponent of the annotation
         * @return A pointer to a visitor which visits the subcomponent, or null if this visitor isn't interested in visiting a subcomponent
         */
        virtual lccc::optional<AnnotationVisitor&> visitMeta();
        /**
         * Visits an (unqouted) identifier
         * @return A pointer to a visitor which visits the identifier, or null if this visitor isn't interested in visiting an identifier
         */
        virtual lccc::optional<PathVisitor&> visitIdentifier();

        virtual lccc::optional<ValueVisitor&> visitItem();
    };

    /**
     * A subtype of Visitor which can have annotations
     */
    struct AnnotatedElementVisitor : Visitor{
        /**
         * Constructs a new AnnotatedElementVisitor with an optional parent
         */
        explicit AnnotatedElementVisitor(lccc::optional<AnnotatedElementVisitor&> other=lccc::nullopt);
        /**
         * Visits an annotation on this visitor.
         * This call may be repeated after the returned visitor ends its visitation.
         * @return A visitor which visits the annotation, or null if this visitor is not interested in visiting an annotation
         */
        virtual lccc::optional<AnnotationVisitor&> visitAnnotation();
    };
    struct ScopeMemberVisitor;


    /**
     * A visitor which visits a new scope.
     */
    struct ScopeVisitor : AnnotatedElementVisitor{
        /**
         * Constructs a new ScopeVisitor with an optional parent
         */
        explicit ScopeVisitor(lccc::optional<ScopeVisitor&> other=lccc::nullopt);
        /**
         * Visits a member of this scope.
         * This call may be repeated after the returned visitor ends its visitation.
         * @return A visitor which visits the member, or null if this visitor is not interested in visiting a member.
         */
        virtual lccc::optional<ScopeMemberVisitor&> visitScopeMember();

        virtual lccc::optional<GenericInstantiationVisitor&> visitExplicitInstantiation();
    };

    /**
     * The visibility of the scope member
     */
    enum class Visibility : std::uint16_t {
        /**
         * A scope member which can be named from any scope
         */
        Public,
        /**
         * A scope member which can be named from the declaring scope,
         *  and any enclosing scope up to but not including the root of the namespace heirarchy
         */
        Origin,
        /**
         * A scope member which can be named from the declaring scope,
         * and the immediately enclosing scope
         */
        Module,
        /**
         * A scope member which can be named from the declaraing scope only.
         */
        Private,
        /**
         * A scope member which cannot be named.
         */
        None
    };

    struct GenericDeclarationVisitor;
    struct TypeVisitor;
    struct GenericParameterVisitor;
    struct ExprVisitor;
    struct ValueVisitor;
    struct GenericMemberVisitor;
    struct TypeDefinitionVisitor;
    struct GlobalVariableVisitor;
    struct FunctionVisitor;

    /**
     * A visitor which visits the member of a scope
     */
    struct ScopeMemberVisitor : AnnotatedElementVisitor{
    public:
        /**
         *  Constructs a new ScopeMemberVisitor, with an optional parent
         */
        explicit ScopeMemberVisitor(lccc::optional<ScopeMemberVisitor&> other=lccc::nullopt);
        /**
         * Visits the visibility of this member.
         */
        virtual void visitVisibility(Visibility);
        /**
         * Visits the name of this scope member.
         *  If a name visits more than one component, or the root component, the behaviour is unspecified
         * @return A visitor which visits the name of this scope member, or null if this visitor is not interested in visiting a name.
         */
        virtual lccc::optional<PathVisitor&> visitName();
        /**
         * Visits a static assertion, by using value to visit the controlling expression, and visits a diagnostic if the visitation evaluates to false
         */
        virtual lccc::optional<ValueVisitor&> visitStaticAssertion(lccc::string_view diag);
        /**
         * Visits an alias of a type
         * @return A visitor which visits the type alias, or null if this visitor is not interested in visiting a type alias.
         */
        virtual lccc::optional<TypeVisitor&> visitTypeAlias();
        /**
         * Visits a generic declaration
         * @return  A visitor which visits the generic declaration, or null if this visitor is not interested in visiting a generic declaration.
         */
        virtual lccc::optional<GenericMemberVisitor&> visitGenericDeclaration();
        /**
         * Visits a type declaration, such as a struct, enum, or union
         * @return A visitor which visits the type definition, or null if this visitor is not interested in visiting a type declaration
         */
        virtual lccc::optional<TypeDefinitionVisitor&> visitTypeDefinition();
        /**
         * Visits a global variable (which may not actually be global, it just needs static, or thread, storage duration.)
         * @return A visitor which visits the global variable, or null if this visitor is not interested in visiting a global variable
         */
        virtual lccc::optional<GlobalVariableVisitor&> visitGlobalVariable();
        /**
         * Visits a function declaration or definition.
         * @return A visitor which visits the function, or null if this is visitor is not interested in visiting a function.
         */
        virtual lccc::optional<FunctionVisitor&> visitFunction();

        /**
         * Visits a subscope
         * @return A visitor which visits the new scope
         */
        virtual lccc::optional<ScopeVisitor&> visitScope();

        /**
         * Visits an external scope, that is, one defined in a different source file.
         * 
         */
        virtual void visitExternalScope();

    };

    /**
     * An intermediate class for generic declarations
     */
    struct GenericDeclarationVisitor : Visitor{
        /**
         * Constructs a new GenericDeclarationVisitor with an optional parent
         */
        explicit GenericDeclarationVisitor(lccc::optional<GenericDeclarationVisitor&> parent=lccc::nullopt);
        /**
         * Visits a generic parameter
         * @return A visitor which visits the parameter,
         */
        virtual lccc::optional<GenericParameterVisitor&> visitGenericParameter();

        virtual lccc::optional<ValueVisitor&> visitRequiresClause();
    };

    /**
     * Visitor that visits a scope member with generic parameters
     */
    struct GenericMemberVisitor : GenericDeclarationVisitor{
        /**
         * Constructs a GenericMemberVisitor with an optional parent
         */
        explicit GenericMemberVisitor(lccc::optional<GenericMemberVisitor&> parent=lccc::nullopt);
        /**
         * Visits the ScopeMember, or a null pointer if this is not interested in visiting its member.
         */
        virtual lccc::optional<ScopeMemberVisitor&> visit();
    };

    struct StructTypeVisitor;
    struct EnumTypeVisitor;

    /**
     * A visitor which visits the definition of a type
     */
    struct TypeDefinitionVisitor : AnnotatedElementVisitor{
    public:
        /**
         * Constructs a new TypeDefinitionVisitor with an optional parent
         */
        explicit TypeDefinitionVisitor(lccc::optional<TypeDefinitionVisitor&> visitor=lccc::nullopt);
        
        /**
         * Visits a structure definition
         * @return A visitor which visits the structure definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual lccc::optional<StructTypeVisitor&> visitStruct();
        /**
         * Visits a enum definition
         * @return A visitor which visits the enum definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual lccc::optional<EnumTypeVisitor&> visitEnum();
        /**
         * Visits a union definition
         * @return A visitor which visits the union definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual lccc::optional<StructTypeVisitor&> visitUnion();
    };

    struct StructFieldVisitor;
    struct EnumeratorVisitor;

    struct StructTypeVisitor : AnnotatedElementVisitor{
    public:
        explicit StructTypeVisitor(lccc::optional<StructTypeVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<StructFieldVisitor&> visitStructField(Visibility);
        virtual lccc::optional<TypeVisitor&> visitBaseClass(Visibility);
        virtual lccc::optional<TypeVisitor&> visitVirtualBaseClass(Visibility);
    };

    struct EnumTypeVisitor : AnnotatedElementVisitor{
    public:
        explicit EnumTypeVisitor(lccc::optional<EnumTypeVisitor&> vparent=lccc::nullopt);
        virtual void visitStrong();
        virtual lccc::optional<TypeVisitor&> visitUnderlyingType();
        virtual lccc::optional<EnumeratorVisitor&> visitEnumerator();
    };

    struct StructFieldVisitor : AnnotatedElementVisitor{
    public:
        explicit StructFieldVisitor(lccc::optional<StructFieldVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<PathVisitor&> visitName();
        virtual lccc::optional<TypeVisitor&> visitType();
        virtual lccc::optional<ValueVisitor&> visitBitFieldLength();
    };

    struct EnumeratorVisitor : AnnotatedElementVisitor{
    public:
        explicit EnumeratorVisitor(lccc::optional<EnumeratorVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<PathVisitor&> visitName();
        virtual lccc::optional<ValueVisitor&> visitValue();
    };


    struct TypeGenericParameterVisitor;
    struct ConstGenericParameterVisitor;
    struct BoundGenericParameterVisitor;
    struct BoundVisitor;

    struct GenericParameterVisitor : virtual AnnotatedElementVisitor{
        explicit GenericParameterVisitor(lccc::optional<GenericParameterVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeGenericParameterVisitor&> visitTypeParameter();
        virtual lccc::optional<ConstGenericParameterVisitor&> visitConstParameter();
        virtual lccc::optional<BoundGenericParameterVisitor&> visitBoundParameter();
        virtual lccc::optional<GenericDeclarationVisitor&> visitGenericType();
        virtual void visitParameterPack();
        virtual lccc::optional<TypeVisitor&> visitDefaultType();
        virtual lccc::optional<ExprVisitor&> visitDefaultValue();
        virtual lccc::optional<BoundVisitor&> visitDefaultBound();
        virtual lccc::optional<PathVisitor&> visitDefaultGenericType();
    };

    struct ConceptVisitor;

    struct TypeGenericParameterVisitor : AnnotatedElementVisitor{
        explicit TypeGenericParameterVisitor(lccc::optional<TypeGenericParameterVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<ConceptVisitor&> visitConcept();
    };

    struct ConstGenericParameterVisitor : AnnotatedElementVisitor{
        explicit ConstGenericParameterVisitor(lccc::optional<ConstGenericParameterVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitType();
    };

    struct ScalarTypeVisitor;
    struct PointerTypeVisitor;

    struct GenericInstantiationVisitor;

    struct ProductTypeVisitor;
    struct SumTypeVisitor;
    struct FunctionTypeVisitor;
    struct ArrayTypeVisitor;

    struct TypeVisitor : AnnotatedElementVisitor {
        explicit TypeVisitor(lccc::optional<TypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<ScalarTypeVisitor&> visitScalarType();
        virtual lccc::optional<PointerTypeVisitor&> visitPointerType();
        virtual lccc::optional<PathVisitor&> visitNamedType();
        virtual lccc::optional<TypeDefinitionVisitor&> visitElaboratedType();
        virtual lccc::optional<GenericInstantiationVisitor&> visitGenericType();
        virtual void visitGenericParameter(uint32_t pnum);
        virtual lccc::optional<ValueVisitor&> visitAlignedAs();
        virtual lccc::optional<ProductTypeVisitor&> visitProductType();
        virtual lccc::optional<SumTypeVisitor&> visitSumType();
        virtual lccc::optional<FunctionTypeVisitor&> visitFunctionType();
        // dyn Trait
        virtual lccc::optional<ConceptVisitor&> visitErasedConceptType();
        // impl Trait or Concept at type position, includes auto
        virtual lccc::optional<ConceptVisitor&> visitReifiedConceptType();
        virtual lccc::optional<TypeVisitor&> visitSlice();
        virtual lccc::optional<ArrayTypeVisitor&> visitArray();
        virtual lccc::optional<ExprVisitor&> visitDecltype();
        virtual void visitVoid();
    };

    struct ProductTypeVisitor : Visitor{
        explicit ProductTypeVisitor(lccc::optional<ProductTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitType();
    };

    struct SumTypeVisitor : Visitor{
        explicit SumTypeVisitor(lccc::optional<SumTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitType();
    };

    struct FunctionTypeVisitor : Visitor{
        explicit FunctionTypeVisitor(lccc::optional<FunctionTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitReturnType();
        virtual lccc::optional<TypeVisitor&> visitParameterType();
        virtual void visitVarargs();
        virtual void visitLinkage(lccc::string_view lit);
    };

    struct ArrayTypeVisitor : Visitor{
        explicit ArrayTypeVisitor(lccc::optional<ArrayTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitComponentType();
        virtual lccc::optional<ValueVisitor&> visitExtent();
    };



    struct BoundGenericParameterVisitor : Visitor{
        explicit BoundGenericParameterVisitor(lccc::optional<BoundGenericParameterVisitor&> visitor=lccc::nullopt);
        virtual lccc::optional<BoundVisitor&> visitEnclosedBy();
    };

    struct GenericInstantiationVisitor : Visitor{
        explicit GenericInstantiationVisitor(lccc::optional<GenericInstantiationVisitor&> visitor=lccc::nullopt);
        virtual lccc::optional<PathVisitor&> visitGenericItem();
        virtual lccc::optional<PathVisitor&> visitGenericParameter();
        virtual lccc::optional<TypeVisitor&> visitTypeParameter();
        virtual lccc::optional<ValueVisitor&> visitConstParameter();
        virtual lccc::optional<BoundVisitor&> visitBoundParameter();
    };

    enum class PointerAliasingRule : std::uint8_t{
        None,
        Unique,
        ReadOnly,
        ReadShallow,
        Invalid,
        Nonnull,
        Volatile,
        VolatileWrite,
        NullOrInvalid,
        StrictAliasing = 0x10
    };

    enum class ValidRangeType : std::uint8_t{
        None,
        Dereference,
        DereferenceWrite,
        WriteOnly,
        NullOrDereference,
        NullOrDereferenceWrite,
        NullOrWriteOnly
    };

    enum class PointerDefinitionType : std::uint8_t {
        None = 0,
        Ref = 1,
        Const = 2,
        RValueRef = 4,
    };

    struct PointerTypeVisitor : Visitor{
        explicit PointerTypeVisitor(lccc::optional<PointerTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitPointeeType();
        virtual void visitAliasingRule(PointerAliasingRule aliasingRule);
        virtual lccc::optional<ValueVisitor&> visitValidRange(ValidRangeType validRange);
        virtual lccc::optional<ValueVisitor&> visitAligned();
        virtual lccc::optional<BoundVisitor&> visitRequiredBounds();
        virtual void visitDefinitionType(PointerDefinitionType type);
    };

    struct IntegerTypeVisitor;
    struct FloatTypeVisitor;
    struct FixedTypeVisitor;

    enum class ScalarValidity : std::uint8_t{
        Nonzero = 0x01,
        Finite = 0x02,
        Nonnan = 0x04,
    };

    struct ScalarTypeVisitor : Visitor{
        explicit ScalarTypeVisitor(lccc::optional<ScalarTypeVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<IntegerTypeVisitor&> visitIntegerType();
        virtual lccc::optional<FloatTypeVisitor&> visitFloatingPointType();
        virtual lccc::optional<FixedTypeVisitor&> visitFixedPointType();
        virtual void visitBitSize(uint16_t bits);
        virtual void visitVectorSize(uint16_t vector);
        virtual void visitComplex();
        virtual void visitValueValidity(ScalarValidity validity);
    };


    struct IntegerTypeVisitor : Visitor{
        explicit IntegerTypeVisitor(lccc::optional<IntegerTypeVisitor&> parent=lccc::nullopt);
        virtual void visitSigned();
        virtual lccc::optional<ValueVisitor&> visitMinimumValue();
        virtual lccc::optional<ValueVisitor&> visitMaximumValue();
    };

    struct FloatTypeVisitor : Visitor{
        explicit FloatTypeVisitor(lccc::optional<FloatTypeVisitor&> vparent=lccc::nullopt);
        virtual void visitDecimalFloat();
    };

    struct FixedTypeVisitor : Visitor {
        explicit FixedTypeVisitor(lccc::optional<FixedTypeVisitor&> vparent=lccc::nullopt);
        virtual void visitFractBits(std::uint16_t fractbits);
    };

    struct BoundVisitor : Visitor{
        explicit BoundVisitor(lccc::optional<BoundVisitor&> vparent=lccc::nullopt);
        virtual void visitGenericBound(std::uint32_t param);
        virtual void visitStatic();
        virtual void visitToken(std::uint64_t token);
    };


    struct ConstantVisitor;

    enum class UndefinedValueKind : std::uint16_t{
        Invalid,
        Uninitialized
    };

    struct ValueVisitor : Visitor{
        explicit ValueVisitor(lccc::optional<ValueVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<ConstantVisitor&> visitConstantValue();
        virtual lccc::optional<ExprVisitor&> visitExpression();
        virtual lccc::optional<TypeVisitor&> visitUndefined(UndefinedValueKind kind);
        virtual void visitParameter(std::uint32_t num);
    };

    struct StringLiteralVisitor;
    struct PointerConstantVisitor;
    struct ArrayConstantVisitor;

    struct ConstantVisitor : Visitor{
        explicit ConstantVisitor(lccc::optional<ConstantVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitInteger(std::intmax_t value);
        virtual lccc::optional<StringLiteralVisitor&> visitStringLiteral();
        virtual lccc::optional<PointerConstantVisitor&> visitPointerConstant();
        virtual lccc::optional<TypeVisitor&> visitExcessValueInteger(lccc::span<const uint8_t> bytes);
        virtual lccc::optional<ArrayConstantVisitor&> visitConstantArray();
        virtual lccc::optional<TypeVisitor&> visitSizeOf();
        virtual lccc::optional<TypeVisitor&> visitAlignOf();
    };

    struct ArrayConstantVisitor : Visitor{
    public:
        explicit ArrayConstantVisitor(lccc::optional<ArrayConstantVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitType();
        virtual lccc::optional<ValueVisitor&> visitRepeatLength();
        virtual lccc::optional<ValueVisitor&> visitValue();
    };

    struct PointerConstantVisitor : Visitor{
        explicit PointerConstantVisitor(lccc::optional<PointerConstantVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<PointerTypeVisitor&> visitType();
        virtual void visitNullPointer();
        virtual lccc::optional<PathVisitor&> visitGlobalAddress();
        virtual void visitLabelAddress(std::uint32_t label);
    };

    struct StringLiteralVisitor : Visitor{
        explicit StringLiteralVisitor(lccc::optional<StringLiteralVisitor&> vparent=lccc::nullopt);

        virtual lccc::optional<TypeVisitor&> visitType();
        virtual void visitByteString(lccc::string_view value);
        virtual void visitUTF8String(lccc::string_view value);
        virtual void visitUTF16String(lccc::u16string_view value);
        virtual void visitUTF32String(lccc::u32string_view value);
        virtual void visitWideString(lccc::wstring_view value);
    };

    enum class UnaryOperation : uint8_t{
        Umn = 0,
        Neg = 1,
        BNeg = 2,
        Pos = 3,
    };

    enum class BinaryOperation : uint16_t {
        Add = 0,
        Sub = 2,
        Div = 3,
        Rem = 4,
        Mod = 5,
        BAnd = 6,
        BOr = 7,
        BXor = 8,
        Subscript = 9,
        CmpEq = 10,
        CmpNe = 11,
        CmpLt = 12,
        CmpLe = 13,
        CmpGt = 14,
        CmpGe = 15,
        CmpSpaceship = 16,
        CmpRaw = 17,


        Fetch = 0x80,

        // Overflow behaviour
        Unchecked = 0x100,
        Trapping = 0x200,
        Satuturating = 0x300,
        Checked = 0x400,
    };

    constexpr BinaryOperation operator|(BinaryOperation a,BinaryOperation b){
        return static_cast<BinaryOperation>(static_cast<std::uint16_t>(a)|static_cast<std::uint16_t>(b));
    }

    enum class ConversionStrength : uint8_t {
        Weak = 0,
        Strong = 1,
        Reinterpret = 2,

        // for Weak or Strong
        Unchecked = 0x10,
        Trapping = 0x20,
        Satuturating = 0x30,
        Checked = 0x40,
    };

    constexpr ConversionStrength operator|(ConversionStrength a,ConversionStrength b){
        return static_cast<ConversionStrength>(static_cast<std::uint8_t>(a)|static_cast<std::uint8_t>(b));
    }

    enum class MemberAccessType : uint8_t {
        // lvalue %0->lvalue %1
        Struct = 0,
        // *%0->*%1
        Indirect = 1,
    };

    enum class PointerSharing : uint8_t {
        Shared = 0,
        Exclusive = 1,
        Unchecked = 2 
    };

    constexpr PointerSharing operator|(PointerSharing a,PointerSharing b) noexcept{
        return static_cast<PointerSharing>(static_cast<uint8_t>(a)|static_cast<uint8_t>(b));
    }

    enum class AccessClass : uint16_t { // Hope I didn't break anything 
        Normal = 0,
        AtomicRelaxed = 1,
        AtomicAcquire = 2,
        AtomicRelease = 3,
        AtomicAcqRel = 4,
        AtomicSeqCst = 5,
        AtomicUnordered = 6,
        Volatile = 0x10,
        Freeze = 0x20,
        NonTemporal = 0x40,
        FailRelaxed = 0x80,
    };

    constexpr AccessClass operator|(AccessClass a,AccessClass b){
        return static_cast<AccessClass>(static_cast<std::uint16_t>(a)|static_cast<std::uint16_t>(b));
    }



    enum class LValueOperation : uint8_t{
        PostInc = 0,
        PreInc = 1,
        PostDec = 2,
        PreDec = 3,
        CmpExcg = 4,
        Swap = 5,
        CmpExcgWeak = 6,

        Unchecked = 0x10,
        Trapping = 0x20,
        Satuturating = 0x30,
        Checked = 0x40,
    };

    constexpr LValueOperation operator|(LValueOperation a,LValueOperation b){
        return static_cast<LValueOperation>(static_cast<std::uint8_t>(a)|static_cast<std::uint8_t>(b));
    }



    struct BlockVisitor;

    struct LambdaVisitor;

    struct StackItemsVisitor : Visitor{
    public:
        explicit StackItemsVisitor(lccc::optional<StackItemsVisitor&> visitor=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitLvalue();
        virtual lccc::optional<TypeVisitor&> visitRvalue();
    };

    struct ExprVisitor : Visitor{
        explicit ExprVisitor(lccc::optional<ExprVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<ValueVisitor&> visitConst();
        virtual lccc::optional<FunctionTypeVisitor&> visitFunctionCall();
        virtual lccc::optional<PathVisitor&> visitMember(MemberAccessType type);
        virtual void visitLocal(uint32_t var);
        
        virtual void visitAsRvalue(AccessClass cl);
        virtual lccc::optional<TypeVisitor&> visitConversion(ConversionStrength);

        virtual void visitUnaryOperator(UnaryOperation op);
        virtual void visitBinaryOperator(BinaryOperation op);
        virtual void visitIndirection();
        virtual lccc::optional<BoundVisitor&> visitLock(PointerSharing type);
        virtual void visitPointerTo();
        virtual lccc::optional<PointerTypeVisitor&> visitDerive();
        virtual void visitDestroy();
        // compiler_fence/atomic_signal_fence or sequence point
        virtual void visitSequence(AccessClass cl);
        // fence/atomic_thread_fence
        virtual void visitFence(AccessClass cl);
        virtual void visitAssignment(AccessClass cl);
        virtual void visitCompoundAssignment(BinaryOperation op,AccessClass cl);
        virtual void visitLValue(LValueOperation op,AccessClass cl);
        virtual void visitBlockExit(uint32_t blk, uint8_t values);
        virtual void pop(uint8_t cnt);
        virtual void dup(uint8_t cnt);
        virtual lccc::optional<BlockVisitor&> visitBlock();
        virtual void visitTupleExpression(uint16_t values);
        virtual lccc::optional<TypeVisitor&> visitAggregateConstruction(uint16_t values);   
        virtual lccc::optional<LambdaVisitor&> visitLambdaExpression(uint16_t captures);
        virtual lccc::optional<StackItemsVisitor&> visitValidateStack();
    };

    

    struct CaseVisitor : Visitor{
    public:
        explicit CaseVisitor(lccc::optional<CaseVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<ValueVisitor&> visitValue();
        virtual void visitTarget(std::uint32_t item);
    };

    struct SwitchVisitor : Visitor{
    public:
        explicit SwitchVisitor(lccc::optional<SwitchVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<CaseVisitor&> visitCase();
        virtual void visitDefault(std::uint32_t item);
    };

    enum Condition : uint8_t{
        NonZero = 0,
        Less = 1,
        Greater = 2,
        Zero = 3,
        Always = 4,
        Never = 5 // Allows typechecking of the Expression Stack against a target
    };

    struct BlockVisitor : ScopeVisitor{
    public:
        explicit BlockVisitor(lccc::optional<BlockVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<ExprVisitor&> visitExpression();
        virtual lccc::optional<StackItemsVisitor&> visitTarget(std::uint32_t item);
        virtual void visitBeginTag(std::uint32_t);
        virtual void visitEndTag(std::uint32_t);
        virtual void visitBranch(std::uint32_t item,Condition condition);
        virtual lccc::optional<SwitchVisitor&> visitSwitch();
        virtual void visitBeginStorage(std::uint32_t local);
        virtual void visitEndStorage(std::uint32_t local);
    };

    struct FunctionVisitor : AnnotatedElementVisitor{
    public:
        explicit FunctionVisitor(lccc::optional<FunctionVisitor&> fnVisitor=lccc::nullopt);
        virtual lccc::optional<FunctionTypeVisitor&> visitType();
        virtual lccc::optional<BlockVisitor&> visitInitialBlock();
        virtual lccc::optional<TypeVisitor&> visitLocalVariable();
    };

    struct LambdaVisitor : Visitor{
    public:
        explicit LambdaVisitor(lccc::optional<LambdaVisitor&> vparent=lccc::nullopt);
        virtual lccc::optional<GenericParameterVisitor&> visitGenericParameter();
        virtual lccc::optional<FunctionVisitor&> visitLambdaBody();
    };

    enum class StorageClass : std::uint8_t{
        None,
        Static,
        Extern,
        Thread,
        Const
    };

    struct GlobalVariableVisitor : AnnotatedElementVisitor{
    public:
        explicit GlobalVariableVisitor(lccc::optional<GlobalVariableVisitor&> parent=lccc::nullopt);
        virtual lccc::optional<TypeVisitor&> visitVariableType();
        virtual void visitStorageClass(StorageClass cl);
    };

    struct FileVisitor : ScopeVisitor{
    public:
        explicit FileVisitor(lccc::optional<FileVisitor&> vparent=lccc::nullopt);
        virtual void visitInputFile(FILE* file);
        virtual void visitOutputFile(FILE* file);
        virtual void visitDiagnosticFile(FILE* file);
        virtual void visitTarget(lccc::Target tgt);
    };

}

#endif //XLANG_VISIT_HPP
