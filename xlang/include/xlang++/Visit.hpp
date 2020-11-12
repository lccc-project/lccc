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
#include <xlang++/Layout.h>

namespace lccc::xlang{
    /**
     * Root of the visitor hierarchy. All visitors inherit from this type.
     *
     */
    class Visitor{
    private:
        Visitor* other;
    protected:
        /**
         * Returns the parent of this visitor, if one exists and inherits from T
         * @tparam T The type of the visitor to return. Does not participate in overload resolution unless T
         *  is a (potentially cv-qualified) class type that inherits from Visitor.
         * @return
         */
        template<typename T,std::enable_if_t<std::is_base_of_v<Visitor,std::remove_cv_t<T>>>* = nullptr>
            T* get_parent(){
                return dynamic_cast<T*>(other);
            }
    public:
        /**
         * Constructs a new Visitor, with a given optional parent
         * @param other The parent of this visitor, or a null pointer. If omitted, treated as null
         */
        explicit Visitor(Visitor* other=nullptr);
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
    };

    /**
     * A visitor which visits a (potentially qualified) identifier.
     */
    struct IdentifierVisitor : Visitor{
        /**
         * Constructs an IdentifierVisitor with a given optional parent
         */
        explicit IdentifierVisitor(IdentifierVisitor* other=nullptr);
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
        explicit AnnotationVisitor(AnnotationVisitor* other=nullptr);
        /**
         * Visits a subcomponent of the annotation
         * @return A pointer to a visitor which visits the subcomponent, or null if this visitor isn't interested in visiting a subcomponent
         */
        virtual AnnotationVisitor* visitMeta();
        /**
         * Visits an (unqouted) identifier
         * @return A pointer to a visitor which visits the identifier, or null if this visitor isn't interested in visiting an identifier
         */
        virtual IdentifierVisitor* visitIdentifier();

        virtual ValueVisitor* visitItem();
    };

    /**
     * A subtype of Visitor which can have annotations
     */
    struct AnnotatedElementVisitor : Visitor{
        /**
         * Constructs a new AnnotatedElementVisitor with an optional parent
         */
        explicit AnnotatedElementVisitor(AnnotatedElementVisitor* other=nullptr);
        /**
         * Visits an annotation on this visitor.
         * This call may be repeated after the returned visitor ends its visitation.
         * @return A visitor which visits the annotation, or null if this visitor is not interested in visiting an annotation
         */
        virtual AnnotationVisitor* visitAnnotation();
    };
    struct ScopeMemberVisitor;


    /**
     * A visitor which visits a new scope.
     */
    struct ScopeVisitor : AnnotatedElementVisitor{
        /**
         * Constructs a new ScopeVisitor with an optional parent
         */
        explicit ScopeVisitor(ScopeVisitor* other=nullptr);
        /**
         * Visits a member of this scope.
         * This call may be repeated after the returned visitor ends its visitation.
         * @return A visitor which visits the member, or null if this visitor is not interested in visiting a member.
         */
        virtual ScopeMemberVisitor* visitScopeMember();
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
         * A scope member which cannot be named. m
         */
        None
    };

    struct GenericDeclarationVisitor;
    struct TypeVisitor;
    struct TypeAliasVisitor;
    struct GenericParameterVisitor;
    struct ExprVisitor;
    struct ValueVisitor;
    struct GenericMemberVisitor;
    struct GenericItemVisitor;
    struct TypeDefinitionVisitor;
    struct GlobalVariableVisitor;
    struct FunctionVisitor;
    struct TypeScopeVisitor;
    struct NamespaceVisitor;

    /**
     * A visitor which visits the member of a scope
     */
    struct ScopeMemberVisitor : AnnotatedElementVisitor{
    public:
        /**
         *  Constructs a new ScopeMemberVisitor, with an optional parent
         */
        explicit ScopeMemberVisitor(ScopeMemberVisitor* other=nullptr);
        /**
         * Visits the visibility of this member.
         */
        virtual void visitVisibility(Visibility);
        /**
         * Visits the name of this scope member.
         *  If a name visits more than one component, or the root component, the behaviour is unspecified
         * @return A visitor which visits the name of this scope member, or null if this visitor is not interested in visiting a name.
         */
        virtual IdentifierVisitor* visitName();
        /**
         * Visits a static assertion, by using value to visit the controlling expression, and visits a diagnostic if the visitation evaluates to false
         */
        virtual void visitStaticAssertion(const lccc::function<void(ValueVisitor&)>& value,lccc::string_view diag);
        /**
         * Visits an alias of a type
         * @return A visitor which visits the type alias, or null if this visitor is not interested in visiting a type alias.
         */
        virtual TypeAliasVisitor* visitTypeAlias();
        /**
         * Visits a generic declaration
         * @return  A visitor which visits the generic declaration, or null if this visitor is not interested in visiting a generic declaration.
         */
        virtual GenericMemberVisitor* visitGenericDeclaration();
        /**
         * Visits a type declaration, such as a struct, enum, or union
         * @return A visitor which visits the type definition, or null if this visitor is not interested in visiting a type declaration
         */
        virtual TypeDefinitionVisitor* visitTypeDefinition();
        /**
         * Visits a global variable (which may not actually be global, it just needs static, or thread, storage duration.)
         * @return A visitor which visits the global variable, or null if this visitor is not interested in visiting a global variable
         */
        virtual GlobalVariableVisitor* visitGlobalVariable();
        /**
         * Visits a function declaration or definition.
         * @return A visitor which visits the function, or null if this is visitor is not interested in visiting a function.
         */
        virtual FunctionVisitor* visitFunction();

        /**
         * Visits a subscope headed by the name of a type
         * @return A visitor which visits the new scope
         */
        virtual TypeScopeVisitor* visitTypeScope();
        /**
         * Visits a subscope headed by a namespace scope
         * @return A visitor which visits the new scope
         */
        virtual NamespaceVisitor* visitNamespaceScope();
    };
    /**
     * An intermediate class for generic declarations
     */
    struct GenericDeclarationVisitor : Visitor{
        /**
         * Constructs a new GenericDeclarationVisitor with an optional parent
         */
        explicit GenericDeclarationVisitor(GenericDeclarationVisitor* parent=nullptr);
        /**
         * Visits a generic parameter
         * @return A visitor which visits the parameter,
         */
        virtual GenericParameterVisitor* visitGenericParameter();
    };

    /**
     * Visitor that visits a scope member with generic parameters
     */
    struct GenericMemberVisitor : GenericDeclarationVisitor{
        /**
         * Constructs a GenericMemberVisitor with an optional parent
         */
        explicit GenericMemberVisitor(GenericMemberVisitor* parent=nullptr);
        /**
         * Visits the ScopeMember, or a null pointer if this is not interested in visiting its member.
         */
        virtual ScopeMemberVisitor* visit();
    };

    struct StructTypeVisitor;
    struct EnumTypeVisitor;
    struct UnionTypeVisitor;
    struct VariantTypeVisitor;

    /**
     * A visitor which visits the definition of a type
     */
    struct TypeDefinitionVisitor : Visitor{
    public:
        /**
         * Constructs a new TypeDefinitionVisitor with an optional parent
         */
        explicit TypeDefinitionVisitor(TypeDefinitionVisitor* visitor=nullptr);
        /**
         * Visits the name of this type
         * @return A visitor which visits the name of the type, or null if this visitor is not interested in visiting its name
         */
        virtual IdentifierVisitor* visitName();
        /**
         * Visits a structure definition
         * @return A visitor which visits the structure definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual StructTypeVisitor* visitStruct();
        /**
         * Visits a enum definition
         * @return A visitor which visits the enum definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual EnumTypeVisitor* visitEnum();
        /**
         * Visits a union definition
         * @return A visitor which visits the union definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual UnionTypeVisitor* visitUnion();
        /**
         * Visits a variant (discriminated union) definition
         * @return A visitor which visits the structure definition, or null if this visitor is not interested in visiting such a definition
         */
        virtual VariantTypeVisitor* visitVariant();
    };

    struct StructFieldVisitor;
    struct EnumeratorVisitor;
    struct VariantConstructorVisitor;

    struct StructTypeVisitor : Visitor{
    public:

    };


    struct TypeGenericParameterVisitor;
    struct ConstGenericParameterVisitor;
    struct BoundGenericParameterVisitor;
    struct BoundVisitor;

    struct GenericParameterVisitor : virtual AnnotatedElementVisitor{
        explicit GenericParameterVisitor(GenericParameterVisitor* parent=nullptr);
        virtual TypeGenericParameterVisitor* visitTypeParameter();
        virtual ConstGenericParameterVisitor* visitConstParameter();
        virtual BoundGenericParameterVisitor* visitBoundParameter();
        virtual GenericDeclarationVisitor* visitGenericType();
        virtual void visitParameterPack();
        virtual IdentifierVisitor* visitName();
        virtual TypeVisitor* visitDefaultType();
        virtual ExprVisitor* visitDefaultValue();
        virtual BoundVisitor* visitDefaultBound();
        virtual GenericItemVisitor* visitDefaultGenericType();
    };

    struct ScalarTypeVisitor;
    struct PointerTypeVisitor;

    struct GenericInstantiationVisitor;

    struct ProductTypeVisitor;
    struct SumTypeVisitor;
    struct FunctionTypeVisitor;

    struct TypeVisitor : AnnotatedElementVisitor {
        explicit TypeVisitor(TypeVisitor* parent=nullptr);
        virtual ScalarTypeVisitor* visitScalarType();
        virtual PointerTypeVisitor* visitPointerType();
        virtual IdentifierVisitor* visitNamedType();
        virtual GenericInstantiationVisitor* visitGenericType();
        virtual void visitGenericParameter(uint32_t pnum);
        virtual ValueVisitor* visitAlignedAs();
        virtual ProductTypeVisitor* visitProductType();
        virtual SumTypeVisitor* visitSumType();
        virtual FunctionTypeVisitor* visitFunctionType();
    };

    struct ProductTypeVisitor : Visitor{
        explicit ProductTypeVisitor(ProductTypeVisitor* parent=nullptr);
        virtual TypeVisitor* visitType();
    };

    struct SumTypeVisitor : Visitor{
        explicit SumTypeVisitor(SumTypeVisitor* parent=nullptr);
        virtual TypeVisitor* visitType();
    };

    struct FunctionTypeVisitor : Visitor{
        explicit FunctionTypeVisitor(FunctionTypeVisitor* parent=nullptr);
        virtual TypeVisitor* visitReturnType();
        virtual TypeVisitor* visitParameterType();
        virtual void visitVarargs();
        virtual IdentifierVisitor* visitLinkage();
        virtual IdentifierVisitor* visitTag();
    };



    struct BoundGenericParameterVisitor : Visitor{
        explicit BoundGenericParameterVisitor(BoundGenericParameterVisitor *visitor=nullptr);
        virtual BoundVisitor* visitEnclosedBy();
    };

    struct GenericInstantiationVisitor : Visitor{
        explicit GenericInstantiationVisitor(GenericInstantiationVisitor* visitor=nullptr);
        virtual GenericItemVisitor* visitGenericItem();
        virtual GenericItemVisitor* visitGenericParameter();
        virtual TypeVisitor* visitTypeParameter();
        virtual ValueVisitor* visitConstParameter();
        virtual BoundVisitor* visitBoundParameter();
    };

    enum class PointerAliasingRule : std::uint8_t{
        Unique,
        ReadOnly,
        ReadShallow,
        Invalid,
        Nonnull,
        Volatile,
        VolatileWrite,
        NullOrInvalid
    };

    enum class ValidRangeType : std::uint8_t{
        Dereference,
        DereferenceWrite,
        WriteOnly,
        NullOrDereference,
        NullOrDereferenceWrite,
        NullOrWriteOnly
    };

    struct PointerTypeVisitor : Visitor{
        explicit PointerTypeVisitor(PointerTypeVisitor* parent=nullptr);
        virtual TypeVisitor* visitPointeeType();
        virtual void visitAliasingRule(PointerAliasingRule aliasingRule);
        virtual ValueVisitor* visitValidRange(ValidRangeType validRange);
        virtual ValueVisitor* visitAligned();
        virtual BoundVisitor* visitRequiredBounds();
    };

    struct IntegerTypeVisitor;
    struct FloatTypeVisitor;

    struct ScalarTypeVisitor : Visitor{
        explicit ScalarTypeVisitor(ScalarTypeVisitor* parent=nullptr);
        virtual IntegerTypeVisitor* visitIntegerType();
        virtual FloatTypeVisitor* visitFloatingPointType();
        virtual void visitBitSize(uint16_t bits);
        virtual void visitVectorSize(uint16_t vector);
    };


    struct IntegerTypeVisitor : Visitor{
        explicit IntegerTypeVisitor(IntegerTypeVisitor* parent=nullptr);
        virtual void visitSigned();
        virtual void visitMinimumValue(std::intmax_t val);
        virtual void visitMaximumValue(std::intmax_t val);
    };

    struct FloatTypeVisitor : Visitor{
        explicit FloatTypeVisitor(FloatTypeVisitor *vparent=nullptr);
        virtual void visitComplex();
    };

    struct BoundVisitor : Visitor{
        explicit BoundVisitor(BoundVisitor *vparent=nullptr);
        virtual void visitGenericBound(std::uint32_t param);
        virtual void visitStatic();
        virtual void visitToken(std::uint64_t token);
    };


    struct ConstantVisitor;

    enum class UndefinedValueKind{
        Invalid,
        Uninitialized,
        Poison
    };

    struct ValueVisitor : Visitor{
        explicit ValueVisitor(ValueVisitor *vparent=nullptr);
        virtual ConstantVisitor* visitConstantValue();
        virtual ExprVisitor* visitExpression();
        virtual void visitUndefined(UndefinedValueKind kind);
        virtual void visitLocal(uint32_t var);

    };

    struct StringLiteralVisitor;
    struct PointerConstantVisitor;

    struct ConstantVisitor : Visitor{
        explicit ConstantVisitor(ConstantVisitor *vparent=nullptr);
        virtual TypeVisitor* visitInteger(std::intmax_t value);
        virtual StringLiteralVisitor* visitStringLiteral();
        virtual void visitBooleanLiteral(bool value);
        virtual PointerConstantVisitor* visitPointerConstant();
        virtual TypeVisitor* visitExcessValueInteger(lccc::span<uint8_t,lccc::dynamic_extent> bytes);
        virtual ConstantVisitor* visitConstantArray(const lccc::function<void(ValueVisitor*)>& size);
    };

    struct PointerConstantVisitor : Visitor{
        explicit PointerConstantVisitor(PointerConstantVisitor *vparent=nullptr);
        virtual PointerTypeVisitor* visitType();
        virtual void visitNullPointer();
        virtual IdentifierVisitor* visitGlobalAddress();
        virtual IdentifierVisitor* visitFunctionAddress();
        virtual void visitLabelAddress(std::uint32_t label);
    };

    struct StringLiteralVisitor : Visitor{
        explicit StringLiteralVisitor(PointerConstantVisitor *vparent=nullptr);
        virtual TypeVisitor* visitType();
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

    enum class BinaryOperation : uint8_t {
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
    };

    enum class ConversionStrength : uint8_t {
        Weak = 0,
        Strong = 1,
        Reinterpret = 2
    };
    enum class MemberAccessType : uint8_t {
        Struct = 0,
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

    enum class AccessClass : uint8_t {
        Normal = 0,
        AtomicRelaxed = 1,
        AtomicAcquire = 2,
        AtomicRelease = 3,
        AtomicAcqRel = 4,
        AtomicSeqCst = 5,
        Voatile = 0x10,
        Freeze = 0x20,
        Nontemporal = 0x40,
    };

    AccessClass operator|(AccessClass a,AccessClass b){
        return static_cast<AccessClass>(static_cast<uint8_t>(a)|static_cast<uint8_t>(b));
    }



    enum class UnaryLValueOperation : uint8_t{
        PostInc = 0,
        PreInc = 1,
        PostDec = 2,
        PreDec = 3
    };

    struct BlockVisitor;

    struct ExprVisitor : Visitor{
        explicit ExprVisitor(ExprVisitor *vparent=nullptr);
        virtual ValueVisitor* visitConst();
        virtual FunctionTypeVisitor* visitFunctionCall();
        virtual IdentifierVisitor* visitMember(MemberAccessType type);
        
        virtual void visitAsRvalue(AccessClass cl);
        virtual void visitAsTemporary();
        virtual TypeVisitor* visitConversion(ConversionStrength);

        virtual void visitUnaryOperator(UnaryOperation op);
        virtual void visitBinaryOperator(BinaryOperation op);
        virtual void visitIndirection();
        virtual BoundVisitor* visitLock(PointerSharing type);
        virtual void visitPointerTo();
        virtual PointerTypeVisitor* visitDerive();
        // compiler_fence/atomic_signal_fence or sequence point
        virtual void visitSequence(AccessClass cl);
        // fence/atomic_thread_fence
        virtual void visitFence(AccessClass cl);
        virtual void visitAssignment(AccessClass cl);
        virtual void visitCompoundAssignment(BinaryOperation op,AccessClass cl);
        virtual void visitUnaryLValue(UnaryLValueOperation op,AccessClass cl);
        virtual void visitBlockExit(uint32_t blk, uint8_t values);
        virtual void pop(uint8_t cnt);
        virtual void dup(uint8_t cnt);
        virtual BlockVisitor* visitBlock();
        virtual void visitTupleExpression(uint16_t values);
        virtual TypeVisitor* visitAggregateConstruction(uint16_t values);
    };

    struct StackItemsVisitor : Visitor{
    public:
        StackItemsVisitor(StackItemsVisitor* visitor=nullptr);
        TypeVisitor* visitLvalue();
        TypeVisitor* visitRvalue();
    };

    enum Condition : uint8_t{
        NonZero = 0,
        Less = 1,
        Greater = 2,
        Zero = 3,
        Always = 4,
        Never = 5
    };

    struct BlockVisitor : AnnotatedElementVisitor{
    public:
        BlockVisitor(BlockVisitor* parent=nullptr);
        virtual ExprVisitor* visitExpression();
        virtual StackItemsVisitor* visitTarget(std::uint32_t item);
        virtual void visitBranch(std::uint32_t item,Condition condition);
    }

}

#endif //XLANG_VISIT_HPP
