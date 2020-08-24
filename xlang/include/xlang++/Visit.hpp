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

    struct IdentifierVisitor : Visitor{
        explicit IdentifierVisitor(IdentifierVisitor* other=nullptr);
        virtual void visitRoot();
        virtual void visitComponent(std::string_view);
        virtual void visitSpecialComponent(std::string_view);
    };

    struct AnnotationVisitor : Visitor{
        explicit AnnotationVisitor(AnnotationVisitor* other=nullptr);
        virtual AnnotationVisitor* visitMeta();
        virtual IdentifierVisitor* visitIdentifier();
        virtual void visitItem(std::string_view);
        virtual void visitItem(std::uint64_t);
    };

    struct AnnotatedElementVisitor : Visitor{
        explicit AnnotatedElementVisitor(AnnotatedElementVisitor* other=nullptr);
        virtual AnnotationVisitor* visitAnnotation();
    };
    struct ScopeMemberVisitor;


    struct ScopeVisitor : AnnotatedElementVisitor{
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
    struct ValueVisitor;
    struct GenericMemberVisitor;
    struct GenericItemVisitor;

    struct ScopeMemberVisitor : AnnotatedElementVisitor{
    public:
        explicit ScopeMemberVisitor(ScopeMemberVisitor* other=nullptr);
        virtual void visitVisibility(Visibility);
        virtual IdentifierVisitor* visitName();
        virtual void visitStaticAssertion(const std::function<void(ExprVisitor&)>&,std::string_view);
        virtual TypeAliasVisitor* visitTypeAlias();
        virtual GenericMemberVisitor* visitGenericDeclaration();
    };

    struct GenericDeclarationVisitor : Visitor{
        explicit GenericDeclarationVisitor(GenericDeclarationVisitor* parent=nullptr);
        virtual GenericParameterVisitor* visitGenericParameter();
    };

    struct GenericMemberVisitor : GenericDeclarationVisitor{
        explicit GenericMemberVisitor(GenericMemberVisitor* parent=nullptr);
        virtual ScopeMemberVisitor* visit();
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
        virtual TypeVisitor* visitExcessValueInteger(const std::vector<uint8_t>& bytes);
        virtual ConstantVisitor* visitConstantArray(const std::function<void(ValueVisitor*)>& size);
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
        virtual void visitByteString(std::string_view value);
        virtual void visitUTF8String(std::string_view value);
        virtual void visitUTF16String(std::u16string_view value);
        virtual void visitUTF32String(std::u32string_view value);
        virtual void visitWideString(std::wstring_view value);
    };

}

#endif //XLANG_VISIT_HPP
