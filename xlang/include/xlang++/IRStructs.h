#ifndef XLANG_IRSTRUCTS_H_2021_05_26_08_12_53
#define XLANG_IRSTRUCTS_H_2021_05_26_08_12_53

#include <xlang++/layout/UniquePtr.hpp>
#include <xlang++/layout/Variant.hpp>
#include <xlang++/layout/Vector.hpp>
#include <xlang++/layout/Pair.hpp>
#include <xlang++/layout/UnorderedMap.hpp>
#include <xlang++/Visit.hpp>

namespace lccc::xlang
{

    enum class BoundKind : std::uint8_t{
        Static,
        Tag,
        Generic,
    };


    struct Bound{
        lccc::variant<BoundKind,lccc::monostate,std::uint32_t,std::uint32_t> value;

        void visit(BoundVisitor *visitor);
    };

    struct Type;

    struct Value;

    enum class GenericParameterKind : std::uint8_t{
        Type,
        Value,
        Bound
    };

    struct GenericParameter{
        lccc::variant<lccc::unique_ptr<Type>,lccc::unique_ptr<Value>,lccc::unique_ptr<Bound>> bound;
    };

    struct IdentifierComponentKind : std::uint8_t{
        None,
        Root,
        Component,
        Dependent,
        Args,
        SpecialComponent
    };

    struct Identifier{
        lccc::vector<lccc::variant<IdentifierComponentKind,lccc::monostate,lccc::monostate,lccc::string,lccc::vector<GenericParameter>,lccc::string>> components;
        void visit (IdentifierVisitor *visitor);
    };

    enum class ValueClass : std::uint8_t{
        RValue,
        LValue
    }

    struct StackItems{
        lccc::vector<lccc::pair<ValueClass,lccc::unique_ptr<Type>>> items;

        void visit(StackItemsVisitor *visitor);
    };

    enum class TypeKind : std::uint16_t {
        None,
        Integer,
        FloatingPoint,
        PointerType,
        NamedType,
        ElaboratedType,
        GenericInstantiation,
        GenericParameter,
        AlignedAs,
        ProductType,
        SumType,
        FunctionType,
        ErasedConcept,
        ReifiedConcept,
        Slice,
        Array,
        Decltype,
        Void,
    };

    struct ScopeMember;

    struct Scope{
        lccc::unordered_map<Identifier,lccc::unique_ptr<ScopeMember>> members;
    };

    struct Type;

    struct ScalarType{
        std::uint16_t bits;
        std::uint16_t vector_size;
        ScalarValidity validity;
        bool complex;

        void visit(ScalarTypeVisitor* visitor);
    };

    struct IntegerType {
        ScalarType header; // Would love for this to be a base class, but then it wouldn't be standard-layout
        lccc::unique_ptr<Value> min_value;
        lccc::unique_ptr<Value> max_value;
        bool is_signed;

        void visit(ScalarTypeVisitor* visitor);
    };

    struct FloatType{
        ScalarType header;
        bool is_decimal;

        void visit(ScalarTypeVisitor* visitor);
    };

    struct PointerType{
        lccc::unique_ptr<Type> pointee;
        PointerAliasingRule alias_rule;
        ValidRangeType valid_range;
        lccc::unique_ptr<Value> valid_range_value;
        PointerDeclarationType decl_type;
        lccc::unique_ptr<Value> aligned;
        Bound bound;
    };

    struct FunctionType{
        lccc::unique_ptr<Type> ret_type;
        lccc::vector<Type> param_types;
        lccc::optional<lccc::string> linkage;


        ~FunctionType();
    }


    enum class ExpressionKind : std::uint16_t {
        None,
        Const,
        FunctionCall,
        Member,
        Local,
        AsRvalue,
        Convert,
        UnaryOperator,
        BinaryOperator,
        Indirection,
        Lock,
        PointerTo,
        Destroy,
        Sequence,
        Fence,
        Assign,
        CompoundAssign,
        LValue,
        BlockExit,
        Pop,
        Dup,
        Block,
        TupleExpr,
        AggregateCtor,
        LambdaExpr,
        ValidateStack,
    }; 

    struct Expression{
        lccc::variant<
            XLangExpressionKind,
            lccc::monostate,
            lccc::unique_ptr<Value>,
            
        > expr;
    };

    enum class ValueKind : std::uint16_t{
        None = 0,
        Undefined = 1,
        Scalar = 2,
        WideScalar = 3,
        String = 4,
        UTF8String = 5,
        UTF16String = 6,
        UTF32String = 7,
        WideString = 8,
        Array = 9,
        SizeOf = 10,
        AlignOf = 11,
        Nullptr = 12,
        GlobalAddress = 13,
        LabelAddress = 14,
        Complex = 15,
        Parameter = 16,
    };
}

#endif /* XLANG_IRSTRUCTS_H_2021_05_26_08_12_53 */