#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Encoding {
    Function(Name, FunctionType),
    Data(Name),
    SpecialName(SpecialName),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Name {
    Nested(NestedName),
    Unscoped(UnscopedName),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Sub {
    Numbered(Option<i64>),
    St,
    Sa,
    Sb,
    Ss,
    Si,
    So,
    Sd,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum UnscopedName {
    Global(UnqualifiedName),
    // St3foo
    Std(UnqualifiedName),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum UnqualifiedName {
    OperatorName(Operator, Option<Vec<String>>),
    Name(String),
    StructuredBinding(Vec<String>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Prefix {
    Name(Option<Box<Prefix>>, UnqualifiedName),
    TemplateParam(Option<i64>),
    Substitution(Sub),
}
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum TemplatePrefix {
    TemplateName(Option<Prefix>, UnqualifiedName),
    TemplateParam(Option<i64>),
    Substitution(Sub),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum NestedName {
    Name(Prefix, UnqualifiedName),
    Template(TemplatePrefix, Vec<TemplateArg>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Operator {
    New,
    NewArray,
    Delete,
    DeleteArray,
    Await,
    Pos,
    Neg,
    AddrOf,
    Deref,
    Compl,
    Plus,
    Minus,
    Multiply,
    Divide,
    Rem,
    And,
    Or,
    Eor,
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    RemAssign,
    AndAssign,
    OrAssign,
    EorAssign,
    LeftShift,
    LeftShifAssign,
    RightShift,
    RightShiftAssign,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Spaceship,
    Not,
    BoolAnd,
    BoolOr,
    Inc,
    Dec,
    Comma,
    PointerToMember,
    PointerMember,
    Parethesis,
    Index,
    Question,
    Cast(Type),
    Literal(String),
    VendorQualifier(u8, String),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum SpecialName {
    VTable(Type),
    VTT(Type),
    TypeInfo(Type),
    TypeInfoName(Type),
    Thunk(Offset, Box<Encoding>),
    CovariantThunk {
        thisoff: Offset,
        retoff: Offset,
        base: Box<Encoding>,
    },
    Guard(Name),
    Temporary(Name, Option<u64>),
    TransactionSafeEntry(Box<Encoding>),
    TrackCallerThunk {
        fnname: Box<Encoding>,
        locname: Box<Encoding>,
        num: Option<u64>,
    },
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Offset {
    NonVirtual(i64),
    Virtual { thisof: i64, vcallof: i64 },
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Qualifier {
    Const,
    Volatile,
    Ref,
    RValue,
    VendorQualifier(String, Vec<TemplateArg>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Type {
    BuiltinType(BuiltinType),
    QualifiedType(Vec<Qualifier>, Box<Type>),
    FunctionType(Box<FunctionType>),
    PointerToMemberType { cl: Box<Type>, mem: Box<Type> },
    TemplateParam(Option<u64>),
    Pointer(Box<Type>),
    LValueRef(Box<Type>),
    RValueRef(Box<Type>),
    Complex(Box<Type>),
    Imaginary(Box<Type>),
    Substitution(Sub),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct FunctionType {
    pub extern_c: bool,
    pub params: Vec<Type>,
    pub ret: Option<Type>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum BuiltinType {
    Void,
    WCharT,
    Bool,
    Char,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Int128,
    UInt128,
    Float,
    Double,
    LongDouble,
    Float128,
    Ellipsis,
    Decimal64,
    Decimal128,
    Decimal32,
    Half,
    FloatN(u16),
    Char32T,
    Char16T,
    Char8T,
    Auto,
    DecltypeAuto,
    NullptrT,
    Unit,
    Tuple(Vec<Type>),
    Slice(Box<Type>),
    UnboundLifetime,
    HTRB(u32, Box<Type>),
    VendorType(String, Vec<TemplateArg>),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum TemplateArg {
    Type(Type),
}
