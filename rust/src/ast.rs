use core::ops::{Deref, DerefMut};

use crate::{interning::Symbol, lex::Lexeme, span::Span};

pub use crate::lex::StringType;

pub use crate::span::Spanned;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Visibility {
    Pub,
    Priv,
    Scoped(Spanned<SimplePath>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum CrateRef {
    Name(Symbol),
    SelfCr,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ImportName {
    /// `as $0`
    Name(Symbol),
    /// `as _`
    Ignore,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ImportTail {
    Group(Vec<Spanned<ImportItem>>),
    Wildcard,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImportItem {
    prefix: Spanned<SimplePath>,
    tail: Option<Spanned<ImportTail>>,
    asname: Option<Spanned<ImportName>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ItemBody {
    Mod(Spanned<ItemMod>),
    Value(Spanned<ItemValue>),
    ExternCrate {
        craten: Spanned<CrateRef>,
        asname: Option<Spanned<ImportName>>,
    },
    Use(Spanned<ImportItem>),
    UserType(Spanned<UserType>),
    Function(Spanned<Function>),
    ExternBlock(Spanned<ExternBlock>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Async;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub safety: Option<Spanned<Safety>>,
    pub abi: Option<Spanned<Symbol>>,
    pub constness: Option<Spanned<Mutability>>,
    pub is_async: Option<Spanned<Async>>,
    pub name: Spanned<Symbol>,
    pub generics: Option<Spanned<GenericParams>>,
    pub reciever: Option<Spanned<SelfParam>>,
    pub params: Vec<Spanned<Param>>,
    pub varargs: Option<Spanned<Varargs>>,
    pub ret_ty: Option<Spanned<Type>>,
    pub body: Option<Spanned<Block>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Param {
    pub pat: Option<Spanned<Pattern>>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SelfParam {
    BaseSelf(Option<Spanned<Type>>),
    RefSelf(Option<Spanned<Mutability>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Varargs {
    pub name: Option<Spanned<Symbol>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ExternBlock {
    pub tag: Option<Spanned<Symbol>>,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum StructKind {
    Union,
    Struct,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Constructor {
    Struct(Spanned<StructCtor>),
    Tuple(Spanned<TupleCtor>),
    Unit,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructCtor {
    pub fields: Vec<Spanned<StructField>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructField {
    pub vis: Option<Spanned<Visibility>>,
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TupleCtor {
    pub fields: Vec<Spanned<TupleField>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TupleField {
    pub vis: Option<Spanned<Visibility>>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EnumVariant {
    pub attrs: Vec<Spanned<Attr>>,
    pub ctor: Spanned<Constructor>,
    pub discriminant: Option<Spanned<Expr>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum UserTypeBody {
    Struct(Spanned<StructKind>, Spanned<Constructor>),
    Enum(Vec<Spanned<EnumVariant>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UserType {
    pub name: Spanned<Symbol>,
    pub generics: Spanned<GenericParams>,
    pub where_clauses: Option<Vec<Spanned<WhereClause>>>,
    pub body: Spanned<UserTypeBody>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericParams {
    pub params: Vec<Spanned<GenericParam>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericParam {
    Type(TypeParam),
    Lifetime(LifetimeParam),
    Const(ConstParam),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeParam {
    pub name: Spanned<Symbol>,
    pub bounds: Vec<Spanned<GenericBound>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericBound {
    LifetimeBound(Spanned<Lifetime>),
    TraitBound(Spanned<Path>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Lifetime {
    Named(Spanned<Symbol>),
    Elided,
    Static,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LifetimeParam {
    pub name: Spanned<Symbol>,
    pub bounds: Vec<Spanned<Lifetime>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ConstParam {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WhereClause {
    pub ty: Spanned<Type>,
    pub bounds: Vec<Spanned<GenericBound>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ItemMod {
    pub name: Spanned<Symbol>,
    pub content: Option<Spanned<Mod>>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum GlobalPattern {
    Name(Option<Spanned<Mutability>>, Spanned<Symbol>),
    Discard,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Static,
    Const,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ItemValue {
    pub vkind: Spanned<ValueKind>,
    pub name: Spanned<GlobalPattern>,
    pub ty: Spanned<Type>,
    pub init: Option<Spanned<Expr>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Item {
    pub vis: Option<Spanned<Visibility>>,
    pub attrs: Vec<Spanned<Attr>>,
    pub item: Spanned<ItemBody>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnType {
    pub safety: Option<Spanned<Safety>>,
    pub abi: Option<Spanned<Symbol>>,
    pub param_tys: Vec<Type>,
    pub retty: Option<Box<Spanned<Type>>>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Safety {
    Unsafe,
    Safe,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Path(Spanned<Path>),
    Reference(Option<Spanned<Mutability>>, Box<Spanned<Type>>),
    Pointer(Spanned<Mutability>, Box<Spanned<Type>>),
    Array(Box<Spanned<Type>>, Option<Box<Spanned<Expr>>>),
    FnType(FnType),
    Never,
    Tuple(Vec<Spanned<Type>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PathSegment {
    pub ident: Spanned<SimplePathSegment>,
    pub generics: Option<Spanned<GenericArgs>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericArgs {
    pub args: Vec<Spanned<GenericArg>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericArg {
    LifetimeArg(Spanned<Lifetime>),
    Type(Spanned<Type>),
    Const(Spanned<Expr>),
    Id(Spanned<Symbol>),
    AssociatedType(Spanned<Symbol>, Spanned<AssociatedTypeBound>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AssociatedTypeBound {
    Exact(Spanned<Type>),
    Bound(Vec<Spanned<GenericBound>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PathRoot {
    QSelf(Box<Spanned<Type>>, Option<Box<Spanned<Path>>>),
    Root,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path {
    pub root: Option<Spanned<PathRoot>>,
    pub segments: Vec<Spanned<PathSegment>>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Literal {
    pub val: Spanned<Symbol>,
    pub lit_kind: LiteralKind,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum LiteralKind {
    String(StringType),
    Char(StringType),
    Int(Option<Spanned<Symbol>>),
    Float(Option<Spanned<Symbol>>),
    Bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr {
    IdExpr(Spanned<Path>),
    BinaryExpr(Spanned<BinaryOp>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    UnaryExpr(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    RangeFull,
    FunctionCall {
        base: Box<Spanned<Expr>>,
        method_name: Option<Spanned<Symbol>>,
        args: Vec<Spanned<Expr>>,
    },
    MemberAccess(Box<Spanned<Expr>>, Spanned<Symbol>),
    Index {
        base: Box<Spanned<Expr>>,
        index: Box<Spanned<Expr>>,
    },
    AsCast(Box<Spanned<Expr>>, Box<Spanned<Type>>),
    BlockExpr(Spanned<CompoundBlock>),
    Literal(Spanned<Literal>),
    Break(Option<Spanned<Label>>, Option<Box<Spanned<Expr>>>),
    Continue(Option<Spanned<Label>>, Option<Box<Spanned<Expr>>>),
    Yield(Option<Box<Spanned<Expr>>>),
    ConstBlock(Spanned<Block>),
    AsyncBlock(Spanned<Block>),
    Closure(Spanned<Closure>),
    Yeet(Option<Box<Expr>>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Label {
    pub name: Spanned<Symbol>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub capture_rule: Option<Spanned<CaptureSpec>>,
    pub params: Vec<Spanned<ClosureParam>>,
    pub retty: Option<Spanned<Type>>,
    pub body: Box<Spanned<Expr>>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum CaptureSpec {
    Move,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ClosureParam {
    pub pat: Spanned<Pattern>,
    pub ty: Option<Spanned<Type>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Pattern {
    BareId(Spanned<Symbol>),
    Const(Spanned<Path>),
    Binding(Spanned<BindingPattern>),
    Discard,
    Tuple(Vec<Spanned<Pattern>>),
    Ref(Option<Spanned<Mutability>>, Box<Spanned<Pattern>>),
    OrPattern(Vec<Spanned<Pattern>>),
    RangePattern(Option<Box<Spanned<Pattern>>>, Option<Box<Spanned<Pattern>>>),
    RangeInclusivePattern(Option<Box<Spanned<Pattern>>>, Box<Spanned<Pattern>>),
    LiteralPattern(Spanned<Literal>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BindingPattern {
    pub ismut: Option<Spanned<Mutability>>,
    pub id: Spanned<Symbol>,
    pub bound_pattern: Option<Box<Spanned<Pattern>>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Spanned<Statement>>,
    pub tail_expr: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Empty, // everyone forgets about me
    DiscardExpr(Spanned<Expr>),
    ItemDecl(Spanned<Item>),
    Block(Spanned<CompoundBlock>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CompoundBlock {
    SimpleBlock(Spanned<Block>),
    If(Spanned<IfBlock>),
    While(Spanned<CondBlock>),
    Loop(Spanned<Block>),
    Unsafe(Spanned<Block>),
    For(Spanned<ForBlock>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ForBlock {
    pub pat: Box<Spanned<Pattern>>,
    pub iter: Box<Spanned<Expr>>,
    pub block: Spanned<Block>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IfBlock {
    pub cond: Box<Spanned<Expr>>,
    pub block: Spanned<Block>,
    pub elseifs: Vec<Spanned<CondBlock>>,
    pub elseblock: Option<Spanned<Block>>,
}

// Note: Include the defining keyword in the top-level span.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CondBlock {
    pub cond: Box<Spanned<Expr>>,
    pub block: Spanned<Block>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
    Less,
    Greater,
    Equal,
    LessEqual,
    GreaterEqual,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BoolAndAssign,
    BoolOrAssign,
    LeftShift,
    RightShift,
    LeftShiftAssign,
    RightShiftAssign,
    Range,
    RangeInclusive,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    RawAddrOf(Spanned<Mutability>),
    AddrOf(Option<Spanned<Mutability>>),
    Deref,
    Neg,
    Not,
    RangeFrom,
    RangeTo,
    RangeToInclusive,
    Await,
    Try,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SimplePathSegment {
    Identifier(Symbol),
    SuperPath,
    SelfPath,
    CratePath,
    MacroCratePath,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SimplePath {
    pub from_root: bool,
    pub segments: Vec<Spanned<SimplePathSegment>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AttrInput {
    DelimTokenTree(Spanned<Vec<Lexeme>>),
    MetaValue(Spanned<Literal>),
    MetaGroup(Vec<Spanned<Attr>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Attr {
    pub name: Spanned<SimplePath>,
    pub input: Option<AttrInput>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Mod {
    pub attrs: Vec<Spanned<Attr>>,
    pub items: Vec<Spanned<Item>>,
}
