use core::ops::{Deref, DerefMut};

use crate::{interning::Symbol, lex::Lexeme, span::Span};

#[derive(Debug, Copy, Clone)]
pub struct Spanned<T> {
    pub body: T,
    pub span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.body
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.body
    }
}

#[derive(Debug)]
pub enum Visibility {
    Pub,
    Priv,
    Scoped(Spanned<SimplePath>),
}

#[derive(Debug)]
pub enum CrateRef {
    Name(Symbol),
    SelfCr,
}

#[derive(Debug)]
pub enum ImportName {
    /// `as $0`
    Name(Symbol),
    /// `as _`
    Ignore,
}

#[derive(Debug)]
pub enum ImportTail {
    Group(Vec<Spanned<ImportItem>>),
    Wildcard,
}

#[derive(Debug)]
pub struct ImportItem {
    prefix: Spanned<SimplePath>,
    tail: Option<Spanned<ImportTail>>,
    asname: Option<Spanned<ImportName>>,
}

#[derive(Debug)]
pub enum ItemBody {
    Mod(Spanned<ItemMod>),
    Value(Spanned<ItemValue>),
    ExternCrate {
        craten: Spanned<CrateRef>,
        asname: Option<Spanned<ImportName>>,
    },
    Use(Spanned<ImportItem>),
    UserType(UserType),
}

#[derive(Debug)]
pub enum StructKind {
    Union,
    Struct,
}

#[derive(Debug)]
pub enum Constructor {
    Struct(Spanned<StructCtor>),
    Tuple(Spanned<TupleCtor>),
    Unit,
}

#[derive(Debug)]
pub struct StructCtor {
    pub fields: Vec<Spanned<StructField>>,
}

#[derive(Debug)]
pub struct StructField {
    pub vis: Option<Spanned<Visibility>>,
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug)]
pub struct TupleCtor {
    pub fields: Vec<Spanned<TupleField>>,
}

#[derive(Debug)]
pub struct TupleField {
    pub vis: Option<Spanned<Visibility>>,
    pub ty: Spanned<Type>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub attrs: Vec<Spanned<Attr>>,
    pub ctor: Spanned<Constructor>,
    pub discriminant: Option<Spanned<Expr>>,
}

#[derive(Debug)]
pub enum UserTypeBody {
    Struct(Spanned<StructKind>, Spanned<Constructor>),
    Enum(Vec<Spanned<EnumVariant>>),
}

#[derive(Debug)]
pub struct UserType {
    pub name: Spanned<Symbol>,
    pub generics: Spanned<GenericParams>,
    pub where_clauses: Option<Vec<Spanned<WhereClause>>>,
    pub body: Spanned<UserTypeBody>,
}

#[derive(Debug)]
pub struct GenericParams {
    pub params: Vec<Spanned<GenericParam>>,
}

#[derive(Debug)]
pub enum GenericParam {
    Type(TypeParam),
    Lifetime(LifetimeParam),
    Const(ConstParam),
}

#[derive(Debug)]
pub struct TypeParam {
    pub name: Spanned<Symbol>,
    pub bounds: Vec<Spanned<GenericBound>>,
}

#[derive(Debug)]
pub enum GenericBound {
    LifetimeBound(Spanned<Lifetime>),
    TraitBound(Spanned<Path>),
}

#[derive(Debug)]
pub enum Lifetime {
    Named(Spanned<Symbol>),
    Elided,
    Static,
}

#[derive(Debug)]
pub struct LifetimeParam {
    pub name: Spanned<Symbol>,
    pub bounds: Vec<Spanned<Lifetime>>,
}

#[derive(Debug)]
pub struct ConstParam {
    pub name: Spanned<Symbol>,
    pub ty: Spanned<Type>,
}

#[derive(Debug)]
pub struct WhereClause {
    pub ty: Spanned<Type>,
    pub bounds: Vec<Spanned<GenericBound>>,
}

#[derive(Debug)]
pub struct ItemMod {
    pub name: Spanned<Symbol>,
    pub content: Option<Spanned<Mod>>,
}

#[derive(Debug)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Debug)]
pub enum GlobalPattern {
    Name(Option<Spanned<Mutability>>, Spanned<Symbol>),
    Discard,
}

#[derive(Debug)]
pub enum ValueKind {
    Static,
    Const,
}

#[derive(Debug)]
pub struct ItemValue {
    pub vkind: Spanned<ValueKind>,
    pub name: Spanned<GlobalPattern>,
    pub ty: Spanned<Type>,
    pub init: Option<Spanned<Expr>>,
}

#[derive(Debug)]
pub struct Item {
    pub vis: Option<Spanned<Visibility>>,
    pub attrs: Vec<Spanned<Attr>>,
    pub item: Spanned<ItemBody>,
}

#[derive(Debug)]
pub struct FnType {
    pub safety: Option<Spanned<Safety>>,
    pub abi: Option<Spanned<Symbol>>,
    pub param_tys: Vec<Type>,
    pub retty: Option<Box<Spanned<Type>>>,
}

#[derive(Debug)]
pub enum Safety {
    Unsafe,
    Safe,
}

#[derive(Debug)]
pub enum Type {
    Path(Spanned<Path>),
    Reference(Option<Spanned<Mutability>>, Box<Spanned<Type>>),
    Pointer(Spanned<Mutability>, Box<Spanned<Type>>),
    Array(Box<Spanned<Type>>, Option<Spanned<Expr>>),
    FnType(FnType),
}

#[derive(Debug)]
pub enum PathPrefix {
    SimplePrefix(Option<Spanned<SimplePathSegment>>),
    SelfTy,
    QSelf(Box<Spanned<Type>>, Option<Box<Spanned<Path>>>),
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Spanned<Symbol>,
    pub generics: Option<Spanned<GenericArgs>>,
}

#[derive(Debug)]
pub struct GenericArgs {
    pub args: Vec<Spanned<GenericArg>>,
}

#[derive(Debug)]
pub enum GenericArg {
    LifetimeArg(Spanned<Lifetime>),
    Type(Spanned<Type>),
    Const(Spanned<Expr>),
    Id(Spanned<Symbol>),
    AssociatedType(Spanned<Symbol>, Spanned<AssociatedTypeBound>),
}

#[derive(Debug)]
pub enum AssociatedTypeBound {
    Exact(Spanned<Type>),
    Bound(Vec<Spanned<GenericBound>>),
}

#[derive(Debug)]
pub struct Path {
    pub prefix: Option<Spanned<PathPrefix>>,
    pub segments: Vec<Spanned<PathSegment>>,
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct Block {
    pub stats: Vec<Spanned<Statement>>,
    pub tail_expr: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug)]
pub enum Statement {
    DiscardExpr(Spanned<Expr>),
    ItemDecl(Spanned<Item>),
    Block(Spanned<CompoundBlock>),
}

#[derive(Debug)]
pub enum CompoundBlock {
    SimpleBlock(Spanned<Block>),
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum SimplePathSegment {
    Identifier(Symbol),
    SuperPath,
    SelfPath,
    CratePath,
    MacroCratePath,
}

#[derive(Debug)]
pub struct SimplePath {
    pub from_root: bool,
    pub segments: Vec<Spanned<SimplePathSegment>>,
}

#[derive(Debug)]
pub enum AttrInput {
    DelimTokenTree(Vec<Lexeme>, Span),
}

#[derive(Debug)]
pub struct Attr {
    pub name: Spanned<SimplePath>,
    pub input: Option<AttrInput>,
}

#[derive(Debug)]
pub struct Mod {
    pub attrs: Vec<Spanned<Attr>>,
    pub items: Vec<Spanned<Item>>,
}
