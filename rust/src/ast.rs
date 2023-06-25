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
pub enum ItemBody {
    Mod(Spanned<ItemMod>),
    Value(Spanned<ItemValue>),
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
    pub vis: Spanned<Visibility>,
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
pub enum PathSegment {
    Identifier(Symbol),
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
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}
