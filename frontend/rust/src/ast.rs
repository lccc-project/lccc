use core::fmt;

use crate::lex::Group;
use crate::{interning::Symbol, lex::Lexeme};

pub use crate::lex::{CharType, StringType};

pub use crate::span::Spanned;

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Visibility {
    Pub,
    Priv,
    Scoped(Spanned<SimplePath>),
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum CrateRef {
    Name(Symbol),
    SelfCr,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ImportName {
    /// `as $0`
    Name(Symbol),
    /// `as _`
    Ignore,
}

#[allow(dead_code)]
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
pub struct MacroRules {
    pub name: Spanned<Symbol>,
    pub body: Spanned<Group>,
}

#[allow(dead_code)]
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
    MacroRules(Spanned<MacroRules>),
    Trait(Spanned<TraitDef>),
    ImplBlock(Spanned<ImplBlock>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Auto;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TraitDef {
    pub safety: Option<Spanned<Safety>>,
    pub auto: Option<Spanned<Auto>>,
    pub name: Spanned<Symbol>,
    pub generics: Option<Spanned<GenericParams>>,
    pub supertraits: Option<Vec<Spanned<GenericBound>>>,
    pub where_clauses: Option<Spanned<Vec<Spanned<WhereClause>>>>,
    pub body: Vec<Spanned<Item>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImplBlock {
    pub safety: Option<Spanned<Safety>>,
    pub tr: Option<Spanned<Path>>,
    pub ty: Spanned<Type>,
    pub generics: Option<Spanned<GenericParams>>,
    pub where_clauses: Option<Spanned<Vec<Spanned<WhereClause>>>>,
    pub body: Vec<Spanned<Item>>,
    pub impl_id: Spanned<u64>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Async;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Extern {
    pub tag: Option<Spanned<Symbol>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function {
    pub safety: Option<Spanned<Safety>>,
    pub abi: Option<Spanned<Extern>>,
    pub constness: Option<Spanned<Mutability>>,
    pub is_async: Option<Spanned<Async>>,
    pub name: Spanned<Symbol>,
    pub generics: Option<Spanned<GenericParams>>,
    pub receiver: Option<Spanned<SelfParam>>,
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

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SelfParam {
    BaseSelf(Option<Spanned<Mutability>>, Option<Spanned<Type>>),
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

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum StructKind {
    Union,
    Struct,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Constructor {
    Struct(StructCtor),
    Tuple(TupleCtor),
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

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum UserTypeBody {
    Struct(Spanned<StructKind>, Spanned<Constructor>),
    Enum(Vec<Spanned<EnumVariant>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UserType {
    pub name: Spanned<Symbol>,
    pub generics: Option<Spanned<GenericParams>>,
    pub where_clauses: Option<Spanned<Vec<Spanned<WhereClause>>>>,
    pub body: UserTypeBody,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericParams {
    pub params: Vec<Spanned<GenericParam>>,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericBound {
    LifetimeBound(Spanned<Lifetime>),
    TraitBound(Spanned<Path>),
}

#[allow(dead_code)]
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

impl core::fmt::Display for Mutability {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Const => f.write_str("const"),
            Self::Mut => f.write_str("mut"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum GlobalPattern {
    Name(Option<Spanned<Mutability>>, Spanned<Symbol>),
    Discard,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum GenericArg {
    LifetimeArg(Spanned<Lifetime>),
    Type(Spanned<Type>),
    Const(Spanned<Expr>),
    Id(Spanned<Path>),
    AssociatedType(Spanned<Symbol>, Spanned<AssociatedTypeBound>),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AssociatedTypeBound {
    Exact(Spanned<Type>),
    Bound(Vec<Spanned<GenericBound>>),
}

#[allow(dead_code)]
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

impl Path {
    pub fn as_bare_id(&self) -> Option<Spanned<Symbol>> {
        if self.root.is_some() {
            None
        } else {
            match &*self.segments {
                [lone] => {
                    if lone.generics.is_some() {
                        None
                    } else {
                        match &lone.ident.body {
                            SimplePathSegment::Identifier(id) => {
                                Some(lone.ident.copy_span(|_| *id))
                            }
                            SimplePathSegment::SuperPath => None,
                            SimplePathSegment::SelfPath => None,
                            SimplePathSegment::CratePath => None,
                            SimplePathSegment::MacroCratePath => None,
                        }
                    }
                }
                _ => None,
            }
        }
    }

    pub fn as_bare_id_with_generics(&self) -> Option<(Spanned<Symbol>, &GenericArgs)> {
        if self.root.is_some() {
            None
        } else {
            match &*self.segments {
                [lone] => match &lone.ident.body {
                    SimplePathSegment::Identifier(id) => {
                        Some((lone.ident.copy_span(|_| *id), lone.generics.as_deref()?))
                    }
                    SimplePathSegment::SuperPath => None,
                    SimplePathSegment::SelfPath => None,
                    SimplePathSegment::CratePath => None,
                    SimplePathSegment::MacroCratePath => None,
                },
                _ => None,
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Literal {
    pub val: Spanned<Symbol>,
    pub lit_kind: LiteralKind,
}

impl core::fmt::Display for Literal {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self.lit_kind {
            LiteralKind::String(_) => {
                f.write_fmt(format_args!("\"{}\"", self.val.escape_default()))
            }
            LiteralKind::Char(_) => f.write_fmt(format_args!("'{}'", self.val.escape_default())),
            _ => f.write_str(&self.val),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum LiteralKind {
    String(StringType),
    Char(CharType),
    Int(Option<Spanned<Symbol>>),
    Float(Option<Spanned<Symbol>>),
    Bool,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ConstructorExpr {
    pub ctor_name: Spanned<Path>,
    pub fields: Vec<Spanned<FieldInit>>,
    pub fill: Option<Box<Spanned<Expr>>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FieldInit {
    pub name: Spanned<Symbol>,
    pub val: Option<Spanned<Expr>>,
}

#[allow(dead_code)]
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
    Continue(Option<Spanned<Label>>),
    Yield(Option<Box<Spanned<Expr>>>),
    ConstBlock(Spanned<Block>),
    AsyncBlock(Spanned<AsyncBlock>),
    Closure(Spanned<Closure>),
    Yeet(Option<Box<Spanned<Expr>>>),
    Constructor(Spanned<ConstructorExpr>),
    Await(Box<Spanned<Expr>>),
    Try(Box<Spanned<Expr>>),
    Group(Box<Spanned<Expr>>),
    Tuple(Vec<Spanned<Expr>>),
    Return(Option<Box<Spanned<Expr>>>),
    Array(Vec<Spanned<Expr>>),
    ArrayRepeat {
        base: Box<Spanned<Expr>>,
        len: Box<Spanned<Expr>>,
    },
    Frag(Box<Spanned<Expr>>),
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Label {
    pub name: Spanned<Symbol>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AsyncBlock {
    pub capture_rule: Option<Spanned<CaptureSpec>>,
    pub block: Spanned<Block>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
    pub capture_rule: Option<Spanned<CaptureSpec>>,
    pub params: Spanned<Vec<Spanned<ClosureParam>>>,
    pub retty: Option<Spanned<Type>>,
    pub body: Box<Spanned<Expr>>,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum CaptureSpec {
    Move,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ClosureParam {
    pub pat: Spanned<Pattern>,
    pub ty: Option<Spanned<Type>>,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Statement {
    Empty, // everyone forgets about me
    DiscardExpr(Spanned<Expr>),
    ItemDecl(Spanned<Item>),
    Block(Spanned<CompoundBlock>), // todo: should this be a statement or an expression? - yes
    LetStatement(Spanned<LetStatement>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LetStatement {
    pub name: Spanned<Pattern>,
    pub ty: Option<Spanned<Type>>,
    pub val: Option<Spanned<Expr>>,
    pub else_block: Option<Spanned<Block>>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CompoundBlock {
    SimpleBlock(Spanned<Block>),
    If(Spanned<IfBlock>),
    While(Spanned<CondBlock>),
    Loop(Spanned<Block>),
    Unsafe(Spanned<Block>),
    For(Spanned<ForBlock>),
    Match(Spanned<MatchBlock>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MatchArmValue {
    Expr(Spanned<Expr>),
    Block(Spanned<Block>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MatchArm {
    pub discrim: Spanned<Pattern>,
    pub guard: Option<Spanned<Expr>>,
    pub value: Spanned<MatchArmValue>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MatchBlock {
    pub discriminee: Box<Spanned<Expr>>,
    pub arms: Vec<Spanned<MatchArm>>,
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

#[allow(dead_code)]
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
    NotEqual,
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

#[allow(dead_code)]
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
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => f.write_str("+"),
            BinaryOp::Sub => f.write_str("-"),
            BinaryOp::Mul => f.write_str("*"),
            BinaryOp::Div => f.write_str("/"),
            BinaryOp::Rem => f.write_str("%"),
            BinaryOp::BitAnd => f.write_str("&"),
            BinaryOp::BitOr => f.write_str("|"),
            BinaryOp::BitXor => f.write_str("^"),
            BinaryOp::BoolAnd => f.write_str("&&"),
            BinaryOp::BoolOr => f.write_str("||"),
            BinaryOp::Less => f.write_str("<"),
            BinaryOp::Greater => f.write_str(">"),
            BinaryOp::Equal => f.write_str("=="),
            BinaryOp::NotEqual => f.write_str("!="),
            BinaryOp::LessEqual => f.write_str("<="),
            BinaryOp::GreaterEqual => f.write_str(">="),
            BinaryOp::Assign => f.write_str("="),
            BinaryOp::AddAssign => f.write_str("+="),
            BinaryOp::SubAssign => f.write_str("-="),
            BinaryOp::MulAssign => f.write_str("*="),
            BinaryOp::DivAssign => f.write_str("/="),
            BinaryOp::RemAssign => f.write_str("%="),
            BinaryOp::BitAndAssign => f.write_str("&="),
            BinaryOp::BitOrAssign => f.write_str("|="),
            BinaryOp::BitXorAssign => f.write_str("^="),
            BinaryOp::BoolAndAssign => f.write_str("&&="),
            BinaryOp::BoolOrAssign => f.write_str("||="),
            BinaryOp::LeftShift => f.write_str("<<"),
            BinaryOp::RightShift => f.write_str(">>"),
            BinaryOp::LeftShiftAssign => f.write_str("<<="),
            BinaryOp::RightShiftAssign => f.write_str(">>="),
            BinaryOp::Range => f.write_str(".."),
            BinaryOp::RangeInclusive => f.write_str("..="),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => f.write_str("-"),
            _ => todo!(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum SimplePathSegment {
    Identifier(Symbol),
    SuperPath,
    SelfPath,
    CratePath,
    MacroCratePath,
}

impl core::fmt::Display for SimplePathSegment {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            SimplePathSegment::Identifier(id) => f.write_str(id),
            SimplePathSegment::SuperPath => f.write_str("super"),
            SimplePathSegment::SelfPath => f.write_str("self"),
            SimplePathSegment::CratePath => f.write_str("crate"),
            SimplePathSegment::MacroCratePath => f.write_str("$crate"),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SimplePath {
    pub from_root: bool,
    pub segments: Vec<Spanned<SimplePathSegment>>,
}

impl core::fmt::Display for SimplePath {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let mut sep = if self.from_root { "::" } else { "" };

        for seg in &self.segments {
            f.write_str(sep)?;
            sep = "::";
            seg.body.fmt(f)?;
        }
        Ok(())
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum AttrInput {
    DelimTokenTree(Spanned<Vec<Lexeme>>),
    MetaValue(Spanned<Literal>),
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
