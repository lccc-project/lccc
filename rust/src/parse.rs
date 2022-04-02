#![allow(dead_code, clippy::module_name_repetitions)] // For now
use peekmore::{PeekMore, PeekMoreIterator};
use xlang::prelude::v1::HashMap;

use crate::{
    lex::{GroupType, Lexeme, TokenType},
    macro_parse::{MacroArm, MacroMatcher, MacroOutput},
};

pub use crate::lex::{CharType, StrType};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Visibility {
    None,
    Pub,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BoundKind {
    Negative,
    Maybe,
    Normal,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeTag {
    Struct,
    Union,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Safety {
    Safe,
    Unsafe,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SimplePath {
    pub root: bool,
    pub idents: Vec<String>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Item {
    ExternBlock {
        attrs: Vec<Meta>,
        abi: Option<String>,
        items: Vec<Self>,
    },
    FnDeclaration {
        attrs: Vec<Meta>,
        visibility: Visibility,
        is_const: bool,
        is_async: bool,
        safety: Safety,
        abi: Option<String>,
        name: String,
        generics: Generics,
        params: Vec<FnParam>,
        return_ty: Option<Type>,
        block: Option<Vec<BlockItem>>,
    },
    MacroExpansion {
        attrs: Vec<Meta>,
        target: SimplePath,
        args: Vec<Lexeme>,
    },
    MacroRules {
        attrs: Vec<Meta>,
        visibility: Option<Visibility>, // pub macro_rules!
        name: String,
        arms: Vec<MacroArm>,
    },
    Type(Struct),
    Mod {
        name: String,
        vis: Visibility,
        content: Mod,
    },
    Adt {
        attrs: Vec<Meta>,
        name: String,
        generics: Generics,
        vis: Visibility,
        variants: Vec<EnumVariant>,
    },
    TypeAlias {
        attrs: Vec<Meta>,
        vis: Visibility,
        name: String,
        generics: Generics,
        defn: Option<Type>,
    },
    Trait {
        attrs: Vec<Meta>,
        vis: Visibility,
        safety: Safety,
        auto: bool,
        name: String,
        generics: Generics,
        supertraits: Vec<GenericBound>,
        body: TraitBody,
    },
    Impl {
        attrs: Vec<Meta>,
        safety: Safety,
        generics: Generics,
        tr: Option<TraitBound>,
        ty: Type,
        body: Vec<Item>,
    },
    Static {
        attrs: Vec<Meta>,
        vis: Visibility,
        name: Pattern,
        init: Expr,
    },
    Const {
        attrs: Vec<Meta>,
        vis: Visibility,
        name: Pattern,
        init: Option<Expr>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_bounds: Vec<WhereBound>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct WhereBound {
    pub ty: Type,
    pub bounds: Vec<GenericBound>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TraitBody {
    Alias(Vec<GenericBound>),
    Block(Vec<Item>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitImplName {
    const_kind: TraitBoundConst,
    negative_impl: bool,
    name: Path,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericBound {
    Lifetime(Lifetime),
    Trait(TraitBound),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TraitBoundConst {
    NoConst,
    ConstIfConst,
    Const,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitBound {
    pub unbound: BoundKind,
    pub const_kind: TraitBoundConst,
    pub trait_name: Path,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericParam {
    Lifetime(String, Vec<Lifetime>),
    Type(String, Vec<GenericBound>),
    Const(String, Type),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumVariant {
    pub attrs: Vec<Meta>,
    pub name: String,
    pub ctor: StructBody,
    pub discrim: Option<Expr>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField {
    pub attrs: Vec<Meta>,
    pub vis: Visibility,
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleField {
    pub attrs: Vec<Meta>,
    pub vis: Visibility,
    pub ty: Type,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub attrs: Vec<Meta>,
    pub vis: Visibility,
    pub tag: TypeTag,
    pub name: String,
    pub generics: Generics,
    pub body: StructBody,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum StructBody {
    Struct(Vec<StructField>),
    Tuple(Vec<TupleField>),
    Unit,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Meta {
    Ident(SimplePath),
    String(String),
    IntLit(i128),
    Group(SimplePath, Vec<Self>),
    KeyValue(SimplePath, Box<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FnParam {
    pub pat: Option<Pattern>,
    pub ty: Option<Type>, // Only for closures/...
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Pattern {
    Discard,
    Ident(String),
    Binding(String, Box<Pattern>),
    Ref(Mutability, Box<Pattern>),
    Unref(Mutability, Box<Pattern>),
    Parentheses(Box<Pattern>),
    Tuple(Vec<Pattern>),
    TupleStruct(Path, Vec<Pattern>),
    Struct(Path, Vec<FieldPattern>),
    Const(Path),
    RangeInclusive(Option<Box<Pattern>>, Option<Box<Pattern>>),
    RangeExclusive(Option<Box<Pattern>>, Option<Box<Pattern>>),
    Slice(Vec<Pattern>),
    StringLiteral(StrType, String),
    CharLiteral(StrType, String),
    IntLiteral(u128),
    DotDotDot,                      // ...: VaList
    Or(Box<Pattern>, Box<Pattern>), // pat1 | pat2
    SelfPat,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldPattern {
    Rest,
    Field(FieldName, Option<Pattern>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockItem {
    Item(Box<Item>),
    Expr(Expr),
    Discard(Expr),
    Let {
        pattern: Pattern,
        ty: Option<Type>,
        value: Option<Expr>,
    },
    MacroExpansion {
        target: SimplePath,
        args: Vec<Lexeme>,
        terminator: bool,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathRoot {
    Root,
    CrateRoot(SimplePath, u128),
    QSelf(Box<Type>, Option<Box<Path>>),
    SelfPath,
    Super,
    Crate,
    SelfTy,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Lifetime {
    Static,
    Inferred,
    Bound(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArg {
    Name(Path),
    Type(Type),
    Expr(Expr),
    Lifetime(Lifetime),
    AssociatedType(String, Type),
    AssociatedTypeBounds(String, Vec<GenericBound>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    Id(String),
    Generics(Vec<GenericArg>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path {
    pub root: Option<PathRoot>,
    pub components: Vec<PathComponent>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ElseBlock {
    Else(Vec<BlockItem>),
    ElseIf {
        control: Box<Expr>,
        block: Vec<BlockItem>,
    },
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    Try,
    Neg,
    Deref,
    Not,
    Ref(Mutability),
    RawRef(Mutability),
    RangeTo,
    RangeFrom,
    RangeToInclusive,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Range,
    RangeInclusive,
    BooleanOr,
    BooleanAnd,
    CompareEq,
    CompareNe,
    CompareLt,
    CompareGt,
    CompareLe,
    CompareGe,
    And,
    Or,
    Xor,
    Lsh,
    Rsh,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Assign,
    AndAssign,
    OrAssign,
    XorAssign,
    LshAssign,
    RshAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    UnsafeBlock(Vec<BlockItem>),
    Block(Vec<BlockItem>),
    Loop(Vec<BlockItem>),
    While {
        control: Box<Expr>,
        block: Vec<BlockItem>,
    },
    If {
        control: Box<Expr>,
        block: Vec<BlockItem>,
        elses: Vec<ElseBlock>,
    },
    LetExpr(Pattern, Option<Type>, Box<Expr>),
    Id(Path),
    FunctionCall {
        func: Box<Self>,
        args: Vec<Self>,
    },
    Cast(Box<Self>, Type),
    StringLiteral(StrType, String),
    CharLiteral(CharType, String),
    Parentheses(Box<Self>),
    MacroExpansion {
        target: SimplePath,
        args: Vec<Lexeme>,
    },
    IntLiteral(i128),
    StructConstructor(Path, Vec<StructFieldInitializer>),
    Field(Box<Expr>, FieldName),
    Await(Box<Expr>),
    Return(Option<Box<Expr>>),
    Break(Option<Lifetime>, Option<Box<Expr>>),
    Continue(Option<Lifetime>),
    Yield(Option<Box<Expr>>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    ArrayIndex {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    ArrayCtor(Box<ArrayCtor>),
    TypeAscription(Box<Expr>, Type),
    Try(Box<Expr>),
    RangeFull,
    TupleCtor(Vec<Expr>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ArrayCtor {
    Repeat { expr: Expr, quantity: Expr },
    List(Vec<Expr>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldName {
    Id(String),
    Tuple(u32),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructFieldInitializer {
    pub name: FieldName,
    pub expr: Expr,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Name(Path),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Never,
    Tuple(Vec<Type>),
    Wildcard,
    Reference {
        mutability: Mutability,
        lifetime: Option<Lifetime>,
        underlying: Box<Self>,
    },
    Slice(Box<Type>),
    Array(Box<Type>, Box<Expr>),
}

pub fn parse_simple_path<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> SimplePath {
    let mut root = false;
    let mut path = Vec::new();
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "::" => {
            it.next();
            root = true;
        }
        _ => {}
    }
    loop {
        match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Identifier,
                tok,
                ..
            } => {
                path.push(tok);
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "::" => {
                        it.next();
                        continue;
                    }
                    _ => break SimplePath { root, idents: path },
                }
            }
            tok => panic!("Unexpected Token {:?}", tok),
        }
    }
}

pub fn parse_meta<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Meta {
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Identifier,
            ..
        } => {
            let path = parse_simple_path(it);
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "=" => {
                    it.next();
                    Meta::KeyValue(path, Box::new(parse_meta(it)))
                }
                Some(Lexeme::Group {
                    ty: GroupType::Parentheses,
                    ..
                }) => match it.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Parentheses,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut group = Vec::new();
                        loop {
                            group.push(parse_meta(&mut it));
                            match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == "," => continue,
                                Some(tok) => panic!("Unexpected token {:?}", tok),
                                None => break,
                            }
                        }
                        Meta::Group(path, group)
                    }
                    _ => unreachable!(),
                },
                _ => Meta::Ident(path),
            }
        }
        Lexeme::Token {
            ty: TokenType::Number,
            tok,
            ..
        } => {
            let val = tok.parse().unwrap();
            it.next();
            Meta::IntLit(val)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "-" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Number,
                    tok,
                    ..
                } => Meta::IntLit(-tok.parse::<i128>().unwrap()),
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::String(_),
            ..
        } => match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::String(_),
                tok,
                ..
            } => Meta::String(tok),
            _ => unreachable!(),
        },
        tok => panic!("Unexpected Token {:?}", tok),
    }
}

fn parse_macro_matcher<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
) -> Option<MacroMatcher> {
    match it.next()? {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "$" => todo!("Macro matcher fragement/repetition"),
        tok @ Lexeme::Token { .. } => Some(MacroMatcher::RawToken(tok)),
        Lexeme::Group { ty, inner, .. } => {
            let mut it = inner.into_iter().peekmore();
            Some(MacroMatcher::Group(
                ty,
                core::iter::from_fn(move || parse_macro_matcher(&mut it)).collect(),
            ))
        }
    }
}

fn parse_macro_output<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
) -> Option<MacroOutput> {
    match it.next()? {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "$" => todo!("Macro matcher fragement/repetition"),
        tok @ Lexeme::Token { .. } => Some(MacroOutput::RawToken(tok)),
        Lexeme::Group { ty, inner, .. } => {
            let mut it = inner.into_iter().peekmore();
            Some(MacroOutput::Group(
                ty,
                core::iter::from_fn(move || parse_macro_output(&mut it)).collect(),
            ))
        }
    }
}

pub fn parse_visibility<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Visibility {
    match it.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        }) if tok == "pub" => {
            it.next();
            match it.peek() {
                Some(Lexeme::Group {
                    ty: GroupType::Parentheses,
                    ..
                }) => todo!("pub(<path>)"),
                _ => Visibility::Pub,
            }
        }
        _ => Visibility::None,
    }
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
pub fn parse_item<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    mut attrs: Vec<Meta>,
) -> Option<Item> {
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "#" => {
                it.next();
                match it.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Brackets,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        attrs.push(parse_meta(&mut it));
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                }
            }
            _ => break,
        }
    }
    let vis = parse_visibility(it);
    match it.peek()? {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } if tok == "macro_rules" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "!" => match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => match it.next().unwrap() {
                        Lexeme::Group {
                            ty: GroupType::Braces,
                            inner,
                            ..
                        } => {
                            let name = tok;
                            let mut it = inner.into_iter().peekmore();
                            let mut arms = Vec::new();
                            loop {
                                let mut matchers = Vec::new();
                                let mut outputs = Vec::new();
                                match it.next() {
                                    Some(Lexeme::Group { inner, .. }) => {
                                        let mut it = inner.into_iter().peekmore();
                                        while let Some(matcher) = parse_macro_matcher(&mut it) {
                                            matchers.push(matcher);
                                        }
                                    }
                                    Some(tok) => panic!("Unexpected Token {:?}", tok),
                                    None => {
                                        break Some(Item::MacroRules {
                                            attrs,
                                            visibility: None,
                                            name,
                                            arms,
                                        })
                                    }
                                }
                                match it.next().unwrap() {
                                    Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    } if tok == "=>" => match it.next().unwrap() {
                                        Lexeme::Group {
                                            ty: GroupType::Braces,
                                            inner,
                                            ..
                                        } => {
                                            let mut it = inner.into_iter().peekmore();
                                            while let Some(output) = parse_macro_output(&mut it) {
                                                outputs.push(output);
                                            }
                                        }
                                        tok => panic!("Unexpected Token {:?}", tok),
                                    },
                                    tok => panic!("Unexpected token {:?}", tok),
                                }

                                arms.push(MacroArm {
                                    matchers,
                                    expansion: outputs,
                                });
                                match it.next() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == ";" => continue,
                                    Some(tok) => panic!("Unexpected Token {:?}", tok),
                                    None => {
                                        break Some(Item::MacroRules {
                                            attrs,
                                            visibility: None,
                                            name,
                                            arms,
                                        })
                                    }
                                }
                            }
                        }
                        tok => panic!("Unexpected Token {:?}", tok),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },

                tok => panic!(
                    "Unexpected Token {:?} (Note: macro_rules::foo! not yet handled)",
                    tok
                ),
            }
        }
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } if tok == "union" => {
            let union = tok.clone();
            it.next();
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "::" => {
                    if vis != Visibility::None {
                        panic!("Cannot apply visibility {:?} to a macro expansion", vis)
                    }
                    it.next();
                    let mut path = parse_simple_path(it);
                    path.idents.insert(0, union);
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == "!" => match it.next().unwrap() {
                            Lexeme::Group {
                                ty: GroupType::Braces,
                                inner,
                                ..
                            } => Some(Item::MacroExpansion {
                                attrs,
                                target: path,
                                args: inner,
                            }),
                            Lexeme::Group { inner, .. } => {
                                match it.next().unwrap() {
                                    Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    } if tok == ";" => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                };
                                Some(Item::MacroExpansion {
                                    attrs,
                                    target: path,
                                    args: inner,
                                })
                            }
                            tok => panic!("Unexpected token {:?}", tok),
                        },
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                }
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "!" => {
                    if vis != Visibility::None {
                        panic!("Cannot apply visibility {:?} to a macro expansion", vis)
                    }
                    let path = SimplePath {
                        root: false,
                        idents: vec![union],
                    };
                    match it.next().unwrap() {
                        Lexeme::Group {
                            ty: GroupType::Braces,
                            inner,
                            ..
                        } => Some(Item::MacroExpansion {
                            attrs,
                            target: path,
                            args: inner,
                        }),
                        Lexeme::Group { inner, .. } => {
                            match it.next().unwrap() {
                                Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                } if tok == ";" => {}
                                tok => panic!("Unexpected token {:?}", tok),
                            };
                            Some(Item::MacroExpansion {
                                attrs,
                                target: path,
                                args: inner,
                            })
                        }
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                }
                _ => Some(Item::Type(parse_struct_or_union(
                    vis,
                    attrs,
                    it,
                    TypeTag::Union,
                ))),
            }
        }
        Lexeme::Token {
            ty: TokenType::Identifier,
            ..
        } => {
            let path = parse_simple_path(it);
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "!" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => Some(Item::MacroExpansion {
                    attrs,
                    target: path,
                    args: inner,
                }),
                Lexeme::Group { inner, .. } => {
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == ";" => {}
                        tok => panic!("Unexpected token {:?}", tok),
                    };
                    Some(Item::MacroExpansion {
                        attrs,
                        target: path,
                        args: inner,
                    })
                }
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } => match &**tok {
            "extern" => {
                match it.peek_next() {
                    Some(Lexeme::Token {
                        ty: TokenType::String(_),
                        ..
                    }) => match it.peek_next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Keyword,
                            tok,
                            ..
                        }) if tok == "fn" => {
                            it.reset_cursor();
                            return Some(parse_fn_item(vis, attrs, it));
                        }
                        _ => {}
                    },
                    Some(Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    }) if tok == "fn" => {
                        it.reset_cursor();
                        return Some(parse_fn_item(vis, attrs, it));
                    }
                    _ => {}
                }
                it.reset_cursor();
                it.next();
                if vis != Visibility::None {
                    panic!("extern block with visibility is invalid");
                }
                let abi = match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::String(_),
                        ..
                    }) => {
                        if let Some(Lexeme::Token {
                            ty: TokenType::String(_),
                            tok,
                            ..
                        }) = it.next()
                        {
                            Some(tok)
                        } else {
                            unreachable!()
                        }
                    }
                    _ => Some("C".to_string()),
                };

                let block = it.next().expect("Missing block");

                match block {
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
                        ..
                    } => {
                        let mut iter = inner.into_iter().peekmore();
                        let mut items = Vec::new();
                        while let Some(item) = parse_item(&mut iter, Vec::new()) {
                            items.push(item);
                        }
                        Some(Item::ExternBlock { attrs, abi, items })
                    }
                    tok => panic!("Invalid tokens for block {:?}", tok),
                }
            }
            "unsafe" => match it.peek_next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "extern" || tok == "fn" => {
                    it.reset_cursor();
                    Some(parse_fn_item(vis, attrs, it))
                }
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "trait" => {
                    it.reset_cursor();
                    Some(parse_trait_item(vis, attrs, it))
                }
                Lexeme::Token {
                    ty: TokenType::Identifier,
                    tok,
                    ..
                } if tok == "auto" => {
                    it.reset_cursor();
                    Some(parse_trait_item(vis, attrs, it))
                }
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "impl" => {
                    it.reset_cursor();
                    if vis != Visibility::None {
                        panic!("Cannot use visibility {:?} on impl item", vis);
                    }
                    Some(parse_impl_item(attrs, it))
                }
                tok => panic!("Unexpected token {:?}", tok),
            },
            "impl" => {
                if vis != Visibility::None {
                    panic!("Cannot use visibility {:?} on impl item", vis);
                }
                Some(parse_impl_item(attrs, it))
            }
            "fn" | "async" => Some(parse_fn_item(vis, attrs, it)),
            "const" => match it.peek_next() {
                Some(Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                }) if tok == "unsafe" || tok == "async" || tok == "extern" || tok == "fn" => {
                    it.reset_cursor();
                    Some(parse_fn_item(vis, attrs, it))
                }
                _ => {
                    it.reset_cursor();
                    it.next();
                    let name = parse_pattern(it).unwrap();
                    let init = match it.peek().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == "=" => {
                            it.next();
                            Some(parse_expr(it, true))
                        }
                        _ => None,
                    };

                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == ";" => {}
                        tok => panic!("Unexpected token {:?}", tok),
                    }

                    Some(Item::Const {
                        attrs,
                        vis,
                        name,
                        init,
                    })
                }
            },
            "static" => {
                it.next();
                let name = parse_pattern(it).unwrap();
                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == "=" => {}
                    tok => panic!("Unexpected token {:?}", tok),
                }
                let init = parse_expr(it, true);

                Some(Item::Static {
                    attrs,
                    vis,
                    name,
                    init,
                })
            }
            "auto" | "trait" => Some(parse_trait_item(vis, attrs, it)),
            "struct" => Some(Item::Type(parse_struct_or_union(
                vis,
                attrs,
                it,
                TypeTag::Struct,
            ))),
            "union" => Some(Item::Type(parse_struct_or_union(
                vis,
                attrs,
                it,
                TypeTag::Union,
            ))),
            "mod" => {
                it.next();
                let name = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => tok,
                    tok => panic!("Unexpected Token {:?}", tok),
                };

                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ";" => todo!("Out of line modules"),
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
                        ..
                    } => Some(Item::Mod {
                        name,
                        vis,
                        content: parse_mod(inner.into_iter(), attrs),
                    }),
                    tok => panic!("Unexpected Token {:?}", tok),
                }
            }
            "enum" => {
                it.next();
                let name = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => tok,
                    tok => panic!("Unexpected Token {:?}", tok),
                };
                let params = match it.peek().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == "<" => {
                        it.next();
                        parse_generic_decl(it)
                    }
                    _ => Vec::new(),
                };
                let mut where_bounds = Vec::new();
                match it.peek().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    } if tok == "where" => {
                        it.next();
                        loop {
                            match it.peek() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == ";" => break,
                                Some(Lexeme::Group {
                                    ty: GroupType::Braces,
                                    ..
                                }) => break,
                                None => break,
                                Some(_) => {}
                            }
                            let ty = parse_type(it).unwrap();
                            let bounds = match it.next().unwrap() {
                                Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                } if tok == ":" => {
                                    it.next();
                                    parse_bounds(it)
                                }
                                tok => panic!("Unexpected token {:?}", tok),
                            };
                            where_bounds.push(WhereBound { ty, bounds })
                        }
                    }
                    _ => {}
                }

                let generics = Generics {
                    params,
                    where_bounds,
                };

                match it.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut variants = Vec::new();
                        loop {
                            if it.peek().is_none() {
                                break;
                            }
                            let mut attrs = Vec::new();
                            loop {
                                match it.peek() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == "#" => {
                                        it.next();
                                        match it.next().unwrap() {
                                            Lexeme::Group {
                                                ty: GroupType::Brackets,
                                                inner,
                                                ..
                                            } => {
                                                let mut it = inner.into_iter().peekmore();
                                                attrs.push(parse_meta(&mut it));
                                            }
                                            tok => panic!("Unexpected token {:?}", tok),
                                        }
                                    }
                                    _ => break,
                                }
                            }
                            let name = match it.next().unwrap() {
                                Lexeme::Token {
                                    ty: TokenType::Identifier,
                                    tok,
                                    ..
                                } => tok,
                                tok => panic!("Unexpected Token {:?}", tok),
                            };
                            let ctor = parse_struct_body(&mut it);
                            let discrim = match it.peek() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == "=" => {
                                    it.next();
                                    Some(parse_expr(&mut it, true))
                                }
                                _ => None,
                            };
                            variants.push(EnumVariant {
                                attrs,
                                name,
                                ctor,
                                discrim,
                            });
                            match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == "," => continue,
                                None => break,
                                Some(tok) => panic!("Unexpected Token {:?}", tok),
                            }
                        }
                        Some(Item::Adt {
                            attrs,
                            name,
                            generics,
                            vis,
                            variants,
                        })
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                }
            }
            _ => todo!(),
        },
        tok => todo!("{:?}", tok),
    }
}

pub fn parse_struct_or_union<I: Iterator<Item = Lexeme>>(
    vis: Visibility,
    attrs: Vec<Meta>,
    it: &mut PeekMoreIterator<I>,
    tag: TypeTag,
) -> Struct {
    it.next(); // eat struct/union
    let name = match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => tok,
        tok => panic!("Unexpected Token {:?}", tok),
    };

    let params = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => parse_generic_decl(it),
        _ => Vec::new(),
    };

    let mut where_bounds = Vec::new();
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "where" => {
            it.next();
            loop {
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == ";" => break,
                    Some(Lexeme::Group {
                        ty: GroupType::Braces,
                        ..
                    }) => break,
                    None => break,
                    Some(_) => {}
                }
                let ty = parse_type(it).unwrap();
                let bounds = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {
                        it.next();
                        parse_bounds(it)
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                };
                where_bounds.push(WhereBound { ty, bounds })
            }
        }
        _ => {}
    }

    let generics = Generics {
        params,
        where_bounds,
    };

    let body = parse_struct_body(it);

    match body {
        StructBody::Tuple(_) | StructBody::Unit => match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == ";" => {}
            tok => panic!("Unexpected token {:?}", tok),
        },
        _ => {}
    }

    Struct {
        attrs,
        vis,
        tag,
        name,
        generics,
        body,
    }
}

pub fn parse_struct_body<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> StructBody {
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ";" || tok == "," => return StructBody::Unit,
        _ => {}
    }

    match it.next().unwrap() {
        Lexeme::Group {
            ty: GroupType::Parentheses,
            inner,
            ..
        } => {
            let mut it = inner.into_iter().peekmore();
            // Tuple
            let mut fields = Vec::new();
            loop {
                let mut attrs = Vec::new();
                match it.peek() {
                    None => break StructBody::Tuple(fields),
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "#" => loop {
                        match it.peek() {
                            Some(Lexeme::Token {
                                ty: TokenType::Symbol,
                                tok,
                                ..
                            }) if tok == "#" => {
                                it.next();
                                match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Brackets,
                                        inner,
                                        ..
                                    } => {
                                        let mut it = inner.into_iter().peekmore();
                                        attrs.push(parse_meta(&mut it));
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => break,
                        }
                    },
                    Some(_) => {}
                }
                let vis = parse_visibility(&mut it);
                let ty = parse_type(&mut it).unwrap();
                fields.push(TupleField { attrs, vis, ty });
                match it.next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "," => continue,
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                    None => break StructBody::Tuple(fields),
                }
            }
        }
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
            ..
        } => {
            let mut it = inner.into_iter().peekmore();
            // Tuple
            let mut fields = Vec::new();
            loop {
                let mut attrs = Vec::new();
                match it.peek() {
                    None => break StructBody::Struct(fields),
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "#" => loop {
                        match it.peek() {
                            Some(Lexeme::Token {
                                ty: TokenType::Symbol,
                                tok,
                                ..
                            }) if tok == "#" => {
                                it.next();
                                match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Brackets,
                                        inner,
                                        ..
                                    } => {
                                        let mut it = inner.into_iter().peekmore();
                                        attrs.push(parse_meta(&mut it));
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => break,
                        }
                    },
                    Some(_) => {}
                }
                let vis = parse_visibility(&mut it);
                let name = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => tok,
                    tok => panic!("Unexpected token {:?}", tok),
                };
                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {}
                    tok => panic!("Unexpected token {:?}", tok),
                }
                let ty = parse_type(&mut it).unwrap();
                fields.push(StructField {
                    attrs,
                    vis,
                    name,
                    ty,
                });
                match it.next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "," => continue,
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                    None => break StructBody::Struct(fields),
                }
            }
        }
        tok => panic!("Unexpected token {:?}", tok),
    }
}

pub fn parse_fn_item<I: Iterator<Item = Lexeme>>(
    visibility: Visibility,
    mut attrs: Vec<Meta>,
    peek: &mut PeekMoreIterator<I>,
) -> Item {
    let is_const = match peek.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        }) if tok == "const" => {
            peek.next();
            true
        }
        _ => false,
    };
    let is_async = match peek.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        }) if tok == "async" => {
            peek.next();
            true
        }
        _ => false,
    };
    let safety = match peek.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        }) if tok == "unsafe" => {
            peek.next();
            Safety::Unsafe
        }
        _ => Safety::Safe,
    };
    let abi = match peek.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        }) if tok == "extern" => {
            peek.next();
            match peek.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::String(StrType::Default),
                    tok,
                    ..
                } => Some(tok),
                _ => Some("C".to_string()),
            }
        }
        _ => None,
    };

    match peek.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "fn" => {}
        tok => panic!("Unexpected token {:?}", tok),
    }
    let name = match peek.next().expect("Invalid Item") {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => tok,
        _ => panic!("Invalid Token"),
    };

    let generic_params = match peek.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => {
            peek.next();
            parse_generic_decl(peek)
        }
        _ => Vec::new(),
    };

    let params = if let Some(Lexeme::Group {
        ty: GroupType::Parentheses,
        inner,
        ..
    }) = peek.next()
    {
        let mut peek = inner.into_iter().peekmore();
        let mut params = Vec::new();
        loop {
            if peek.peek().is_some() {
                params.push(parse_fn_param(&mut peek));
            } else {
                break;
            }

            match peek.next() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "," => continue,
                Some(tok) => panic!("Unexpected Token {:?}", tok),
                None => break,
            }
        }
        params
    } else {
        panic!("Invalid Token")
    };

    let retty = match peek.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "->" => {
            peek.next();
            Some(parse_type(peek).unwrap())
        }
        _ => None,
    };

    let mut where_bounds = Vec::new();
    match peek.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "where" => {
            peek.next();
            loop {
                match peek.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == ";" => break,
                    Some(Lexeme::Group {
                        ty: GroupType::Braces,
                        ..
                    }) => break,
                    None => break,
                    Some(_) => {}
                }
                let ty = parse_type(peek).unwrap();
                let bounds = match peek.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {
                        peek.next();
                        parse_bounds(peek)
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                };
                where_bounds.push(WhereBound { ty, bounds })
            }
        }
        _ => {}
    }

    let generics = Generics {
        params: generic_params,
        where_bounds,
    };

    let body = match peek.next().unwrap() {
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
            ..
        } => {
            let mut it = inner.into_iter().peekmore();
            loop {
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "#" => {
                        match it.peek_next().unwrap() {
                            Lexeme::Token {
                                ty: TokenType::Symbol,
                                tok,
                                ..
                            } if tok == "!" => {
                                it.next();
                                it.next();
                                match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Brackets,
                                        inner,
                                        ..
                                    } => {
                                        let mut it = inner.into_iter().peekmore();
                                        attrs.push(parse_meta(&mut it));
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => break, // No need to validate this is actually #[attr], parsing items will handle that
                        }
                    }
                    _ => break,
                }
            }
            it.reset_cursor();
            Some(parse_block(it))
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ";" => None,
        tok => panic!("Unexpected token {:?}", tok),
    };
    Item::FnDeclaration {
        attrs,
        visibility,
        is_const,
        is_async,
        safety,
        abi,
        name,
        generics,
        params,
        return_ty: retty,
        block: body,
    }
}

pub fn parse_path_in_type<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Path {
    let mut path = parse_path(it);
    if !matches!(path.components.last(), Some(PathComponent::Generics(_))) {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "<" => {
                it.next();
                path.components
                    .push(PathComponent::Generics(parse_generics(it)))
            }
            _ => {}
        }
    }
    path
}

pub fn parse_trait_item<I: Iterator<Item = Lexeme>>(
    vis: Visibility,
    attrs: Vec<Meta>,
    it: &mut PeekMoreIterator<I>,
) -> Item {
    let safety = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "unsafe" => {
            it.next();
            Safety::Unsafe
        }
        _ => Safety::Safe,
    };
    let auto = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } if tok == "auto" => {
            it.next();
            true
        }
        _ => false,
    };

    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "trait" => {}
        tok => panic!("Unexpected token {:?}", tok),
    }

    let name = match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => tok,
        tok => panic!("Unexpected token {:?}", tok),
    };

    let params = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => {
            it.next();
            parse_generic_decl(it)
        }
        _ => Vec::new(),
    };

    let supertraits = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ":" => {
            it.next();
            parse_bounds(it)
        }
        _ => Vec::new(),
    };

    let mut where_bounds = Vec::new();
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "where" => {
            it.next();
            loop {
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == ";" => break,
                    Some(Lexeme::Group {
                        ty: GroupType::Braces,
                        ..
                    }) => break,
                    None => break,
                    Some(_) => {}
                }
                let ty = parse_type(it).unwrap();
                let bounds = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {
                        it.next();
                        parse_bounds(it)
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                };
                where_bounds.push(WhereBound { ty, bounds })
            }
        }
        _ => {}
    }

    let generics = Generics {
        params,
        where_bounds,
    };

    let body = match it.next().unwrap() {
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
            ..
        } => {
            let mut it = inner.into_iter().peekmore();
            let items = core::iter::from_fn(move || parse_item(&mut it, Vec::new())).collect();
            TraitBody::Block(items)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "=" => TraitBody::Alias(parse_bounds(it)),
        tok => panic!("Unexpected token {:?}", tok),
    };
    Item::Trait {
        attrs,
        vis,
        safety,
        auto,
        name,
        generics,
        supertraits,
        body,
    }
}

pub fn parse_impl_item<I: Iterator<Item = Lexeme>>(
    mut attrs: Vec<Meta>,
    it: &mut PeekMoreIterator<I>,
) -> Item {
    let safety = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "unsafe" => {
            it.next();
            Safety::Unsafe
        }
        _ => Safety::Safe,
    };

    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "impl" => {}
        tok => panic!("Unexpected token {:?}"),
    }

    let generics = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => {
            let mut is_path_prefix = false;
            match it.peek_next() {
                Some(Lexeme::Token {
                    ty: TokenType::Identifier,
                    ..
                }) => match it.peek_next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == ":" || tok == "," || tok == ">" => {}
                    _ => is_path_prefix = true,
                },
                Some(Lexeme::Token {
                    ty: TokenType::Lifetime,
                    ..
                }) => {}
                _ => is_path_prefix = true,
            }
            it.reset_cursor();
            if is_path_prefix {
                Vec::new()
            } else {
                it.next();
                parse_generic_decl(it)
            }
        }
        _ => Vec::new(),
    };

    let (tr, ty) = match it.peek().unwrap() {
        Lexeme::Group { .. } => (None, parse_type(it).unwrap()),
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "~" || tok == "?" => {
            let bound = parse_trait_bound(it);
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "for" => {}
                tok => panic!("Unexpected token {:?}"),
            }
            (Some(bound), parse_type(it).unwrap())
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "self" || tok == "crate" || tok == "super" => {
            let path = parse_path_in_type(it);
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                }) if tok == "for" => {
                    it.next();
                    (
                        Some(TraitBound {
                            unbound: BoundKind::Normal,
                            const_kind: TraitBoundConst::NoConst,
                            trait_name: path,
                        }),
                        parse_type(it).unwrap(),
                    )
                }
                _ => (None, Type::Name(path)),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "const" => {
            let bound = parse_trait_bound(it);
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "for" => {}
                tok => panic!("Unexpected token {:?}"),
            }
            (Some(bound), parse_type(it).unwrap())
        }
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => {
            let path = parse_path_in_type(it);
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                }) if tok == "for" => {
                    it.next();
                    (
                        Some(TraitBound {
                            unbound: BoundKind::Normal,
                            const_kind: TraitBoundConst::NoConst,
                            trait_name: path,
                        }),
                        parse_type(it).unwrap(),
                    )
                }
                _ => (None, Type::Name(path)),
            }
        }
        _ => (None, parse_type(it).unwrap()),
    };

    let mut where_bounds = Vec::new();
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "where" => {
            it.next();
            loop {
                let ty = match parse_type(it) {
                    Some(ty) => ty,
                    None => break,
                };
                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {}
                    tok => panic!("Unexpected token {:?}", tok),
                }
                let bounds = parse_bounds(it);
                where_bounds.push(WhereBound { ty, bounds });
                match it.next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "," => continue,
                    _ => break,
                }
            }
        }
        _ => {}
    }

    let body = match it.next().unwrap() {
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
            ..
        } => {
            let mut it = inner.into_iter().peekmore();
            let mut body = Vec::new();

            loop {
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == "#" => {
                        match it.peek_next().unwrap() {
                            Lexeme::Token {
                                ty: TokenType::Symbol,
                                tok,
                                ..
                            } if tok == "!" => {
                                it.next();
                                it.next();
                                match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Brackets,
                                        inner,
                                        ..
                                    } => {
                                        let mut it = inner.into_iter().peekmore();
                                        attrs.push(parse_meta(&mut it));
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => break, // No need to validate this is actually #[attr], parsing items will handle that
                        }
                    }
                    _ => break,
                }
            }
            it.reset_cursor();
            while let Some(x) = parse_item(&mut it, Vec::new()) {
                body.push(x);
            }
            body
        }
        tok => panic!("Unexpected token {:?}", tok),
    };

    Item::Impl {
        attrs,
        safety,
        generics: Generics {
            params: generics,
            where_bounds,
        },
        tr,
        ty,
        body,
    }
}

pub fn parse_lifetime<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
) -> Option<Lifetime> {
    match it.peek()? {
        Lexeme::Token {
            ty: TokenType::Lifetime,
            tok,
            ..
        } if tok == "static" => Some(Lifetime::Static),
        Lexeme::Token {
            ty: TokenType::Lifetime,
            tok,
            ..
        } if tok == "_" => Some(Lifetime::Inferred),
        Lexeme::Token {
            ty: TokenType::Lifetime,
            tok,
            ..
        } => {
            let name = tok.clone();
            it.next();
            Some(Lifetime::Bound(name))
        }
        _ => None,
    }
}

pub fn parse_trait_bound<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> TraitBound {
    let unbound = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "?" => {
            it.next();
            BoundKind::Maybe
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "!" => {
            it.next();
            BoundKind::Negative
        }
        _ => BoundKind::Normal,
    };

    let const_kind = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "const" => {
            it.next();
            TraitBoundConst::Const
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "~" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "const" => TraitBoundConst::ConstIfConst,
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        _ => TraitBoundConst::NoConst,
    };

    let trait_name = parse_path_in_type(it);

    TraitBound {
        unbound,
        const_kind,
        trait_name,
    }
}

pub fn parse_bounds<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Vec<GenericBound> {
    let mut ret = Vec::new();
    match it.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        }) if tok == "," || tok == ";" || tok == "=" => return ret,
        Some(Lexeme::Group {
            ty: GroupType::Braces,
            ..
        }) => return ret,
        None => return ret,
        Some(_) => {}
    }
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Lifetime,
                tok,
                ..
            }) => ret.push(GenericBound::Lifetime(parse_lifetime(it).unwrap())),
            None => break,
            Some(_) => ret.push(GenericBound::Trait(parse_trait_bound(it))),
        }

        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "+" => {
                it.next();
                continue;
            }
            _ => break,
        }
    }
    ret
}

pub fn parse_generic_decl<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
) -> Vec<GenericParam> {
    let mut params = Vec::new();
    loop {
        match it.peek().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == ">" => break,
            Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
                ..
            } if tok == "const" => {
                it.next();
                let name = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => tok,
                    tok => panic!("Unexpected Token {:?}", tok),
                };
                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {}
                    tok => panic!("Unexpected Token {:?}", tok),
                };
                let ty = parse_type(it).unwrap();
                params.push(GenericParam::Const(name, ty));
            }
            Lexeme::Token {
                ty: TokenType::Lifetime,
                ..
            } => {
                let name = match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Lifetime,
                        tok,
                        ..
                    } if tok != "static" && tok != "_" => tok,
                    tok => panic!("Unexpected token {:?}", tok),
                };
                let mut bounds = Vec::new();
                match it.peek().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {
                        it.next();
                        loop {
                            if let Some(life) = parse_lifetime(it) {
                                bounds.push(life);
                                match it.peek().unwrap() {
                                    Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    } if tok == "+" => continue,
                                    _ => break,
                                }
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {}
                }
                params.push(GenericParam::Lifetime(name, bounds))
            }
            Lexeme::Token {
                ty: TokenType::Identifier,
                tok,
                ..
            } => {
                let name = tok.clone();
                it.next();
                let bounds = match it.peek().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == ":" => {
                        it.next();
                        parse_bounds(it)
                    }
                    _ => Vec::new(),
                };
                params.push(GenericParam::Type(name, bounds));
            }
            tok => panic!("Unexpected token {:?}", tok),
        }

        match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == "," => continue,
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == ">" => break,
            tok => panic!("Unexpected token {:?}", tok),
        }
    }

    params
}

pub fn parse_generics<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Vec<GenericArg> {
    let mut args = Vec::new();
    loop {
        match it.peek().unwrap() {
            Lexeme::Token {
                ty: TokenType::Lifetime,
                ..
            } => args.push(GenericArg::Lifetime(parse_lifetime(it).unwrap())),
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == ">" => {
                it.next();
                return args;
            }
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == "<" || tok == "::" => args.push(GenericArg::Name(parse_path(it))),
            Lexeme::Token {
                ty: TokenType::Identifier,
                ..
            } => match it.peek_next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "=" => {
                    it.reset_cursor();
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Identifier,
                            tok,
                            ..
                        } => {
                            it.next();
                            let ty = parse_type(it).unwrap();
                            args.push(GenericArg::AssociatedType(tok, ty));
                        }
                        _ => unreachable!(),
                    }
                }
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == ":" => {
                    it.reset_cursor();
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Identifier,
                            tok,
                            ..
                        } => {
                            it.next();
                            let bounds = parse_bounds(it);
                            args.push(GenericArg::AssociatedTypeBounds(tok, bounds));
                        }
                        _ => unreachable!(),
                    }
                }
                _ => {
                    it.reset_cursor();
                    args.push(GenericArg::Name(parse_path(it)));
                }
            },
            Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
                ..
            } if tok == "self" || tok == "super" || tok == "crate" || tok == "Self" => {
                args.push(GenericArg::Name(parse_path(it)))
            }
            Lexeme::Group {
                ty: GroupType::Braces,
                ..
            } => {
                args.push(GenericArg::Expr(parse_expr(it, true)));
            }
            _ => args.push(GenericArg::Type(parse_type(it).unwrap())),
        }

        match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == ">" => return args,
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == "," => continue,
            tok => panic!("Unexpected token {:?}", tok),
        }
    }
}

pub fn parse_path<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Path {
    let root = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "::" => {
            it.next();
            Some(PathRoot::Root)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => {
            it.next();
            let ty = parse_type(it).unwrap();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "as" => {
                    let inner = parse_path(it);
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == ">" => {}
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == "::" => {}
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                    Some(PathRoot::QSelf(Box::new(ty), Some(Box::new(inner))))
                }
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == ">" => {
                    match it.next().unwrap() {
                        Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        } if tok == "::" => {}
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                    Some(PathRoot::QSelf(Box::new(ty), None))
                }
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "self" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "::" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            Some(PathRoot::SelfPath)
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "Self" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "::" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            Some(PathRoot::SelfPath)
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "crate" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "::" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            Some(PathRoot::Crate)
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "super" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == "::" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            Some(PathRoot::Super)
        }
        _ => None,
    };
    let mut components = Vec::new();
    loop {
        match it.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Identifier,
                tok,
                ..
            } => components.push(PathComponent::Id(tok)),
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == "<" => components.push(PathComponent::Generics(parse_generics(it))),
            tok => panic!("Unexpected Token {:?}", tok),
        }
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "::" => {
                it.next();
                continue;
            }
            _ => break Path { root, components },
        }
    }
}

pub fn parse_pattern<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Option<Pattern> {
    match it.peek()? {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "_" => {
            it.next();
            Some(Pattern::Discard)
        }
        Lexeme::Group { .. } => match it.next().unwrap() {
            Lexeme::Group {
                ty: GroupType::Parentheses,
                inner,
                ..
            } => {
                let mut iter = inner.into_iter().peekmore();
                if let Some(pat2) = parse_pattern(&mut iter) {
                    match iter.next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        }) if tok == "," => {
                            let mut inner = vec![pat2];
                            while let Some(pat) = parse_pattern(&mut iter) {
                                inner.push(pat);
                                match iter.next() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == "," => {}
                                    None => break,
                                    Some(tok) => panic!("Unexpected token: {:?}", tok),
                                }
                            }
                            Some(Pattern::Tuple(inner))
                        }
                        Some(tok) => panic!("Unexpected Token: {:?}", tok),
                        None => Some(Pattern::Parentheses(Box::new(pat2))),
                    }
                } else {
                    Some(Pattern::Tuple(vec![]))
                }
            }
            Lexeme::Group {
                ty: GroupType::Brackets,
                ..
            } => todo!("slice patterns"),
            tok => panic!("Unexpected token {:?}", tok),
        },
        _ => {
            let path = parse_path(it);

            match it.peek() {
                Some(Lexeme::Group { .. }) => match it.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Parentheses,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut inner = Vec::new();
                        loop {
                            if let Some(pat) = parse_pattern(&mut it) {
                                inner.push(pat)
                            } else {
                                break;
                            }

                            match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == "," => continue,
                                Some(tok) => panic!("Unexepected token {:?}", tok),
                                None => break,
                            }
                        }
                        Some(Pattern::TupleStruct(path, inner))
                    }
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut inner = Vec::new();

                        loop {
                            let name = match it.next() {
                                None => break,
                                Some(Lexeme::Token {
                                    ty: TokenType::Identifier,
                                    tok,
                                    ..
                                }) => FieldName::Id(tok),
                                Some(Lexeme::Token {
                                    ty: TokenType::Number,
                                    tok,
                                    ..
                                }) => FieldName::Tuple(tok.parse().unwrap()),
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == ".." => {
                                    if let Some(tok) = it.next() {
                                        panic!("Unexpected token {:?}", tok)
                                    }
                                    inner.push(FieldPattern::Rest);
                                    break;
                                }
                                Some(tok) => panic!("Unexpected token {:?}", tok),
                            };

                            let pat = match it.peek() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == ":" => {
                                    it.next();
                                    Some(parse_pattern(&mut it).unwrap())
                                }
                                _ => None,
                            };

                            inner.push(FieldPattern::Field(name, pat));

                            match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                }) if tok == "," => continue,
                                Some(tok) => panic!("Unexpected token {:?}", tok),
                                None => break,
                            }
                        }

                        Some(Pattern::Struct(path, inner))
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                },
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "@" => {
                    let pat = parse_pattern(it).unwrap();
                    match (&path.root, &*path.components) {
                        (None, [PathComponent::Id(id)]) => {
                            Some(Pattern::Binding(id.clone(), Box::new(pat)))
                        }
                        _ => panic!("Cannot create pinding with path {:?}", path),
                    }
                }
                _ => {
                    if path.root == Some(PathRoot::SelfPath) && path.components.is_empty() {
                        Some(Pattern::SelfPat)
                    } else if path.root == None && path.components.len() == 1 {
                        match &*path.components {
                            [PathComponent::Id(id)] => Some(Pattern::Ident(id.clone())),
                            _ => unreachable!(),
                        }
                    } else {
                        Some(Pattern::Const(path))
                    }
                }
            }
        }
    }
}

pub fn parse_type<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Option<Type> {
    match it.peek()? {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "!" => {
            it.next();
            Some(Type::Never)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "_" => {
            it.next();
            Some(Type::Wildcard)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "*" => {
            it.next();
            let mutability = match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "const" => Mutability::Const,
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "mut" => Mutability::Mut,
                tok => panic!("Unexpected token {:?}", tok),
            };

            let underlying = parse_type(it).unwrap();
            Some(Type::Pointer {
                mutability,
                underlying: Box::new(underlying),
            })
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "&" => {
            it.next();
            let lifetime = parse_lifetime(it);
            let mutability = match it.peek().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "mut" => {
                    it.next();
                    Mutability::Mut
                }
                _ => Mutability::Const,
            };

            let underlying = parse_type(it).unwrap();
            Some(Type::Reference {
                mutability,
                lifetime,
                underlying: Box::new(underlying),
            })
        }
        Lexeme::Group {
            ty: GroupType::Brackets,
            ..
        } => match it.next().unwrap() {
            Lexeme::Group {
                ty: GroupType::Brackets,
                inner,
                ..
            } => {
                let mut it = inner.into_iter().peekmore();
                let ty = parse_type(&mut it).unwrap();
                match it.next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) if tok == ";" => {
                        let expr = parse_expr(&mut it, true);
                        Some(Type::Array(Box::new(ty), Box::new(expr)))
                    }
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                    None => Some(Type::Slice(Box::new(ty))),
                }
            }
            _ => unreachable!(),
        },
        Lexeme::Group {
            ty: GroupType::Parentheses,
            ..
        } => match it.next().unwrap() {
            Lexeme::Group {
                ty: GroupType::Parentheses,
                inner,
                ..
            } => {
                let mut it = inner.into_iter().peekmore();
                if let Some(ty) = parse_type(&mut it) {
                    match it.next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        }) if tok == "," => {
                            let mut tys = vec![ty];
                            while let Some(ty) = parse_type(&mut it) {
                                tys.push(ty);
                                match it.next() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == "," => continue,
                                    Some(tok) => panic!("Unexpected Token {:?}", tok),
                                    None => break,
                                }
                            }
                            Some(Type::Tuple(tys))
                        }
                        Some(tok) => panic!("Unexpected Token {:?}", tok),
                        None => Some(ty),
                    }
                } else {
                    Some(Type::Tuple(vec![]))
                }
            }
            _ => unreachable!(),
        },
        _ => Some(Type::Name(parse_path_in_type(it))),
    }
}

pub fn parse_fn_param<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> FnParam {
    let pat = parse_pattern(it).unwrap();

    let ty = match it.peek() {
        Some(Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        }) if tok == ":" => {
            it.next();
            Some(parse_type(it).unwrap())
        }
        _ => None,
    };

    FnParam { pat: Some(pat), ty }
}

pub fn parse_block<I: Iterator<Item = Lexeme>>(it: I) -> Vec<BlockItem> {
    let mut peek = it.peekmore();
    let mut ret = Vec::new();
    loop {
        match peek.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
                ..
            }) => match &**tok {
                "pub" | "fn" | "static" | "const" | "type" | "struct" | "union" | "extern" => {
                    ret.push(BlockItem::Item(Box::new(
                        parse_item(&mut peek, Vec::new()).unwrap(),
                    )));
                }
                "let" => {
                    peek.next();
                    let pattern = parse_pattern(&mut peek).unwrap();
                    let value;
                    let ty;
                    match peek.next().unwrap() {
                        Lexeme::Token { tok, .. } if tok == ";" => {
                            ty = None;
                            value = None;
                        }
                        Lexeme::Token { tok, .. } if tok == "=" => {
                            ty = None;
                            value = Some(parse_expr(&mut peek, true));
                            assert!(
                                matches!(peek.next(), Some(Lexeme::Token { tok, .. }) if tok == ";"),
                                "Expected semicolon"
                            );
                        }
                        Lexeme::Token { tok, .. } if tok == ":" => {
                            ty = Some(parse_type(&mut peek).unwrap());
                            match peek.next().unwrap() {
                                Lexeme::Token { tok, .. } if tok == ";" => {
                                    value = None;
                                }
                                Lexeme::Token { tok, .. } if tok == "=" => {
                                    value = Some(parse_expr(&mut peek, true));
                                    assert!(
                                        matches!(peek.next(), Some(Lexeme::Token { tok, .. }) if tok == ";"),
                                        "Expected semicolon"
                                    );
                                }
                                tok => panic!("Unexpected token {:?}", tok),
                            }
                        }
                        tok => panic!("Unexpected Token {:?}", tok),
                    }
                    ret.push(BlockItem::Let { pattern, ty, value });
                }
                _ => {
                    let expr = parse_expr(&mut peek, true);
                    if let Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    }) = peek.peek()
                    {
                        if tok == ";" {
                            peek.next();
                            ret.push(BlockItem::Discard(expr));
                        } else {
                            ret.push(BlockItem::Expr(expr));
                        }
                    } else {
                        ret.push(BlockItem::Expr(expr));
                    }
                }
            },
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == ";" => {
                peek.next();
            }
            Some(_) => {
                let expr = parse_expr(&mut peek, true);
                if let Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) = peek.peek()
                {
                    if tok == ";" {
                        peek.next();
                        ret.push(BlockItem::Discard(expr));
                    } else {
                        ret.push(BlockItem::Expr(expr));
                    }
                } else {
                    ret.push(BlockItem::Expr(expr));
                }
            }
            None => break,
        }
    }
    ret
}

pub fn parse_opt_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
) -> Option<Expr> {
    it.peek()?;
    Some(parse_expr(it, allows_block))
}

pub fn parse_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
) -> Expr {
    match it.peek() {
        _ => parse_binary_expr(it, allows_block, None),
    }
}

lazy_static::lazy_static! {
    static ref OPERATOR_MAP: HashMap<&'static str,(usize,usize,BinaryOp)> = {
        let mut map = HashMap::new();
        map.insert("=",(1,2,BinaryOp::Assign));
        map.insert("+=",(1,2,BinaryOp::AddAssign));
        map.insert("-=",(1,2,BinaryOp::SubAssign));
        map.insert("*=",(1,2,BinaryOp::MulAssign));
        map.insert("/=",(1,2,BinaryOp::DivAssign));
        map.insert("%=",(1,2,BinaryOp::ModAssign));
        map.insert("&=",(1,2,BinaryOp::AndAssign));
        map.insert("|=",(1,2,BinaryOp::OrAssign));
        map.insert("^=",(1,2,BinaryOp::XorAssign));
        map.insert("<<=",(1,2,BinaryOp::LshAssign));
        map.insert(">>=",(1,2,BinaryOp::RshAssign));
        map.insert("..",(4,3,BinaryOp::Range));
        map.insert("..=",(4,3,BinaryOp::RangeInclusive));
        map.insert("||",(5,6,BinaryOp::BooleanOr));
        map.insert("&&",(7,8,BinaryOp::BooleanAnd));
        map.insert("==",(10,9,BinaryOp::CompareEq));
        map.insert("!=",(10,9,BinaryOp::CompareNe));
        map.insert("<",(10,9,BinaryOp::CompareLt));
        map.insert(">",(10,9,BinaryOp::CompareGt));
        map.insert("<=",(10,9,BinaryOp::CompareLe));
        map.insert(">=",(10,9,BinaryOp::CompareGe));
        map.insert("|",(11,12,BinaryOp::Or));
        map.insert("^",(13,14,BinaryOp::Xor));
        map.insert("&",(15,16,BinaryOp::And));
        map.insert("<<",(17,18,BinaryOp::Lsh));
        map.insert(">>",(17,18,BinaryOp::Rsh));
        map.insert("+",(19,20,BinaryOp::Add));
        map.insert("-",(19,20,BinaryOp::Subtract));
        map.insert("*",(21,22,BinaryOp::Multiply));
        map.insert("/",(21,22,BinaryOp::Divide));
        map.insert("%",(21,22,BinaryOp::Modulus));

        map

    };
}

pub fn parse_binary_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
    precedence: Option<usize>,
) -> Expr {
    let precedence = precedence.unwrap_or(0);
    let mut lhs = parse_unary_expr(it, allows_block);
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) => {
                if let Some((lbp, rbp, op)) = OPERATOR_MAP.get(&**tok).copied() {
                    it.next();
                    if lbp < precedence {
                        continue;
                    }

                    let rhs = parse_binary_expr(it, allows_block, Some(rbp));

                    match (op, &rhs) {
                        (
                            BinaryOp::CompareEq
                            | BinaryOp::CompareNe
                            | BinaryOp::CompareLt
                            | BinaryOp::CompareLe
                            | BinaryOp::CompareGt
                            | BinaryOp::CompareGe,
                            Expr::BinaryOp(
                                BinaryOp::CompareEq
                                | BinaryOp::CompareNe
                                | BinaryOp::CompareLt
                                | BinaryOp::CompareLe
                                | BinaryOp::CompareGt
                                | BinaryOp::CompareGe,
                                _,
                                _,
                            ),
                        ) => {
                            panic!("Cannot chain comparison operators")
                        }
                        (
                            BinaryOp::Range | BinaryOp::RangeInclusive,
                            Expr::BinaryOp(BinaryOp::Range | BinaryOp::RangeInclusive, _, _),
                        ) => {
                            panic!("Cannot chain comparison operators")
                        }
                        (_, _) => {}
                    }

                    lhs = Expr::BinaryOp(op, Box::new(lhs), Box::new(rhs));
                } else {
                    break;
                }
            }
            _ => break,
        }
    }
    lhs
}

pub fn parse_unary_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
) -> Expr {
    let mut expr = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "-" => {
            it.next();
            Expr::UnaryOp(UnaryOp::Neg, Box::new(parse_unary_expr(it, allows_block)))
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "!" => {
            it.next();
            Expr::UnaryOp(UnaryOp::Not, Box::new(parse_unary_expr(it, allows_block)))
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "*" => {
            it.next();
            Expr::UnaryOp(UnaryOp::Not, Box::new(parse_unary_expr(it, allows_block)))
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "&" => {
            it.next();
            match it.peek().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Identifier,
                    tok,
                    ..
                } if tok == "raw" => match it.peek_next() {
                    Some(Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    }) if tok == "const" => {
                        it.next();
                        it.next();
                        Expr::UnaryOp(
                            UnaryOp::RawRef(Mutability::Const),
                            Box::new(parse_unary_expr(it, allows_block)),
                        )
                    }
                    Some(Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    }) if tok == "mut" => {
                        it.next();
                        it.next();
                        Expr::UnaryOp(
                            UnaryOp::RawRef(Mutability::Mut),
                            Box::new(parse_unary_expr(it, allows_block)),
                        )
                    }
                    _ => {
                        it.reset_cursor();
                        Expr::UnaryOp(
                            UnaryOp::Ref(Mutability::Const),
                            Box::new(parse_unary_expr(it, allows_block)),
                        )
                    }
                },
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                    ..
                } if tok == "mut" => {
                    it.next();
                    Expr::UnaryOp(
                        UnaryOp::Ref(Mutability::Mut),
                        Box::new(parse_unary_expr(it, allows_block)),
                    )
                }
                _ => Expr::UnaryOp(
                    UnaryOp::Ref(Mutability::Const),
                    Box::new(parse_unary_expr(it, allows_block)),
                ),
            }
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ".." => {
            it.next();
            Expr::UnaryOp(
                UnaryOp::RangeTo,
                Box::new(parse_unary_expr(it, allows_block)),
            )
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "..=" => {
            it.next();
            Expr::UnaryOp(
                UnaryOp::RangeToInclusive,
                Box::new(parse_unary_expr(it, allows_block)),
            )
        }
        _ => parse_field_expr(it, allows_block),
    };
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
                ..
            }) if tok == "as" => {
                it.next();
                expr = Expr::Cast(Box::new(expr), parse_type(it).unwrap())
            }
            _ => break expr,
        }
    }
}

pub fn parse_field_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
) -> Expr {
    let mut expr = parse_simple_expr(it, allows_block);
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "." => {
                it.next();
                match it.next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Identifier,
                        tok,
                        ..
                    } => expr = Expr::Field(Box::new(expr), FieldName::Id(tok)),
                    Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    } if tok == "await" => expr = Expr::Await(Box::new(expr)),
                    Lexeme::Token {
                        ty: TokenType::Number,
                        tok,
                        ..
                    } => expr = Expr::Field(Box::new(expr), FieldName::Tuple(tok.parse().unwrap())),
                    tok => panic!("Unexpected token {:?}", tok),
                }
            }
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "?" => {
                it.next();
                expr = Expr::Try(Box::new(expr));
            }
            Some(Lexeme::Group {
                ty: GroupType::Brackets | GroupType::Parentheses,
                ..
            }) => match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Brackets,
                    inner,
                    ..
                } => {
                    let mut it = inner.into_iter().peekmore();
                    let index = parse_expr(&mut it, true);
                    expr = Expr::ArrayIndex {
                        base: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Lexeme::Group {
                    ty: GroupType::Parentheses,
                    inner,
                    ..
                } => {
                    let mut it = inner.into_iter().peekmore();
                    let mut args = Vec::new();
                    loop {
                        if it.peek().is_none() {
                            break;
                        }

                        args.push(parse_expr(&mut it, true));
                        match it.next() {
                            Some(Lexeme::Token {
                                ty: TokenType::Symbol,
                                tok,
                                ..
                            }) if tok == "," => continue,
                            Some(tok) => panic!("Unexpected token {:?}", tok),
                            None => break,
                        }
                    }
                    expr = Expr::FunctionCall {
                        func: Box::new(expr),
                        args,
                    };
                }
                _ => unreachable!(),
            },
            _ => break,
        }
    }
    expr
}

pub fn parse_simple_expr<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    allows_block: bool,
) -> Expr {
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "unsafe" && allows_block => {
            drop(it.next().unwrap());
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => {
                    let mut peek = inner.into_iter().peekmore();
                    let block = parse_block(&mut peek);
                    Expr::UnsafeBlock(block)
                }
                _ => panic!("Invalid token"),
            }
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ".." => {
            it.next();
            Expr::RangeFull
        }
        Lexeme::Token {
            ty: TokenType::String(ty),
            tok,
            ..
        } => {
            let lit = tok.clone();
            let ty = *ty;
            it.next();
            Expr::StringLiteral(ty, lit)
        }
        Lexeme::Token {
            ty: TokenType::Character(ty),
            tok,
            ..
        } => {
            let lit = tok.clone();
            let ty = *ty;
            it.next();
            Expr::CharLiteral(ty, lit)
        }
        Lexeme::Token {
            ty: TokenType::Number,
            tok,
            ..
        } => {
            let val = tok.parse().unwrap();
            it.next();
            Expr::IntLiteral(val)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "-" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Number,
                    tok,
                    ..
                } => Expr::IntLiteral(-tok.parse::<i128>().unwrap()),
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "if" => {
            it.next();
            let ctrl = parse_expr(it, false);
            let block = match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => {
                    let mut iter = inner.into_iter().peekmore();
                    parse_block(&mut iter)
                }
                tok => panic!("Unexpected token {:?}", tok),
            };

            let mut elses = Vec::new();

            loop {
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Keyword,
                        tok,
                        ..
                    }) if tok == "else" => {
                        it.next();
                        match it.next().unwrap() {
                            Lexeme::Group {
                                ty: GroupType::Braces,
                                inner,
                                ..
                            } => {
                                let mut iter = inner.into_iter().peekmore();
                                let block = parse_block(&mut iter);
                                elses.push(ElseBlock::Else(block));
                                break;
                            }
                            Lexeme::Token {
                                ty: TokenType::Keyword,
                                tok,
                                ..
                            } if tok == "if" => {
                                it.next();
                                let ctrl = parse_expr(it, false);
                                match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Braces,
                                        inner,
                                        ..
                                    } => {
                                        let mut iter = inner.into_iter().peekmore();
                                        let block = parse_block(&mut iter);
                                        elses.push(ElseBlock::ElseIf {
                                            control: Box::new(ctrl),
                                            block,
                                        });
                                        continue;
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            tok => panic!("Unexpected token {:?}", tok),
                        }
                    }
                    _ => break,
                }
            }
            Expr::If {
                control: Box::new(ctrl),
                block,
                elses,
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "while" => {
            it.next();
            let ctrl = parse_expr(it, false);
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => {
                    let mut iter = inner.into_iter().peekmore();
                    let block = parse_block(&mut iter);
                    Expr::While {
                        control: Box::new(ctrl),
                        block,
                    }
                }
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "loop" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => {
                    let mut iter = inner.into_iter().peekmore();
                    let block = parse_block(&mut iter);
                    Expr::Loop(block)
                }
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "yield" => {
            it.next();
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == ";" => Expr::Yield(None),
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "," => Expr::Yield(None),
                None => Expr::Yield(None),
                Some(_) => Expr::Yield(Some(Box::new(parse_expr(it, allows_block)))),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "return" => {
            it.next();
            match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == ";" => Expr::Yield(None),
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "," => Expr::Yield(None),
                None => Expr::Yield(None),
                Some(_) => Expr::Return(Some(Box::new(parse_expr(it, allows_block)))),
            }
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "continue" => {
            it.next();
            let life = parse_lifetime(it);

            match &life {
                Some(Lifetime::Static) => panic!("Cannot use 'static in a label"),
                Some(Lifetime::Inferred) => panic!("Cannot use '_ in a label"),
                _ => {}
            }

            Expr::Continue(life)
        }
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
            ..
        } if tok == "break" => {
            it.next();
            let life = parse_lifetime(it);

            match &life {
                Some(Lifetime::Static) => panic!("Cannot use 'static in a label"),
                Some(Lifetime::Inferred) => panic!("Cannot use '_ in a label"),
                _ => {}
            }

            let expr = match it.peek() {
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == ";" => None,
                Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                }) if tok == "," => None,
                None => None,
                Some(_) => Some(Box::new(parse_expr(it, allows_block))),
            };

            Expr::Break(life, expr)
        }
        Lexeme::Group { .. } => match it.next().unwrap() {
            Lexeme::Group {
                ty: GroupType::Parentheses,
                inner,
                ..
            } => {
                let mut it = inner.into_iter().peekmore();
                let expr = parse_opt_expr(&mut it, true);

                if let Some(expr) = expr {
                    match it.next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        }) if tok == "," => {
                            let mut exprs = vec![expr];
                            loop {
                                if let Some(expr) = parse_opt_expr(&mut it, true) {
                                    exprs.push(expr);
                                } else {
                                    break;
                                }

                                match it.next() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == "," => continue,
                                    Some(tok) => panic!("Unexpected token {:?}", tok),
                                    None => break,
                                }
                            }

                            Expr::TupleCtor(exprs)
                        }
                        Some(tok) => panic!("Unexpected token {:?}", tok),
                        None => Expr::Parentheses(Box::new(expr)),
                    }
                } else {
                    Expr::TupleCtor(Vec::new())
                }
            }
            Lexeme::Group {
                ty: GroupType::Brackets,
                inner,
                ..
            } => {
                let mut it = inner.into_iter().peekmore();
                let expr = parse_opt_expr(&mut it, true);

                let ctor = if let Some(expr) = expr {
                    match it.next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        }) if tok == ";" => {
                            let repeat = parse_expr(&mut it, true);

                            ArrayCtor::Repeat {
                                expr,
                                quantity: repeat,
                            }
                        }
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                            ..
                        }) if tok == "," => {
                            let mut exprs = vec![expr];
                            loop {
                                if let Some(expr) = parse_opt_expr(&mut it, true) {
                                    exprs.push(expr);
                                } else {
                                    break;
                                }

                                match it.next() {
                                    Some(Lexeme::Token {
                                        ty: TokenType::Symbol,
                                        tok,
                                        ..
                                    }) if tok == "," => continue,
                                    Some(tok) => panic!("Unexpected token {:?}", tok),
                                    None => break,
                                }
                            }
                            ArrayCtor::List(exprs)
                        }
                        Some(tok) => panic!("Unexpected token {:?}", tok),
                        None => ArrayCtor::List(vec![expr]),
                    }
                } else {
                    ArrayCtor::List(Vec::new())
                };
                Expr::ArrayCtor(Box::new(ctor))
            }
            Lexeme::Group {
                ty: GroupType::Braces,
                inner,
                ..
            } => {
                let mut iter = inner.into_iter().peekmore();
                let block = parse_block(&mut iter);
                Expr::Block(block)
            }
            _ => unreachable!(),
        },
        _ => {
            let path = parse_path(it);
            match it.peek() {
                Some(Lexeme::Group {
                    ty: GroupType::Braces,
                    ..
                }) if allows_block => match it.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
                        ..
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut fields = Vec::new();
                        loop {
                            let name = match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Identifier,
                                    tok,
                                    ..
                                }) => FieldName::Id(tok),
                                Some(Lexeme::Token {
                                    ty: TokenType::Number,
                                    tok,
                                    ..
                                }) => FieldName::Tuple(tok.parse().unwrap()),
                                None => break,
                                Some(tok) => panic!("Unexpected Token {:?}", tok),
                            };
                            match it.next().unwrap() {
                                Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
                                    ..
                                } if tok == ":" => {}
                                tok => panic!("Unexpected Token {:?}", tok),
                            }

                            fields.push(StructFieldInitializer {
                                name,
                                expr: parse_expr(&mut it, true),
                            });
                        }
                        Expr::StructConstructor(path, fields)
                    }
                    _ => unreachable!(),
                },
                _ => Expr::Id(path),
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Mod {
    pub attrs: Vec<Meta>,
    pub items: Vec<Item>,
}

pub fn parse_mod<I: Iterator<Item = Lexeme>>(it: I, mut attrs: Vec<Meta>) -> Mod {
    let mut it = it.peekmore();
    let mut items = Vec::new();
    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            }) if tok == "#" => {
                match it.peek_next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                        ..
                    } if tok == "!" => {
                        it.next();
                        it.next();
                        match it.next().unwrap() {
                            Lexeme::Group {
                                ty: GroupType::Brackets,
                                inner,
                                ..
                            } => {
                                let mut it = inner.into_iter().peekmore();
                                attrs.push(parse_meta(&mut it));
                            }
                            tok => panic!("Unexpected token {:?}", tok),
                        }
                    }
                    _ => break, // No need to validate this is actually #[attr], parsing items will handle that
                }
            }
            _ => break,
        }
    }
    it.reset_cursor();
    while let Some(x) = parse_item(&mut it, Vec::new()) {
        items.push(x);
    }

    Mod { attrs, items }
}
