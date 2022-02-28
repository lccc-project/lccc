#![allow(dead_code, clippy::module_name_repetitions)] // For now
use peekmore::{PeekMore, PeekMoreIterator};
use xlang::prelude::v1::HashMap;

use crate::{
    lex::{GroupType, Lexeme, TokenType},
    macro_parse::{MacroArm, MacroMatcher, MacroOutput},
};

pub use crate::lex::StrType;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Visibility {
    None,
    Pub,
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
        safety: Safety,
        name: String,
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
        generics: Vec<GenericParam>,
        vis: Visibility,
        variants: Vec<EnumVariant>,
    },
    TypeAlias {
        attrs: Vec<Meta>,
        vis: Visibility,
        name: String,
        generics: Vec<GenericParam>,
        defn: Option<Type>,
    },
    Trait {
        attrs: Vec<Meta>,
        vis: Visibility,
        safety: Safety,
        auto: bool,
        name: String,
        generics: Vec<GenericParam>,
    },
    Impl {
        attrs: Vec<Meta>,
        safety: Safety,
        trait_name: Option<TraitImplName>,
        ty: Type,
    },
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
    pub unbound: bool,
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
    pub generics: Vec<GenericParam>,
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
    pub pat: Pattern,
    pub ty: Type,
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
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldPattern {
    Rest,
    Field(FieldName, Pattern),
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
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Range,
    RangeTo,
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
    TypeAscription(Box<Expr>, Type),
    Try(Box<Expr>),
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
                    _ => None,
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
                    _ => panic!("Invalid tokens for block"),
                }
            }
            "fn" | "unsafe" => Some(parse_fn_item(vis, attrs, it)),
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
                let generics = match it.peek().unwrap() {
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

    let generics = match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "<" => parse_generic_decl(it),
        _ => Vec::new(),
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
    attrs: Vec<Meta>,
    peek: &mut PeekMoreIterator<I>,
) -> Item {
    peek.next(); // eat `fn`, do `extern fn`, `unsafe fn`
    let name = match peek.next().expect("Invalid Item") {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => tok,
        _ => panic!("Invalid Token"),
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

    match peek.next().expect("Invalid Token") {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ";" => Item::FnDeclaration {
            attrs,
            visibility,
            safety: Safety::Safe,
            name,
            params,
            return_ty: None,
            block: None,
        },
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "->" => {
            let retty = parse_type(peek).unwrap();
            match peek.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    ..
                } if tok == ";" => Item::FnDeclaration {
                    visibility,
                    attrs,
                    safety: Safety::Safe,
                    name,
                    params,
                    return_ty: Some(retty),
                    block: None,
                },
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
                    ..
                } => Item::FnDeclaration {
                    attrs,
                    visibility,
                    safety: Safety::Safe,
                    name,
                    params,
                    return_ty: Some(retty),
                    block: Some(parse_block(inner.into_iter())),
                },
                tok => panic!("Unexpected Token {:?}", tok),
            }
        }
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
            ..
        } => Item::FnDeclaration {
            attrs,
            visibility,
            safety: Safety::Safe,
            name,
            params,
            return_ty: None,
            block: Some(parse_block(inner.into_iter())),
        },
        _ => panic!("Invalid Token"),
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

pub fn parse_bounds<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Vec<GenericBound> {
    let mut ret = Vec::new();
    loop {
        let unbound = match it.peek().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
                ..
            } if tok == "?" => {
                it.next();
                true
            }
            Lexeme::Token {
                ty: TokenType::Lifetime,
                ..
            } => {
                ret.push(GenericBound::Lifetime(parse_lifetime(it).unwrap()));
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
            _ => false,
        };

        let constness = match it.peek().unwrap() {
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
                        .push(PathComponent::Generics(parse_generics(it)));
                }
                _ => {}
            }
        }

        ret.push(GenericBound::Trait(TraitBound {
            unbound,
            const_kind: constness,
            trait_name: path,
        }));

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
    match it.next()? {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
            ..
        } => Some(Pattern::Ident(tok)),
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == "_" => Some(Pattern::Discard),
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
        _ => todo!("struct patterns"),
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
        _ => {
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
            Some(Type::Name(path))
        }
    }
}

pub fn parse_fn_param<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> FnParam {
    let pat = parse_pattern(it).unwrap();

    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
            ..
        } if tok == ":" => {}
        _ => panic!("Invalid Token"),
    }

    let ty = parse_type(it).unwrap();

    FnParam { pat, ty }
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
                    if lbp < precedence {
                        continue;
                    }
                    it.next();
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
        Lexeme::Group {
            ty: GroupType::Braces,
            ..
        } => todo!("Block exprs"),
        Lexeme::Group {
            ty: GroupType::Parentheses,
            inner,
            ..
        } => {
            let mut iter = inner.clone().into_iter().peekmore();
            it.next();
            Expr::Parentheses(Box::new(parse_expr(&mut iter, true)))
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
