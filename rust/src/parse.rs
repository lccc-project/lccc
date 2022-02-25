#![allow(dead_code, clippy::module_name_repetitions)] // For now
use peekmore::{PeekMore, PeekMoreIterator};

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
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockItem {
    Item(Box<Item>),
    Expr(Expr),
    Discard(Expr),
    Let {
        pattern: Pattern,
        value: Option<Expr>,
    },
    MacroExpansion {
        target: SimplePath,
        args: Vec<Lexeme>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    UnsafeBlock(Vec<BlockItem>),
    Id(String),
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
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Name(String),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Never,
}

pub fn parse_simple_path<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> SimplePath {
    let mut root = false;
    let mut path = Vec::new();
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
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
            } => {
                path.push(tok);
                match it.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
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
                    } => {
                        let mut it = inner.into_iter().peekmore();
                        let mut group = Vec::new();
                        loop {
                            group.push(parse_meta(&mut it));
                            match it.next() {
                                Some(Lexeme::Token {
                                    ty: TokenType::Symbol,
                                    tok,
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
        } => {
            let val = tok.parse().unwrap();
            it.next();
            Meta::IntLit(val)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "-" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Number,
                    tok,
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
        } if tok == "$" => todo!("Macro matcher fragement/repetition"),
        tok @ Lexeme::Token { .. } => Some(MacroMatcher::RawToken(tok)),
        Lexeme::Group { ty, inner } => {
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
        } if tok == "$" => todo!("Macro matcher fragement/repetition"),
        tok @ Lexeme::Token { .. } => Some(MacroOutput::RawToken(tok)),
        Lexeme::Group { ty, inner } => {
            let mut it = inner.into_iter().peekmore();
            Some(MacroOutput::Group(
                ty,
                core::iter::from_fn(move || parse_macro_output(&mut it)).collect(),
            ))
        }
    }
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
pub fn parse_item<I: Iterator<Item = Lexeme>>(
    it: &mut PeekMoreIterator<I>,
    mut attrs: Vec<Meta>,
) -> Option<Item> {
    let mut peek = it.peekmore();
    match peek.next()? {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
        } if tok == "macro_rules" => match peek.next().unwrap() {
            Lexeme::Token {
                ty: TokenType::Symbol,
                tok,
            } if tok == "!" => match peek.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Identifier,
                    tok,
                } => match peek.next().unwrap() {
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
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
                                } if tok == "=>" => match it.next().unwrap() {
                                    Lexeme::Group {
                                        ty: GroupType::Braces,
                                        inner,
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
        },
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
        } => match &*tok {
            "extern" => {
                let abi = match peek.peek() {
                    Some(Lexeme::Token {
                        ty: TokenType::String(_),
                        ..
                    }) => {
                        if let Some(Lexeme::Token {
                            ty: TokenType::String(_),
                            tok,
                        }) = peek.next()
                        {
                            Some(tok)
                        } else {
                            unreachable!()
                        }
                    }
                    _ => None,
                };

                let block = peek.next().expect("Missing block");

                match block {
                    Lexeme::Group {
                        ty: GroupType::Braces,
                        inner,
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
            "pub" => match peek.next().expect("Invalid Item") {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                } => match &*tok {
                    "fn" => Some(parse_fn_item(Visibility::Pub, attrs, &mut peek)),
                    _ => todo!(),
                },
                _ => panic!("Invalid Token"),
            },
            "fn" => Some(parse_fn_item(Visibility::None, attrs, &mut peek)),
            _ => todo!(),
        },
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "#" => loop {
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Brackets,
                    inner,
                } => {
                    let mut iter = inner.into_iter().peekmore();
                    attrs.push(parse_meta(&mut iter));
                }
                tok => panic!("Unexpected Token {:?}", tok),
            }
            match it.peek().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                } if tok == "#" => {
                    it.next();
                    continue;
                }
                _ => break parse_item(it, attrs),
            }
        },
        tok => todo!("{:?}", tok),
    }
}

pub fn parse_fn_item<I: Iterator<Item = Lexeme>>(
    visibility: Visibility,
    attrs: Vec<Meta>,
    mut peek: &mut PeekMoreIterator<I>,
) -> Item {
    let name = match peek.next().expect("Invalid Item") {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
        } => tok,
        _ => panic!("Invalid Token"),
    };

    let params = if let Some(Lexeme::Group {
        ty: GroupType::Parentheses,
        inner,
    }) = peek.next()
    {
        let mut peek = inner.into_iter().peekmore();
        let mut params = Vec::new();
        if peek.peek().is_some() {
            loop {
                params.push(parse_fn_param(&mut peek));
                if let Some(lex) = peek.next() {
                    if lex
                        != (Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok: String::from(","),
                        })
                    {
                        panic!("Invalid token");
                    }
                } else {
                    break;
                }
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
        } if tok == "->" => {
            let retty = parse_type(&mut peek);
            match peek.next().expect("Invalid Token") {
                Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
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
                } => Item::FnDeclaration {
                    attrs,
                    visibility,
                    safety: Safety::Safe,
                    name,
                    params,
                    return_ty: Some(retty),
                    block: Some(parse_block(inner.into_iter())),
                },
                _ => panic!("Invalid Token"),
            }
        }
        Lexeme::Group {
            ty: GroupType::Braces,
            inner,
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

pub fn parse_pattern<I: Iterator<Item = Lexeme>>(mut it: I) -> Pattern {
    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
        } => Pattern::Ident(tok),
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "_" => Pattern::Discard,
        _ => todo!("struct patterns"),
    }
}

pub fn parse_type<I: Iterator<Item = Lexeme>>(mut it: I) -> Type {
    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "!" => Type::Never,
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "*" => {
            let mutability = match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                } if tok == "const" => Mutability::Const,
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                } if tok == "mut" => Mutability::Mut,
                _ => panic!("Invalid token"),
            };

            let underlying = parse_type(it);
            Type::Pointer {
                mutability,
                underlying: Box::new(underlying),
            }
        }
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
        } => Type::Name(tok),
        _ => todo!(),
    }
}

pub fn parse_fn_param<I: Iterator<Item = Lexeme>>(mut it: I) -> FnParam {
    let pat = parse_pattern(&mut it);

    match it.next().unwrap() {
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == ":" => {}
        _ => panic!("Invalid Token"),
    }

    let ty = parse_type(&mut it);

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
            }) => match &**tok {
                "pub" | "fn" | "static" | "const" | "type" | "struct" | "union" | "extern" => {
                    ret.push(BlockItem::Item(Box::new(
                        parse_item(&mut peek, Vec::new()).unwrap(),
                    )));
                }
                "let" => {
                    peek.next();
                    let pattern = parse_pattern(&mut peek);
                    let value;
                    match peek.peek() {
                        Some(Lexeme::Token { tok, .. }) if tok == ";" => {
                            peek.next();
                            value = None;
                        }
                        Some(Lexeme::Token { tok, .. }) if tok == "=" => {
                            peek.next();
                            value = Some(parse_expr(&mut peek));
                            assert!(
                                matches!(peek.next(), Some(Lexeme::Token { tok, .. }) if tok == ";"),
                                "Expected semicolon"
                            );
                        }
                        _ => panic!("Invalid Token"),
                    }
                    ret.push(BlockItem::Let { pattern, value });
                }
                _ => {
                    let expr = parse_expr(&mut peek);
                    if let Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
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
            Some(_) => {
                let expr = parse_expr(&mut peek);
                if let Some(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
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

pub fn parse_expr<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    parse_expr_with_block(it)
}

pub fn parse_expr_with_block<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    match it.peek().unwrap() {
        Lexeme::Token {
            ty: TokenType::Keyword,
            tok,
        } if tok == "unsafe" => {
            drop(it.next().unwrap());
            match it.next().unwrap() {
                Lexeme::Group {
                    ty: GroupType::Braces,
                    inner,
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
        _ => parse_expr_without_block(it),
    }
}

pub fn parse_expr_without_block<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    parse_as_cast(it)
}

pub fn parse_as_cast<I: Iterator<Item = Lexeme>>(mut it: &mut PeekMoreIterator<I>) -> Expr {
    let mut expr = parse_unary_expr(it);

    loop {
        match it.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
            }) if tok == "as" => {
                drop(it.next().unwrap());
                let ty = parse_type(&mut it);
                expr = Expr::Cast(Box::new(expr), ty);
            }
            _ => break expr,
        }
    }
}

pub fn parse_unary_expr<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    parse_function_call(it)
}

pub fn parse_function_call<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    let expr = parse_simple_expr(it);

    match it.peek() {
        Some(Lexeme::Group {
            ty: GroupType::Parentheses,
            ..
        }) => {
            let inner = if let Lexeme::Group {
                ty: GroupType::Parentheses,
                inner,
            } = it.next().unwrap()
            {
                inner
            } else {
                panic!("Invalid token")
            };
            let mut iter = inner.into_iter().peekmore();
            let mut args = Vec::new();
            if iter.peek().is_some() {
                loop {
                    let arg = parse_expr(&mut iter);
                    args.push(arg);
                    match iter.next() {
                        Some(Lexeme::Token {
                            ty: TokenType::Symbol,
                            tok,
                        }) if tok == "," => {}
                        Some(_) => panic!("Invalid token"),
                        None => break,
                    }
                }
            }
            Expr::FunctionCall {
                func: Box::new(expr),
                args,
            }
        }
        _ => expr,
    }
}

pub fn parse_simple_expr<I: Iterator<Item = Lexeme>>(it: &mut PeekMoreIterator<I>) -> Expr {
    match it.next().unwrap() {
        Lexeme::Group {
            ty: GroupType::Parentheses,
            inner,
        } => {
            let mut iter = inner.into_iter().peekmore();
            Expr::Parentheses(Box::new(parse_expr(&mut iter)))
        }
        Lexeme::Token {
            ty: TokenType::Identifier,
            tok,
        } => Expr::Id(tok),
        Lexeme::Token {
            ty: TokenType::String(ty),
            tok,
        } => Expr::StringLiteral(ty, tok),
        Lexeme::Token {
            ty: TokenType::Number,
            tok,
        } => {
            let val = tok.parse().unwrap();
            it.next();
            Expr::IntLiteral(val)
        }
        Lexeme::Token {
            ty: TokenType::Symbol,
            tok,
        } if tok == "-" => {
            it.next();
            match it.next().unwrap() {
                Lexeme::Token {
                    ty: TokenType::Number,
                    tok,
                } => Expr::IntLiteral(-tok.parse::<i128>().unwrap()),
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        tok => panic!("Invalid Token: {:?}", tok),
    }
}

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
            }) if tok == "#" => {
                match it.peek_next().unwrap() {
                    Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                    } if tok == "!" => {
                        it.next();
                        it.next();
                        match it.next().unwrap() {
                            Lexeme::Group {
                                ty: GroupType::Brackets,
                                inner,
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
    while let Some(x) = parse_item(&mut it, Vec::new()) {
        items.push(x);
    }

    Mod { attrs, items }
}
