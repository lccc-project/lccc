#![allow(dead_code, clippy::module_name_repetitions)] // For now
use std::iter::Peekable;

use crate::lex::{GroupType, Lexeme, StrType, TokenType};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
pub enum Item {
    ExternBlock {
        abi: Option<String>,
        items: Vec<Self>,
    },
    FnDeclaration {
        visibility: Visibility,
        safety: Safety,
        name: String,
        params: Vec<FnParam>,
        return_ty: Option<Type>,
        block: Option<Vec<BlockItem>>,
    },
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
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    UnsafeBlock(Vec<BlockItem>),
    Id(String),
    FunctionCall { func: Box<Self>, params: Vec<Self> },
    Cast(Box<Self>, Type),
    StringLiteral(StrType, String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Name(String),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>,
    },
}

#[allow(clippy::too_many_lines)]
pub fn parse_item<I: Iterator<Item = Lexeme>>(it: I) -> Option<Item> {
    let mut peek = it.peekable();
    match peek.next()? {
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
                        let mut iter = inner.into_iter();
                        let mut items = Vec::new();
                        while let Some(item) = parse_item(&mut iter) {
                            items.push(item);
                        }
                        Some(Item::ExternBlock { abi, items })
                    }
                    _ => panic!("Invalid tokens for block"),
                }
            }
            "pub" => match peek.next().expect("Invalid Item") {
                Lexeme::Token {
                    ty: TokenType::Keyword,
                    tok,
                } => match &*tok {
                    "fn" => Some(parse_fn_item(Visibility::Pub, &mut peek)),
                    _ => todo!(),
                },
                _ => panic!("Invalid Token"),
            },
            "fn" => Some(parse_fn_item(Visibility::None, &mut peek)),
            _ => todo!(),
        },
        _ => todo!(),
    }
}

pub fn parse_fn_item<I: Iterator<Item = Lexeme>>(
    visibility: Visibility,
    mut peek: &mut Peekable<I>,
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
        let mut peek = inner.into_iter().peekable();
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
    let mut peek = it.peekable();
    let mut ret = Vec::new();
    loop {
        match peek.peek() {
            Some(Lexeme::Token {
                ty: TokenType::Keyword,
                tok,
            }) => match &**tok {
                "pub" | "fn" | "static" | "const" | "type" | "struct" | "union" | "extern" => {
                    ret.push(BlockItem::Item(Box::new(parse_item(&mut peek).unwrap())));
                }
                "let" => todo!("let"),
                _ => {
                    let expr = parse_expr(&mut peek);
                    if let Some(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok,
                    }) = peek.peek()
                    {
                        if tok == ";" {
                            ret.push(BlockItem::Discard(expr));
                        } else {
                            ret.push(BlockItem::Expr(expr));
                        }
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
                        ret.push(BlockItem::Discard(expr));
                    } else {
                        ret.push(BlockItem::Expr(expr));
                    }
                }
            }
            None => break ret,
        }
    }
}

pub fn parse_expr<I: Iterator<Item = Lexeme>>(mut _it: &mut Peekable<I>) -> Expr {
    todo!()
}

pub fn parse_crate<I: Iterator<Item = Lexeme>>(mut it: I) -> Vec<Item> {
    let mut items = Vec::new();
    while let Some(x) = parse_item(&mut it) {
        items.push(x);
    }

    items
}
