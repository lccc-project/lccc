use crate::{lex::Lexeme, span::Span};

#[derive(Debug)]
pub enum ItemBody {}

#[derive(Debug)]
pub struct Item {
    pub attrs: Vec<Attr>,
    pub body: ItemBody,
    pub span: Span,
}

#[derive(Debug)]
pub struct SimplePath {}

#[derive(Debug)]
pub enum AttrInput {
    DelimTokenTree(Vec<Lexeme>, Span),
}

#[derive(Debug)]
pub struct Attr {
    name: SimplePath,
    input: Option<AttrInput>,
    span: Span,
}

#[derive(Debug)]
pub struct Mod {
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}
