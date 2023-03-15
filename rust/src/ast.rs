use crate::{lex::Lexeme, span::Span, interning::Symbol};

#[derive(Debug)]
pub enum ItemBody {}

#[derive(Debug)]
pub struct Item {
    pub attrs: Vec<Attr>,
    pub body: ItemBody,
    pub span: Span,
}

#[derive(Debug)]
pub enum SimplePathSegmentBody {
    Identifier(Symbol),
    SuperPath,
    SelfPath,
    CratePath,
    MacroCratePath,
}

#[derive(Debug)]
pub struct SimplePathSegment {
    pub body: SimplePathSegmentBody,
    pub span: Span,
}

#[derive(Debug)]
pub struct SimplePath {
    pub from_root: bool,
    pub segments: Vec<SimplePathSegment>,
}

#[derive(Debug)]
pub enum AttrInput {
    DelimTokenTree(Vec<Lexeme>, Span),
}

#[derive(Debug)]
pub struct Attr {
    pub name: SimplePath,
    pub input: Option<AttrInput>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Mod {
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}
