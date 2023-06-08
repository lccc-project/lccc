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
}

#[derive(Debug)]
pub struct ItemMod {
    pub name: Spanned<Symbol>,
    pub content: Option<Spanned<Mod>>,
}

#[derive(Debug)]
pub struct Item {
    pub vis: Spanned<Visibility>,
    pub attrs: Vec<Attr>,
    pub item: Spanned<ItemBody>,
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
    pub name: SimplePath,
    pub input: Option<AttrInput>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Mod {
    pub attrs: Vec<Attr>,
    pub items: Vec<Item>,
}
