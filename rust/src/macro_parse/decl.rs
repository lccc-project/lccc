use peekmore::{PeekMore, PeekMoreIterator};

use crate::{
    interning::Symbol,
    lex::{Group, GroupType, Lexeme, LexemeBody, LexemeClass},
    parse::{do_lexeme_class, do_lexeme_classes, do_lexeme_group},
    span::{Span, Spanned},
};

pub use crate::parse::{Error, Result};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroMatcherKind {
    Tt,
    Ident,
    Literal,
    Lifetime,
    Item,
    Expr,
    Visibility,
    Meta,
    Block,
    Statement,
    Type,
    Path,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MacroMatcher {
    pub kind: Spanned<MacroMatcherKind>,
    pub id: Spanned<Symbol>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum RepetitionType {
    Optional,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Repetition {
    pub tokens: Vec<Spanned<MacroInputToken>>,
    pub sep: Option<Lexeme>,
    pub repty: Spanned<RepetitionType>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroInputToken {
    BareToken(Lexeme),
    Group(GroupType, Vec<Spanned<MacroInputToken>>),
    Repetition(Spanned<Repetition>),
    Matcher(Spanned<MacroMatcher>),
}
