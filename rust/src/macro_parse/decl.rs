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

pub fn do_repetition(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<Repetition>> {
    let (group, mut span) = do_lexeme_group(tree, Some(GroupType::Parens))?;

    let mut iter = group.body.into_iter().peekmore();

    let tokens = core::iter::from_fn(|| do_macro_input(&mut iter).transpose())
        .collect::<Result<Vec<_>>>()?;

    let tok = tree.peek().ok_or_else(|| Error {
        expected: vec![
            LexemeClass::Punctuation("+".into()),
            LexemeClass::Punctuation("*".into()),
            LexemeClass::Punctuation("?".into()),
        ],
        got: LexemeClass::Eof,
        span: span.with_start(span.end),
    })?;

    let sep = match LexemeClass::of(Some(tok)) {
        LexemeClass::Eof => unreachable!(),
        LexemeClass::Group(gty) => {
            return Err(Error {
                expected: vec![
                    LexemeClass::Punctuation("+".into()),
                    LexemeClass::Punctuation("*".into()),
                    LexemeClass::Punctuation("?".into()),
                ],
                got: LexemeClass::Eof,
                span: span.with_start(span.end),
            });
        }
        LexemeClass::Punctuation(sym) if sym == "+" || sym == "*" || sym == "?" => None,
        _ => {
            let tok = iter.next().unwrap();
            span = Span::between(span, tok.span);

            Some(tok)
        }
    };

    let (repty_tok, repty_cl) = do_lexeme_classes(
        tree,
        &[
            LexemeClass::Punctuation("+".into()),
            LexemeClass::Punctuation("*".into()),
            LexemeClass::Punctuation("?".into()),
        ],
    )?;

    let repty = match repty_cl {
        LexemeClass::Punctuation(sym) if sym == "+" => RepetitionType::OneOrMore,
        LexemeClass::Punctuation(sym) if sym == "*" => RepetitionType::ZeroOrMore,
        LexemeClass::Punctuation(sym) if sym == "?" => RepetitionType::Optional,
        _ => unreachable!(),
    };

    let repty = Spanned {
        body: repty,
        span: repty_tok.span,
    };

    Ok(Spanned {
        span,
        body: Repetition { tokens, sep, repty },
    })
}

pub fn do_macro_matcher(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Spanned<MacroMatcher>> {
    todo!()
}

pub fn do_macro_input(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
) -> Result<Option<Spanned<MacroInputToken>>> {
    let tok = match tree.next() {
        Some(tok) => tok,
        None => return Ok(None),
    };

    let mut span = tok.span;

    match &tok.body {
        crate::lex::LexemeBody::Group(group) => {
            let gty = group.ty;

            let group = match tok.body {
                LexemeBody::Group(group) => group,
                _ => unreachable!(),
            };

            let mut iter = group.body.into_iter().peekmore();

            let vec = core::iter::from_fn(|| do_macro_input(&mut iter).transpose())
                .collect::<Result<Vec<_>>>()?;

            Ok(Some(Spanned {
                span,
                body: MacroInputToken::Group(gty, vec),
            }))
        }
        crate::lex::LexemeBody::Token(atok) => {
            if atok.body == "$" {
                match tree.peek() {
                    Some(Lexeme {
                        span: gspan,
                        body:
                            LexemeBody::Group(Group {
                                ty: GroupType::Parens,
                                ..
                            }),
                    }) => {
                        span = Span::between(span, *gspan);
                        let rep = do_repetition(tree)?;
                        Ok(Some(Spanned {
                            body: MacroInputToken::Repetition(rep),
                            span,
                        }))
                    }
                    _ => {
                        let matcher = do_macro_matcher(tree)?;
                        span = Span::between(span, matcher.span);
                        Ok(Some(Spanned {
                            body: MacroInputToken::Matcher(matcher),
                            span,
                        }))
                    }
                }
            } else {
                Ok(Some(Spanned {
                    span,
                    body: MacroInputToken::BareToken(tok),
                }))
            }
        }
        crate::lex::LexemeBody::AstFrag(_) => Ok(Some(Spanned {
            body: MacroInputToken::BareToken(tok),
            span,
        })),
    }
}
