use core::ops::BitOr;

use peekmore::PeekMoreIterator;

use crate::{
    ast::{Attr, Item, Mod},
    lex::{GroupType, Lexeme, LexemeClass, LexemeBody},
    span::{Span, Pos},
};

#[derive(Debug)]
pub struct Error {
    expected: Vec<LexemeClass>,
    got: LexemeClass,
    span: Span,
}

impl BitOr for Error {
    type Output = Self;

    fn bitor(self, mut rhs: Self) -> Self::Output {
        if self.span.start > rhs.span.start {
            self
        } else if self.span.end > rhs.span.end {
            rhs
        } else if self.span.end == rhs.span.end {
            let mut expected = self.expected;
            expected.append(&mut rhs.expected);
            Self {
                expected,
                got: self.got,
                span: self.span,
            }
        } else {
            todo!(); // Uh oh. Multiple files collided.
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;

pub fn do_lexeme_class(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: LexemeClass,
) -> Result<Lexeme> {
    let lexeme = tree.peek();
    let got = LexemeClass::of(lexeme);
    if got.is(&expected) {
        let result = lexeme.unwrap().clone();
        tree.advance_cursor();
        Ok(result)
    } else {
        let span = lexeme.map_or_else(|| Span::new_simple(Pos::default(), Pos::default()), |x| x.span);
        tree.reset_cursor();
        Err(Error {
            expected: vec![expected],
            got,
            span,
        })
    }
}

pub fn do_lexeme_group(
    tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>,
    expected: Option<GroupType>,
) -> Result<Vec<Lexeme>> {
    let lexeme = do_lexeme_class(tree, LexemeClass::Group(expected))?;
    match lexeme.body {
        LexemeBody::Group(x) => Ok(x.body),
        _ => unreachable!(),
    }
}

pub fn do_internal_attr(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Attr> {
    do_lexeme_class(tree, LexemeClass::Punctuation("#".into()))?;
    do_lexeme_class(tree, LexemeClass::Punctuation("!".into()))?;
    let body = do_lexeme_group(tree, Some(GroupType::Brackets))?;
    todo!()
}

pub fn do_external_attr(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Attr> {
    do_lexeme_class(tree, LexemeClass::Punctuation("#".into()))?;
    let body = do_lexeme_group(tree, Some(GroupType::Brackets))?;
    todo!()
}

pub fn do_item(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Item> {
    todo!()
}

pub fn do_mod(tree: &mut PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Mod> {
    let mut attrs = Vec::new();
    let mut items = Vec::new();

    let mut external_attrs = Vec::new();

    while tree.peek().is_some() {
        match do_internal_attr(tree) {
            Ok(attr) => attrs.push(attr),
            Err(x) => match do_external_attr(tree) {
                Ok(attr) => external_attrs.push(attr),
                Err(y) => match do_item(tree) {
                    Ok(mut item) => {
                        item.attrs.append(&mut external_attrs);
                        items.push(item);
                    }
                    Err(z) => Err(x | y | z)?,
                },
            },
        }
    }

    Ok(Mod {
        attrs: Vec::new(),
        items: Vec::new(),
    })
}
