use core::ops::BitOr;

use peekmore::PeekMoreIterator;

use crate::{ast::Mod, lex::{Lexeme, LexemeClass}, span::Span};

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

pub fn do_mod(mut tree: PeekMoreIterator<impl Iterator<Item = Lexeme>>) -> Result<Mod> {
    let mut result_mod = Mod {
        attrs: Vec::new(),
        items: Vec::new(),
    };

    Ok(result_mod)
}
