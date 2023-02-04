use core::iter::Peekable;

use crate::{interning::Symbol, span::Span};

use unicode_xid::UnicodeXID;

#[derive(Debug)]
pub enum GroupType {
    Parens,
    Brackets,
    Braces,
}

impl GroupType {
    pub fn start_char(&self) -> char {
        match self {
            Self::Parens => '(',
            Self::Brackets => '[',
            Self::Braces => '{',
        }
    }

    pub fn end_char(&self) -> char {
        match self {
            Self::Parens => ')',
            Self::Brackets => ']',
            Self::Braces => '}',
        }
    }
}

#[derive(Debug)]
pub enum TokenType {}

#[derive(Debug)]
pub enum LexemeBody {
    Group { ty: GroupType, body: Vec<Lexeme> },
    Token { ty: TokenType, body: Symbol },
}

#[derive(Debug)]
pub struct Lexeme {
    pub span: Span,
    pub body: LexemeBody,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    UnrecognizedChar(char),
}

pub type Result<T> = core::result::Result<T, Error>;

fn do_lexeme(file: &mut Peekable<impl Iterator<Item = char>>) -> Option<Result<Lexeme>> {
    match file.next()? {
        x if x.is_xid_start() || x == '_' => {
            let mut id = String::from(x);
            let sym = Symbol::intern_by_val(id);
            Some(Ok(Lexeme { span: (), body: () }))
        }
        x => Some(Err(Error::UnrecognizedChar(x)))
    }
}

pub fn do_lexeme_str(file: &str) -> Result<Lexeme> {
    do_lexeme(&mut file.chars().peekable()).unwrap_or(Err(Error::UnexpectedEof))
}

pub fn lex(file: &mut impl Iterator<Item = char>) -> Result<Vec<Lexeme>> {
    let mut result = Vec::new();
    let mut file = file.peekable();
    while let Some(lexeme) = do_lexeme(&mut file) {
        result.push(lexeme?);
    }
    Ok(result)
}
