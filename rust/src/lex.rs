use core::iter::Peekable;

use crate::{
    interning::Symbol,
    span::{HygieneRef, RustEdition, Span, Speekable, Speekerator, HygieneMode, Pos},
};

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
pub enum TokenType {
    Character,
    Identifier,
    Lifetime,
    Number,
    String,
}

#[derive(Debug)]
pub enum LexemeBody {
    Group { ty: GroupType, body: Vec<Lexeme> },
    Token { ty: TokenType, body: Symbol },
    AstFrag(AstFrag),
}

#[derive(Debug)]
pub struct Lexeme {
    pub span: Span,
    pub body: LexemeBody,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof,
    UnrecognizedChar(char, Pos),
}

#[derive(Debug)]
pub enum AstFrag {}

pub type Result<T> = core::result::Result<T, Error>;

fn do_lexeme(file: &mut Speekable<impl Iterator<Item = char>>) -> Option<Result<Lexeme>> {
    loop {
        match file.speek() {
            Some(&(start, c)) => match c {
                '"' => {
                    let mut str = String::from('"');
                    file.next();
                    let end = loop {
                        if let Some((pos, c)) = file.snext() {
                            str.push(c);
                            match c {
                                '"' => break pos,
                                '\\' => match file.next() {
                                    Some(c @ ('n' | 'r' | 't' | '\\' | '0' | '"')) => str.push(c),
                                    _ => todo!(),
                                }
                                '\n' => todo!(), // error
                                x => str.push(x),
                            }
                        } else {
                            return Some(Err(Error::UnexpectedEof));
                        }
                    };
                }
                x if x.is_xid_start() || x == '_' => {
                    let mut id = String::from(x);
                    let mut end = start;
                    file.next();
                    while let Some(&(pos, c)) = file.speek() {
                        if !c.is_xid_continue() {
                            break;
                        } else {
                            id.push(c);
                            end = pos;
                            file.next();
                        }
                    }
                    break Some(Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Identifier,
                            body: id.into(),
                        },
                    }));
                }
                ' ' => { file.next(); },
                x => break Some(Err(Error::UnrecognizedChar(x, start))),
            },
            None => break None,
        }
    }
}

pub fn do_lexeme_str(token: &str, span: Span) -> Result<Lexeme> {
    let mut iter = token.chars();
    let ty = match iter.next() {
        Some('"') => TokenType::String,
        Some('0'..='9') => TokenType::Number,
        Some('\'') => {
            // Lifetime or char
            match iter.next() {
                Some(x) if x.is_xid_start() || x == '_' => match iter.next() {
                    Some('\'') => TokenType::Character,
                    _ => TokenType::Lifetime,
                },
                _ => TokenType::Character,
            }
        }
        Some(x) if x.is_xid_start() || x.is_xid_continue() => {
            // TODO: support named identifiers, raw strings
            TokenType::Identifier
        }
        Some(x) => Err(Error::UnrecognizedChar(x, Pos::new(0, 0)))?, // invalid pos b/c we have no idea
        None => Err(Error::UnexpectedEof)?,
    };
    Ok(Lexeme {
        span,
        body: LexemeBody::Token {
            ty,
            body: token.into(),
        },
    })
}

pub fn lex(file: &mut impl Iterator<Item = char>) -> Result<Vec<Lexeme>> {
    let mut result = Vec::new();
    let mut file = file.speekable();
    while let Some(lexeme) = do_lexeme(&mut file) {
        result.push(lexeme?);
    }
    Ok(result)
}
