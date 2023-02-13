use core::{fmt, iter::Peekable};

use crate::{
    interning::Symbol,
    span::{HygieneMode, HygieneRef, Pos, RustEdition, Span, Speekable, Speekerator},
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

    pub fn from_start_char(start: char) -> Self {
        match start {
            '(' => Self::Parens,
            '[' => Self::Brackets,
            '{' => Self::Braces,
            _ => panic!("invalid start char"),
        }
    }
}

#[derive(Debug)]
pub enum IdentifierType {
    Default,
    Raw,
}

#[derive(Debug)]
pub enum StringType {
    Default,
    Raw(u8), // number of #s
    Byte,
    RawByte(u8), // see above
}

#[derive(Debug)]
pub enum TokenType {
    Character,
    CommentMulti,
    CommentSingle,
    Identifier(IdentifierType),
    Lifetime,
    Number,
    Punctuation,
    String(StringType),
}

pub enum LexemeBody {
    Group { ty: GroupType, body: Vec<Lexeme> },
    Token { ty: TokenType, body: Symbol },
    AstFrag(AstFrag),
}

impl fmt::Debug for LexemeBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Group { ty, body } => f.debug_tuple("Group").field(ty).field(body).finish(),
            Self::Token { ty, body } => write!(f, "Token({:?}, {:?})", ty, body),
            Self::AstFrag(frag) => write!(f, "AstFrag({:?})", frag),
        }
    }
}

#[derive(Debug)]
pub struct Lexeme {
    pub span: Span,
    pub body: LexemeBody,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof(Pos),
    UnrecognizedChar(char, Pos),
}

#[derive(Debug)]
pub enum AstFrag {}

pub type Result<T> = core::result::Result<T, Error>;

fn do_str(file: &mut Speekable<impl Iterator<Item = char>>) -> Result<(String, Pos)> {
    let mut str = String::from('"');
    let end = loop {
        if let Some((pos, c)) = file.snext() {
            str.push(c);
            match c {
                '"' => break pos,
                '\\' => match file.next() {
                    Some(c @ ('n' | 'r' | 't' | '\\' | '0' | '"')) => str.push(c),
                    x => todo!("{} {:?}", str, x),
                },
                '\n' => todo!(), // error
                _ => {}
            }
        } else {
            Err(Error::UnexpectedEof(file.last_pos()))?
        }
    };
    Ok((str, end))
}

fn do_lexeme(file: &mut Speekable<impl Iterator<Item = char>>) -> Result<Lexeme> {
    loop {
        match file.snext() {
            Some((start, c)) => match c {
                ' ' | '\n' => {}
                '(' | '[' | '{' => {
                    let ty = GroupType::from_start_char(c);
                    let (body, end) = do_group(file, Some(ty.end_char()))?;
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Group { ty, body },
                    });
                }
                '"' => {
                    let (str, end) = do_str(file)?;
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::String(StringType::Default),
                            body: str.into(),
                        },
                    });
                }
                '0'..='9' => {
                    let mut id = String::from(c);
                    let mut end = start;
                    while let Some(&(pos, c)) = file.speek() {
                        if !c.is_xid_continue() {
                            break;
                        } else {
                            id.push(c);
                            end = pos;
                            file.next();
                        }
                    }
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Identifier(IdentifierType::Default),
                            body: id.into(),
                        },
                    });
                }
                x if x.is_xid_start() || x == '_' => {
                    let mut id = String::from(x);
                    let mut end = start;
                    while let Some(&(pos, c)) = file.speek() {
                        if !c.is_xid_continue() {
                            break;
                        } else {
                            id.push(c);
                            end = pos;
                            file.next();
                        }
                    }
                    if id == "r" || id == "rb" {
                        todo!();
                    } else if id == "b" {
                        if file.peek() == Some(&'"') {
                            file.next();
                            let (str, end) = do_str(file)?;
                            break Ok(Lexeme {
                                span: Span::new_simple(start, end),
                                body: LexemeBody::Token {
                                    ty: TokenType::String(StringType::Byte),
                                    body: (id + &str).into(),
                                },
                            });
                        }
                    }
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Identifier(IdentifierType::Default),
                            body: id.into(),
                        },
                    });
                }
                '/' => {
                    let (tok, end, ty) = match file.speek() {
                        Some(&(pos, '/')) => {
                            file.next();
                            let mut tok = String::from("//");
                            let mut end = pos;
                            while let Some((pos, c)) = file.snext() {
                                if c == '\n' {
                                    break;
                                }
                                tok.push(c);
                                end = pos;
                            }
                            (tok.into(), end, TokenType::CommentSingle)
                        }
                        Some(&(pos, '*')) => {
                            file.next();
                            let mut tok = String::from("/*");
                            let mut end = pos;
                            let mut star = false;
                            while let Some((pos, c)) = file.snext() {
                                tok.push(c);
                                end = pos;
                                if c == '/' && star {
                                    break;
                                }
                                star = c == '*';
                            }
                            (tok.into(), end, TokenType::CommentMulti)
                        }
                        Some(&(end, '=')) => {
                            file.next();
                            ("/=".into(), end, TokenType::Punctuation)
                        }
                        _ => ("/".into(), start, TokenType::Punctuation),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token { ty, body: tok },
                    });
                }
                '.' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '.')) => {
                            file.next();
                            match file.speek() {
                                Some(&(end, '.')) => {
                                    file.next();
                                    ("...", end)
                                }
                                Some(&(end, '=')) => {
                                    file.next();
                                    ("..=", end)
                                }
                                _ => ("..", start),
                            }
                        }
                        _ => (".", start),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: punct.into(),
                        },
                    });
                }
                ':' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, ':')) => {
                            file.next();
                            ("::", end)
                        }
                        _ => (":", start),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: punct.into(),
                        },
                    });
                }
                '=' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '=')) => {
                            file.next();
                            ("==", end)
                        }
                        Some(&(end, '>')) => {
                            file.next();
                            ("=>", end)
                        }
                        _ => ("=", start),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: punct.into(),
                        },
                    });
                }
                '+' | '*' | '!' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '=')) => {
                            file.next();
                            ("=", end)
                        }
                        _ => ("", start),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: (String::from(c) + punct).into(),
                        },
                    });
                }
                '-' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '>')) => {
                            file.next();
                            ("->", end)
                        }
                        Some(&(end, '=')) => {
                            file.next();
                            ("-=", end)
                        }
                        _ => ("-", start),
                    };
                    break Ok(Lexeme {
                        span: Span::new_simple(start, end),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: punct.into(),
                        },
                    });
                }
                ';' | '#' | ',' => {
                    break Ok(Lexeme {
                        span: Span::new_simple(start, start),
                        body: LexemeBody::Token {
                            ty: TokenType::Punctuation,
                            body: String::from(c).into(),
                        },
                    });
                }
                x => Err(Error::UnrecognizedChar(x, start))?,
            },
            None => Err(Error::UnexpectedEof(file.last_pos()))?,
        }
    }
}

/// Assumes that the token is legal. If it is not, the result is unspecified.
pub fn do_lexeme_str(token: &str, span: Span) -> Result<Lexeme> {
    let mut iter = token.chars();
    let ty = match iter.next() {
        Some('"') => TokenType::String(StringType::Default),
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
            let hash_pos = token.find('#');
            let quote_pos = token.find('"');
            match (hash_pos, quote_pos) {
                (None, None) => TokenType::Identifier(IdentifierType::Default),
                (None, Some(_)) => {
                    if x == 'b' {
                        TokenType::String(StringType::Byte)
                    } else if x == 'r' && iter.next() == Some('b') {
                        TokenType::String(StringType::RawByte(0))
                    } else {
                        // Could have an if, but again, we are assuming a legal token
                        TokenType::String(StringType::Raw(0))
                    }
                }
                (Some(_), None) => TokenType::Identifier(IdentifierType::Raw),
                (Some(a), Some(b)) if a > b => {
                    if x == 'b' {
                        TokenType::String(StringType::Byte)
                    } else if x == 'r' && iter.next() == Some('b') {
                        TokenType::String(StringType::RawByte(0))
                    } else {
                        // Could have an if, but again, we are assuming a legal token
                        TokenType::String(StringType::Raw(0))
                    }
                }
                (Some(a), Some(b)) => todo!(),
            }
        }
        Some(x) => Err(Error::UnrecognizedChar(x, Pos::new(0, 0)))?, // invalid pos b/c we have no idea
        None => Err(Error::UnexpectedEof(Pos::new(0, 0)))?,
    };
    Ok(Lexeme {
        span,
        body: LexemeBody::Token {
            ty,
            body: token.into(),
        },
    })
}

pub fn do_group(
    file: &mut Speekable<impl Iterator<Item = char>>,
    end_char: Option<char>,
) -> Result<(Vec<Lexeme>, Pos)> {
    let mut result = Vec::new();
    let end = loop {
        let lexeme = do_lexeme(file);
        if let Ok(lexeme) = lexeme {
            result.push(lexeme);
        } else if let Some(c) = end_char {
            if let Err(Error::UnrecognizedChar(x, end)) = lexeme {
                if x == c {
                    break end;
                } else {
                    lexeme?;
                }
            } else {
                lexeme?;
            }
        } else if let Err(Error::UnexpectedEof(end)) = lexeme {
            break end;
        } else {
            lexeme?;
        }
    };
    Ok((result, end))
}

pub fn lex(file: &mut impl Iterator<Item = char>) -> Result<Vec<Lexeme>> {
    do_group(&mut file.speekable(), None).map(|x| x.0)
}
