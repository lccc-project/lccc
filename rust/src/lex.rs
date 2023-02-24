use core::fmt;

use crate::{
    interning::Symbol,
    span::{Pos, Span, Speekable, Speekerator},
};

use unicode_xid::UnicodeXID;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GroupType {
    Parens,
    Brackets,
    Braces,
}

impl GroupType {
    #[allow(dead_code)]
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

#[derive(Clone, Copy, Debug)]
pub enum IdentifierType {
    Default,
    Keyword,
    Raw,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug)]
pub enum StringType {
    Default,
    Raw(u8), // number of #s
    Byte,
    RawByte(u8), // see above
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone)]
pub struct Group {
    pub ty: GroupType,
    pub body: Vec<Lexeme>,
}

impl Group {
    pub fn new(ty: GroupType, body: Vec<Lexeme>) -> Self {
        Self { ty, body }
    }

    pub fn with_span(self, span: Span) -> Lexeme {
        Lexeme {
            span,
            body: LexemeBody::Group(self),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub ty: TokenType,
    pub body: Symbol,
}

impl Token {
    pub fn new(ty: TokenType, body: impl Into<Symbol>) -> Self {
        Self {
            ty,
            body: body.into(),
        }
    }

    pub fn punct(body: impl Into<Symbol>) -> Self {
        Self::new(TokenType::Punctuation, body)
    }

    pub fn with_span(self, span: Span) -> Lexeme {
        Lexeme {
            span,
            body: LexemeBody::Token(self),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum LexemeBody {
    Group(Group),
    Token(Token),
    AstFrag(AstFrag),
}

impl fmt::Debug for LexemeBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Group(Group { ty, body }) => {
                f.debug_tuple("Group").field(ty).field(body).finish()
            }
            Self::Token(Token { ty, body }) => write!(f, "Token({:?}, {:?})", ty, body),
            Self::AstFrag(frag) => write!(f, "AstFrag({:?})", frag),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexemeClass {
    Character,
    Eof,
    Group(Option<GroupType>),
    Keyword(Symbol),
    Identifier,
    Lifetime,
    Number,
    Punctuation(Symbol),
    String,
}

impl LexemeClass {
    pub fn of(lexeme: Option<&Lexeme>) -> Self {
        match lexeme {
            None => Self::Eof,
            Some(Lexeme {
                body: LexemeBody::Group(Group { ty, .. }),
                ..
            }) => Self::Group(Some(*ty)),
            Some(Lexeme {
                body: LexemeBody::Token(token),
                ..
            }) => match token.ty {
                TokenType::Character => Self::Character,
                TokenType::Identifier(ty) => match ty {
                    IdentifierType::Keyword => Self::Keyword(token.body),
                    _ => Self::Identifier,
                },
                TokenType::Lifetime => Self::Lifetime,
                TokenType::Number => Self::Number,
                TokenType::Punctuation => Self::Punctuation(token.body),
                TokenType::String(_) => Self::String,
                _ => unreachable!(), // Comments should be removed by now
            },
            _ => todo!(),
        }
    }

    pub fn is(&self, other: &Self) -> bool {
        self == other || (matches!(other, Self::Group(None)) && matches!(self, Self::Group(_)))
    }
}

#[derive(Clone, Debug)]
pub struct Lexeme {
    pub span: Span,
    pub body: LexemeBody,
}

#[derive(Debug)]
pub enum Error {
    UnexpectedEof(Pos),
    UnrecognizedChar(char, Pos),
}

#[derive(Clone, Debug)]
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
                    break Ok(Group { ty, body }.with_span(Span::new_simple(start, end)));
                }
                '"' => {
                    let (str, end) = do_str(file)?;
                    break Ok(Token::new(TokenType::String(StringType::Default), str)
                        .with_span(Span::new_simple(start, end)));
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
                    break Ok(
                        Token::new(TokenType::Number, id).with_span(Span::new_simple(start, end))
                    );
                }
                x if x.is_xid_start() || x == '_' => {
                    let mut id = String::from(x);
                    let mut end = start;
                    let mut ty = TokenType::Identifier(IdentifierType::Default);
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
                        match file.peek() {
                            Some('#') => {
                                id.push('#');
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
                                ty = TokenType::Identifier(IdentifierType::Raw);
                            }
                            Some('"') => todo!(),
                            _ => {}
                        }
                    } else if id == "b" {
                        if file.peek() == Some(&'"') {
                            file.next();
                            let (str, end) = do_str(file)?;
                            break Ok(Token::new(TokenType::String(StringType::Byte), id + &str)
                                .with_span(Span::new_simple(start, end)));
                        }
                    }
                    break Ok(Token::new(ty, id).with_span(Span::new_simple(start, end)));
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
                            (tok, end, TokenType::CommentSingle)
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
                            (tok, end, TokenType::CommentMulti)
                        }
                        Some(&(end, '=')) => {
                            file.next();
                            ("/=".into(), end, TokenType::Punctuation)
                        }
                        _ => ("/".into(), start, TokenType::Punctuation),
                    };
                    break Ok(Token::new(ty, tok).with_span(Span::new_simple(start, end)));
                }
                '\'' => match file.snext() {
                    Some((pos, '\\')) => {
                        let mut token = String::from("'\\");
                        let mut end = pos;
                        loop {
                            match file.snext() {
                                Some((pos, '\'')) => {
                                    token.push('\'');
                                    end = pos;
                                    break;
                                }
                                Some((pos, '\n')) => Err(Error::UnrecognizedChar('\n', pos))?,
                                Some((pos, x)) => {
                                    token.push(x);
                                    end = pos;
                                }
                                None => Err(Error::UnexpectedEof(end))?,
                            }
                        }
                        break Ok(Token::new(TokenType::Character, token)
                            .with_span(Span::new_simple(start, end)));
                    }
                    Some((pos, x)) => {
                        let mut token = String::from("'");
                        token.push(x);
                        match file.speek() {
                            Some(&(end, '\'')) => {
                                file.next();
                                token.push('\'');
                                break Ok(Token::new(TokenType::Character, token)
                                    .with_span(Span::new_simple(start, end)));
                            }
                            Some((_, x)) if !x.is_xid_continue() => {
                                break Ok(Token::new(TokenType::Lifetime, token)
                                    .with_span(Span::new_simple(start, pos)));
                            }
                            None => {
                                break Ok(Token::new(TokenType::Lifetime, token)
                                    .with_span(Span::new_simple(start, pos)));
                            }
                            Some(&(pos, x)) => {
                                file.next();
                                token.push(x);
                                let mut end = pos;
                                while let Some(&(pos, x)) = file.speek() {
                                    if !x.is_xid_continue() {
                                        break;
                                    }
                                    file.next();
                                    end = pos;
                                    token.push(x);
                                }
                                break Ok(Token::new(TokenType::Lifetime, token)
                                    .with_span(Span::new_simple(start, end)));
                            }
                        }
                    }
                    None => Err(Error::UnexpectedEof(start))?,
                },
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
                                _ => ("..", end),
                            }
                        }
                        _ => (".", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                '<' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '<')) => {
                            file.next();
                            match file.speek() {
                                Some(&(end, '=')) => {
                                    file.next();
                                    ("<<=", end)
                                }
                                _ => ("<<", end),
                            }
                        }
                        Some(&(end, '=')) => {
                            file.next();
                            ("<=", end)
                        }
                        _ => ("<", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                '>' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '>')) => {
                            file.next();
                            match file.speek() {
                                Some(&(end, '=')) => {
                                    file.next();
                                    (">>=", end)
                                }
                                _ => (">>", end),
                            }
                        }
                        Some(&(end, '=')) => {
                            file.next();
                            (">=", end)
                        }
                        _ => (">", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                '|' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '|')) => ("||", end),
                        Some(&(end, '=')) => {
                            file.next();
                            ("|=", end)
                        }
                        _ => ("|", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                ':' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, ':')) => {
                            file.next();
                            ("::", end)
                        }
                        _ => (":", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
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
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                '&' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '=')) => {
                            file.next();
                            ("&=", end)
                        }
                        Some(&(end, '&')) => {
                            file.next();
                            ("&&", end)
                        }
                        _ => ("&", start),
                    };
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                '+' | '*' | '!' => {
                    let (punct, end) = match file.speek() {
                        Some(&(end, '=')) => {
                            file.next();
                            ("=", end)
                        }
                        _ => ("", start),
                    };
                    break Ok(Token::punct(String::from(c) + punct)
                        .with_span(Span::new_simple(start, end)));
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
                    break Ok(Token::punct(punct).with_span(Span::new_simple(start, end)));
                }
                ';' | '#' | ',' | '@' => {
                    break Ok(
                        Token::punct(String::from(c)).with_span(Span::new_simple(start, start))
                    );
                }
                x => Err(Error::UnrecognizedChar(x, start))?,
            },
            None => Err(Error::UnexpectedEof(file.last_pos()))?,
        }
    }
}

#[allow(dead_code)]
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
                (Some(_), Some(_)) => todo!(),
            }
        }
        Some(x) => Err(Error::UnrecognizedChar(x, Pos::default()))?, // invalid pos b/c we have no idea
        None => Err(Error::UnexpectedEof(Pos::default()))?,
    };
    Ok(Token::new(ty, token).with_span(span))
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

pub fn lex(
    file: &mut impl Iterator<Item = char>,
    file_name: impl Into<Symbol>,
) -> Result<Vec<Lexeme>> {
    do_group(&mut file.speekable(file_name), None).map(|x| x.0)
}

pub fn filter_comments(tree: &mut Vec<Lexeme>) {
    tree.retain_mut(|lexeme| match &mut lexeme.body {
        LexemeBody::Group(group) => {
            filter_comments(&mut group.body);
            true
        }
        LexemeBody::Token(Token {
            ty: TokenType::CommentMulti | TokenType::CommentSingle,
            ..
        }) => false, // TODO: Doc comments
        _ => true,
    })
}
