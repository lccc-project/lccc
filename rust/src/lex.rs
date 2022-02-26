#![allow(clippy::module_name_repetitions)] // I like this naming convention, gimme a break

use core::fmt::{self, Display, Formatter};
use core::iter::Peekable;

use unicode_xid::UnicodeXID;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum GroupType {
    Parentheses,
    Brackets,
    Braces,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CharType {
    Byte,
    Default,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum StrType {
    Byte,
    Default,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TokenType {
    Keyword,
    Reserved,
    Identifier,
    Character(CharType),
    String(StrType),
    Number,
    Lifetime,
    Symbol,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Position {
    pub row: u32,
    pub col: u32,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.col, self.row);
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub begin: Position,
    pub end: Position,
    pub path: String,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Lexeme {
    Group { ty: GroupType, inner: Vec<Self>, span: Span },
    Token { ty: TokenType, tok: String, span: Span },
}

impl Lexeme {
    pub fn span(&self) -> &Span {
        let (Self::Group { span, .. } | Self::Token { span, .. }) = self;
        span
    }
}

struct LexerIterator<I: Iterator<Item = char>> {
    iter: Peekable<I>,
    pub pos: Position,
}

impl<I: Iterator<Item = char>> LexerIterator<I> {
    fn next(&mut self) -> Option<char> {
        let next_char = self.iter.next();
        match next_char {
            None => {}
            Some('\n') => {
                self.pos.row += 1;
                self.pos.col = 1;
            }
            Some(_) => self.pos.col += 1,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|x| *x)
    }
}

impl<I: Iterator<Item = char>> From<Peekable<I>> for LexerIterator<I> {
    fn from(other: Peekable<I>) -> Self {
        Self {
            iter: other,
            pos: Position {
                row: 1,
                col: 1,
            }
        }
    }
}

pub fn lex_single_char<I: Iterator<Item = char>>(file: &mut Peekable<I>) -> char {
    let x = file.next().unwrap();
    if x == '\\' {
        match file.next().unwrap() {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\', // Not a typo
            '0' => '\0',
            'u' | 'x' => todo!("character code"),
            _ => panic!("insert useful diagnostic here"),
        }
    } else {
        x
    }
}

pub fn lex_string<I: Iterator<Item = char>>(file: &mut LexerIterator<I>) -> String {
    let mut result = String::new();
    while let Some(x) = file.peek() {
        if x == '"' {
            break;
        }
        result.push(lex_single_char(file));
    }
    file.next();
    result
}

#[allow(clippy::too_many_lines)]
pub fn lex_group<I: Iterator<Item = char>>(
    file: &mut LexerIterator<I>,
    term: Option<char>,
) -> Vec<Lexeme> {
    let mut result = Vec::new();
    while let Some(x) = file.peek() {
        let begin = file.pos;
        match x {
            x if x.is_xid_start() || x == '_' => {
                let mut id = String::from(x);
                file.next();
                while let Some(x) = file.peek() {
                    if !x.is_xid_continue() {
                        break;
                    }
                    id.push(x);
                    file.next();
                }
                match &id as &str {
                    "b" if file.peek() == Some('"') => {
                        file.next();
                        result.push(Lexeme::Token {
                            ty: TokenType::String(StrType::Byte),
                            tok: lex_string(file),
                            span: Span { begin, end: file.pos }
                        });
                    }
                    "b" if file.peek() == Some('\'') => {
                        file.next();
                        let single_char = lex_single_char(file);
                        assert!(
                            file.next() == Some('\''),
                            "expected closing quote, didn't get"
                        );
                        result.push(Lexeme::Token {
                            ty: TokenType::Character(CharType::Byte),
                            tok: String::from(single_char),
                            span: Span { begin, end: file.pos }
                        });
                    }
                    "r" => todo!(),
                    _ => result.push(Lexeme::Token {
                        ty: match &id as &str {
                            "as" | "async" | "await" | "break" | "const" | "continue" | "crate"
                            | "dyn" | "else" | "enum" | "extern" | "false" | "fn" | "for"
                            | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move"
                            | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static"
                            | "struct" | "super" | "trait" | "true" | "type" | "unsafe" | "use"
                            | "where" | "while" => TokenType::Keyword,
                            "abstract" | "become" | "box" | "do" | "final" | "macro"
                            | "override" | "priv" | "typeof" | "unsized" | "virtual" | "yield" => {
                                TokenType::Reserved
                            }
                            "_" => TokenType::Symbol,
                            _ => TokenType::Identifier,
                        },
                        tok: id,
                        span: Span { begin, end: file.pos }
                    }),
                }
            }
            x if x >= '0' && x <= '9' => {
                let mut num = String::from(x);
                file.next();
                while let Some(x) = file.peek() {
                    if !x.is_xid_continue() {
                        // Let parser do syntactic verification
                        break;
                    }
                    num.push(x);
                    file.next();
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Number,
                    tok: num,
                    span: Span { begin, end: file.pos }
                });
            }
            '"' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::String(StrType::Default),
                    tok: lex_string(file),
                    span: Span { begin, end: file.pos }
                });
            }
            x if x.is_whitespace() => {
                file.next();
            }
            x if Some(x) == term => {
                file.next();
                break;
            }
            ':' | ';' | '#' | ',' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok: String::from(x),
                    span: Span { begin, end: file.pos }
                });
            }
            '-' | '=' => {
                let mut tok = String::from(x);
                file.next();
                if let Some(next) = file.peek() {
                    if next == '=' || next == '>' {
                        file.next();
                        tok.push(next);
                    }
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span { begin, end: file.pos }
                });
            }
            '*' | '!' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span { begin, end: file.pos }
                });
            }
            '&' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('&') {
                    file.next();
                    tok.push('&');
                } else if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span { begin, end: file.pos }
                });
            }
            '.' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('.') {
                    file.next();
                    tok.push('.');
                    if file.peek() == Some('.') {
                        file.next();
                        tok.push('.');
                    } else if file.peek() == Some('=') {
                        file.next();
                        tok.push('=');
                    }
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span { begin, end: file.pos }
                });
            }
            '/' => {
                file.next();
                if file.peek() == Some('=') {
                    file.next();
                    result.push(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok: String::from("/="),
                        span: Span { begin, end: file.pos }
                    });
                } else if file.peek() == Some('/') {
                    // SINGLE-LINE COMMENT
                    file.next();
                    while file.next() != Some('\n') {}
                } else if file.peek() == Some('*') {
                    // MULTI-LINE COMMENT
                    file.next();
                    while !(file.next() == Some('*') && file.peek() == Some('/')) {}
                    file.next();
                } else {
                    result.push(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok: String::from("/"),
                        span: Span { begin, end: file.pos }
                    });
                }
            }
            '(' | '[' | '{' => {
                file.next();
                result.push(Lexeme::Group {
                    ty: match x {
                        '(' => GroupType::Parentheses,
                        '[' => GroupType::Brackets,
                        '{' => GroupType::Braces,
                        _ => unreachable!(),
                    },
                    inner: lex_group(
                        file,
                        Some(match x {
                            '(' => ')',
                            '[' => ']',
                            '{' => '}',
                            _ => unreachable!(),
                        }),
                    ),
                    span: Span { begin, end: file.pos }
                });
            }
            _ => todo!("{}", x),
        }
    }
    result
}

pub fn lex<I: Iterator<Item = char>>(file: &mut I) -> Vec<Lexeme> {
    lex_group(&mut file.peekable().into(), None)
}
