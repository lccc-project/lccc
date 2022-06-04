#![allow(clippy::module_name_repetitions)] // I like this naming convention, gimme a break

use core::fmt::{self, Display, Formatter};
use core::iter::Peekable;

use unicode_xid::UnicodeXID;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum IdentifierKind {
    Normal,
    Keyword,
    Raw,
}

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
    Identifier(IdentifierKind),
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
        write!(f, "{}:{}", self.col, self.row)
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
    Group {
        ty: GroupType,
        inner: Vec<Self>,
        span: Span,
    },
    Token {
        ty: TokenType,
        tok: String,
        span: Span,
    },
}

#[allow(dead_code)]
impl Lexeme {
    pub fn span(&self) -> &Span {
        let (Self::Group { span, .. } | Self::Token { span, .. }) = self;
        span
    }
}

pub struct LexerIterator<I: Iterator<Item = char>> {
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
        next_char
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|x| *x)
    }
}

impl<I: Iterator<Item = char>> From<Peekable<I>> for LexerIterator<I> {
    fn from(other: Peekable<I>) -> Self {
        Self {
            iter: other,
            pos: Position { row: 1, col: 1 },
        }
    }
}

pub fn lex_single_char<I: Iterator<Item = char>>(file: &mut LexerIterator<I>) -> char {
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
                    if !x.is_xid_continue() && x != '_' {
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
                            span: Span {
                                begin,
                                end: file.pos,
                                path: String::from("<todo>"),
                            },
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
                            span: Span {
                                begin,
                                end: file.pos,
                                path: String::from("<todo>"),
                            },
                        });
                    }
                    prefix if file.peek() == Some('\"') || file.peek() == Some('\'') => {
                        todo!("Unhandled prefix for string or character {}", prefix)
                    }
                    prefix if file.peek() == Some('#') => {
                        file.next();
                        let rbegin = file.pos;
                        let c = file.next().unwrap();
                        if c.is_xid_start() || c == '_' {
                            let mut rid = c.to_string();
                            while let Some(x) = file.peek() {
                                if !x.is_xid_continue() && x != '_' {
                                    break;
                                }
                                rid.push(x);
                                file.next();
                            }
                            match prefix {
                                "r" => match &*rid {
                                    "crate" | "super" | "Self" | "self" => {
                                        panic!("Cannot use {} as a raw identifier", rid)
                                    }
                                    _ => result.push(Lexeme::Token {
                                        ty: TokenType::Identifier(IdentifierKind::Raw),
                                        tok: rid,
                                        span: Span {
                                            begin: rbegin,
                                            end: file.pos,
                                            path: String::from("<todo>"),
                                        },
                                    }),
                                },
                                "k" => result.push(Lexeme::Token {
                                    ty: TokenType::Identifier(IdentifierKind::Keyword),
                                    tok: rid,
                                    span: Span {
                                        begin: rbegin,
                                        end: file.pos,
                                        path: String::from("<todo>"),
                                    },
                                }),
                                prefix => panic!("Invalid prefix {}", prefix),
                            }
                        } else if c == '"' || c == '#' {
                            todo!("Unhandled raw string")
                        } else {
                            todo!("Can you have a non id, non-raw string after a prefix")
                        }
                    }
                    _ => result.push(Lexeme::Token {
                        ty: match &id as &str {
                            "as" | "async" | "await" | "break" | "const" | "continue" | "crate"
                            | "dyn" | "else" | "enum" | "extern" | "false" | "fn" | "for"
                            | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move"
                            | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static"
                            | "struct" | "super" | "trait" | "true" | "type" | "unsafe" | "use"
                            | "where" | "while" | "abstract" | "become" | "box" | "do"
                            | "final" | "macro" | "override" | "priv" | "typeof" | "unsized"
                            | "virtual" | "yield" => TokenType::Identifier(IdentifierKind::Keyword),
                            "_" => TokenType::Symbol,
                            _ => TokenType::Identifier(IdentifierKind::Normal),
                        },
                        tok: id,
                        span: Span {
                            begin,
                            end: file.pos,
                            path: String::from("<todo>"),
                        },
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
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '"' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::String(StrType::Default),
                    tok: lex_string(file),
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '\'' => {
                file.next();
                let c = file.peek().unwrap();
                if c.is_xid_start() || c == '_' {
                    file.next();
                    let nc = file.peek();
                    match nc {
                        Some('\'') => result.push(Lexeme::Token {
                            ty: TokenType::Character(CharType::Default),
                            tok: c.to_string(),
                            span: Span {
                                begin,
                                end: file.pos,
                                path: String::from("<todo>"),
                            },
                        }),
                        Some(nc) if c.is_xid_continue() || c == '_' => {
                            let mut id = c.to_string();
                            id.push(nc);
                            while let Some(x) = file.peek() {
                                if !x.is_xid_continue() || x != '_' {
                                    break;
                                }
                                id.push(x);
                                file.next();
                            }
                            result.push(Lexeme::Token {
                                ty: TokenType::Lifetime,
                                tok: id,
                                span: Span {
                                    begin,
                                    end: file.pos,
                                    path: String::from("<todo>"),
                                },
                            })
                        }
                        _ => result.push(Lexeme::Token {
                            ty: TokenType::Lifetime,
                            tok: c.to_string(),
                            span: Span {
                                begin,
                                end: file.pos,
                                path: String::from("<todo>"),
                            },
                        }),
                    }
                } else {
                    let c = lex_single_char(file);
                    if file.next().unwrap() != '\'' {
                        panic!("Expected \' following character literal")
                    }
                }
            }
            x if x.is_whitespace() => {
                file.next();
            }
            x if Some(x) == term => {
                file.next();
                break;
            }
            ':' => {
                let mut tok = String::from(x);
                file.next();
                if let Some(':') = file.peek() {
                    file.next();
                    tok.push(':');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '@' | ',' | ';' | '#' | '$' | '?' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok: String::from(x),
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
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
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '+' | '*' | '%' | '^' | '!' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
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
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '|' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('|') {
                    file.next();
                    tok.push('|');
                } else if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '<' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('<') {
                    file.next();
                    tok.push('<');
                }
                if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '>' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some('>') {
                    file.next();
                    tok.push('>');
                }
                if file.peek() == Some('=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
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
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            '/' => {
                file.next();
                if file.peek() == Some('=') {
                    file.next();
                    result.push(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok: String::from("/="),
                        span: Span {
                            begin,
                            end: file.pos,
                            path: String::from("<todo>"),
                        },
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
                        span: Span {
                            begin,
                            end: file.pos,
                            path: String::from("<todo>"),
                        },
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
                    span: Span {
                        begin,
                        end: file.pos,
                        path: String::from("<todo>"),
                    },
                });
            }
            _ => todo!("{:?}", x),
        }
    }
    result
}

pub fn lex<I: Iterator<Item = char>>(file: &mut I) -> Vec<Lexeme> {
    lex_group(&mut file.peekable().into(), None)
}
