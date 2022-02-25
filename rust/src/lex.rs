#![allow(clippy::module_name_repetitions)] // I like this naming convention, gimme a break

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

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Lexeme {
    Group { ty: GroupType, inner: Vec<Self> },
    Token { ty: TokenType, tok: String },
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

pub fn lex_string<I: Iterator<Item = char>>(file: &mut Peekable<I>) -> String {
    let mut result = String::new();
    while let Some(x) = file.peek() {
        if *x == '"' {
            break;
        }
        result.push(lex_single_char(file));
    }
    file.next();
    result
}

#[allow(clippy::too_many_lines)]
pub fn lex_group<I: Iterator<Item = char>>(
    file: &mut Peekable<I>,
    term: Option<char>,
) -> Vec<Lexeme> {
    let mut result = Vec::new();
    while let Some(&x) = file.peek() {
        match x {
            x if x.is_xid_start() || x == '_' => {
                let mut id = String::from(x);
                file.next();
                while let Some(&x) = file.peek() {
                    if !x.is_xid_continue() {
                        break;
                    }
                    id.push(x);
                    file.next();
                }
                match &id as &str {
                    "b" if file.peek() == Some(&'"') => {
                        file.next();
                        result.push(Lexeme::Token {
                            ty: TokenType::String(StrType::Byte),
                            tok: lex_string(file),
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
                    }),
                }
            }
            x if x >= '0' && x <= '9' => {
                let mut num = String::from(x);
                file.next();
                while let Some(&x) = file.peek() {
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
                });
            }
            '"' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::String(StrType::Default),
                    tok: lex_string(file),
                });
            }
            x if x.is_whitespace() => {
                file.next();
            }
            x if Some(x) == term => {
                file.next();
                break;
            }
            ':' | ';' | '#' | '!' => {
                file.next();
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok: String::from(x),
                });
            }
            '-' | '=' => {
                let mut tok = String::from(x);
                file.next();
                if let Some(&next) = file.peek() {
                    if next == '=' || next == '>' {
                        file.next();
                        tok.push(next);
                    }
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                });
            }
            '*' => {
                let mut tok = String::from(x);
                file.next();
                if file.peek() == Some(&'=') {
                    file.next();
                    tok.push('=');
                }
                result.push(Lexeme::Token {
                    ty: TokenType::Symbol,
                    tok,
                });
            }
            '/' => {
                file.next();
                if file.peek() == Some(&'=') {
                    file.next();
                    result.push(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok: String::from("/="),
                    });
                } else if file.peek() == Some(&'/') {
                    // SINGLE-LINE COMMENT
                    file.next();
                    while file.next() != Some('\n') {}
                } else if file.peek() == Some(&'*') {
                    // MULTI-LINE COMMENT
                    file.next();
                    while !(file.next() == Some('*') && file.peek() == Some(&'/')) {}
                    file.next();
                } else {
                    result.push(Lexeme::Token {
                        ty: TokenType::Symbol,
                        tok: String::from("/"),
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
                });
            }
            _ => todo!("{}", x),
        }
    }
    result
}

pub fn lex<I: Iterator<Item = char>>(file: &mut I) -> Vec<Lexeme> {
    lex_group(&mut file.peekable(), None)
}
