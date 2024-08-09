#![allow(dead_code)]

use std::convert::Infallible;

use xlang::abi::string::String;
use xlang_frontend::lexer;
use xlang_frontend::span::{NoHygiene, Speekable};

pub use xlang_frontend::lexer::{
    is_ident_part_unicode, is_ident_start_unicode, parse_escape, DefaultTy, Error, HexEscape,
    Lexer, Result, UnicodeEscapeRust,
};

pub type Lexeme = lexer::Lexeme<XirLexer>;
pub type Group = lexer::Group<XirLexer, NoHygiene, GroupType>;
pub type Token = lexer::LToken<XirLexer>;
pub type TokenType = lexer::LToken<XirLexer>;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum GroupType {
    Paren,
    Bracket,
    Brace,
    Angle,
}

pub struct XirLexer;

impl Lexer for XirLexer {
    type Hygiene = NoHygiene;

    type IdentTy = DefaultTy;

    type StrTy = DefaultTy;

    type CharTy = DefaultTy;

    type OtherTy = Infallible;

    type GroupType = GroupType;

    type OtherLex = Infallible;

    fn is_ident_start(&self, c: char) -> bool {
        is_ident_start_unicode(c) || c == '_' || c == '$'
    }

    fn is_ident_part(&self, c: char) -> bool {
        is_ident_part_unicode(c) || c == '_' || c == '$' || c == '.'
    }

    fn ident_type(&self, _: &str) -> Self::IdentTy {
        DefaultTy
    }

    fn is_num_sep(&self, _: char) -> bool {
        false
    }

    fn is_num_prefix_frag(&self, c: &str) -> bool {
        c == "0"
    }

    fn is_num_prefix(&self, c: &str) -> Option<u32> {
        match c {
            "0x" => Some(16),
            _ => None,
        }
    }

    fn check_valid_num<I: Iterator<Item = char>>(
        &self,
        c: &str,
        it: &mut xlang_frontend::iter::PeekMoreIterator<Speekable<I>>,
    ) -> Result<()> {
        if c == "0x" {
            match it.next() {
                Some((pos, c)) => Err(Error::UnrecognizedChar(c, pos)),
                None => Err(Error::UnexpectedEof(it.last_pos())),
            }
        } else {
            Ok(())
        }
    }

    fn is_float_in_radix(&self, _: u32) -> bool {
        false
    }

    fn is_float_exp_char(&self, _: char, _: u32) -> bool {
        false
    }

    fn is_group_start(&self, c: char) -> Option<(Self::GroupType, char)> {
        match c {
            '{' => Some((GroupType::Brace, '}')),
            '[' => Some((GroupType::Bracket, ']')),
            '<' => Some((GroupType::Angle, '>')),
            '(' => Some((GroupType::Paren, ')')),
            _ => None,
        }
    }

    fn next_token_start<I: Iterator<Item = char>>(
        &self,
        it: &mut xlang_frontend::iter::PeekMoreIterator<Speekable<I>>,
    ) -> Result<Option<(char, xlang_frontend::span::Pos)>> {
        let (pos, c) = loop {
            match it.next() {
                Some((pos, c)) => match c {
                    c if c.is_ascii_whitespace() => continue,
                    '/' => match it.peek() {
                        Some((_, '/')) => {
                            it.next();
                            while let Some((_, c)) = it.next() {
                                if c == '\n' {
                                    break;
                                }
                            }
                        }
                        Some((_, '*')) => {
                            it.next();
                            loop {
                                match it.next() {
                                    Some((_, c)) if c == '*' => match it.next() {
                                        Some((_, c)) if c == '/' => break,
                                        Some(_) => {}
                                        None => return Err(Error::UnexpectedEof(it.last_pos())),
                                    },
                                    Some(_) => {}
                                    None => return Err(Error::UnexpectedEof(it.last_pos())),
                                }
                            }
                        }
                        _ => break (pos, '/'),
                    },
                    c => break (pos, c),
                },
                None => return Ok(None),
            }
        };

        Ok(Some((c, pos)))
    }

    fn punct_list(&self) -> &[&str] {
        &["->", "::", ":", "#", "@", "*", ";"]
    }

    fn check_valid_punct<I: Iterator<Item = char>>(
        &self,
        c: &str,
        it: &mut xlang_frontend::iter::PeekMoreIterator<Speekable<I>>,
    ) -> Result<()> {
        match c {
            "->" | "::" | ":" | "#" | "@" | "*" | ";" => Ok(()),
            _ => match it.next() {
                Some((pos, c)) => Err(Error::UnrecognizedChar(c, pos)),
                None => Err(Error::UnexpectedEof(it.last_pos())),
            },
        }
    }

    fn default_str_type(&self) -> Self::StrTy {
        DefaultTy
    }

    fn default_char_type(&self) -> Self::CharTy {
        DefaultTy
    }

    fn check_escape<I: Iterator<Item = char>>(
        &self,
        it: &mut xlang_frontend::iter::PeekMoreIterator<Speekable<I>>,
    ) -> Result<String> {
        parse_escape(
            it,
            (
                'n',
                't',
                '0',
                'r',
                HexEscape('x', 2),
                UnicodeEscapeRust('u', 1..=6),
            ),
        )
    }
}

pub use xlang_frontend::lexer::lex_file;
