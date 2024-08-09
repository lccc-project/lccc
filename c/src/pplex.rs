use xlang::abi::string::String;
use xlang::abi::try_;
use xlang_frontend::iter::{IntoRewinder, PeekMoreIterator};
use xlang_frontend::lexer::{is_ident_part_unicode, is_ident_start_unicode};
use xlang_frontend::span::{Pos, Spanned, Speekable};
use xlang_frontend::symbol::Symbol;

pub enum StringType {
    Long,
    Utf8,
    Utf16,
    Utf32,
}

pub enum PPTokenType {
    Identifier,
    Punct,
    Number,
    CharLit(Option<StringType>),
    StringLit(Option<StringType>),
    Newline,
}

pub struct PPToken {
    pub sym: Symbol,
    pub ty: PPTokenType,
}

pub enum Error {
    UnexpectedNewline,
    UnexpectedEof,
    UnexpectedChar(char),
}

pub type Span = xlang_frontend::span::Span;

pub type Result<T> = core::result::Result<T, Error>;

pub fn lex_char<I: Iterator<Item = char>>(
    tree: &mut PeekMoreIterator<Speekable<I>>,
) -> Option<(Pos, char)> {
    let &(pos, c) = tree.peek()?;
    if c == '?' {
        let mut tree = tree.into_rewinder();
        match tree.peek_next() {
            Some((_, '?')) => {
                // {	??<
                // }	??>
                // [	??(
                // ]	??)
                // #	??=
                // \	??/
                // ^	??'
                // |	??!
                // ~	??-
                match tree.peek_next() {
                    Some((_, '<')) => {
                        tree.accept();
                        Some((pos, '{'))
                    }
                    Some((_, '>')) => {
                        tree.accept();
                        Some((pos, '}'))
                    }
                    Some((_, '(')) => {
                        tree.accept();
                        Some((pos, '['))
                    }
                    Some((_, ')')) => {
                        tree.accept();
                        Some((pos, ']'))
                    }
                    Some((_, '=')) => {
                        tree.accept();
                        Some((pos, '#'))
                    }
                    Some((_, '/')) => {
                        tree.accept();
                        Some((pos, '\\'))
                    }
                    Some((_, '\'')) => {
                        tree.accept();
                        Some((pos, '^'))
                    }
                    Some((_, '!')) => {
                        tree.accept();
                        Some((pos, '|'))
                    }
                    Some((_, '-')) => {
                        tree.accept();
                        Some((pos, '~'))
                    }
                    _ => Some((pos, c)),
                }
            }
            _ => Some((pos, c)),
        }
    } else if c == '\\' {
        if let Some((_, '\n')) = tree.peek_next() {
            tree.peek_next().copied()
        } else {
            Some((pos, c))
        }
    } else {
        Some((pos, c))
    }
}

fn do_line_comment<I: Iterator<Item = char>>(tree: &mut PeekMoreIterator<Speekable<I>>) {
    while let Some((_, c)) = lex_char(tree) {
        if c == '\n' {
            tree.peek_last();
            break;
        }
    }
    tree.consume_peaked();
}

fn do_block_comment<I: Iterator<Item = char>>(
    tree: &mut PeekMoreIterator<Speekable<I>>,
) -> Result<()> {
    loop {
        let (_, c) = lex_char(tree).ok_or(Error::UnexpectedEof)?;
        if c == '*' {
            let (_, c) = lex_char(tree).ok_or(Error::UnexpectedEof)?;
            if c == '/' {
                tree.consume_peaked();
                break Ok(());
            }
        }
    }
}

pub fn lex_one<I: Iterator<Item = char>>(
    tree: &mut PeekMoreIterator<Speekable<I>>,
) -> Result<Option<Spanned<PPToken>>> {
    let (tok, begin) = loop {
        let (pos, c) = try_!(lex_char(tree));
        if c == '\n' {
            break (
                PPToken {
                    sym: Symbol::intern("\n"),
                    ty: PPTokenType::Newline,
                },
                pos,
            );
        } else if c.is_whitespace() {
            continue;
        } else if c == '/' {
            let mut tree = tree.into_rewinder();
            match try_!(lex_char(&mut tree)) {
                (_, '/') => do_line_comment(&mut tree),
                (_, '*') => do_block_comment(&mut tree)?,
                (_, '=') => {
                    tree.accept();
                    break (
                        PPToken {
                            sym: Symbol::intern("/="),
                            ty: PPTokenType::Punct,
                        },
                        pos,
                    );
                }
                _ => {
                    break (
                        PPToken {
                            sym: Symbol::intern("/"),
                            ty: PPTokenType::Punct,
                        },
                        pos,
                    )
                }
            }
        } else if is_ident_start_unicode(c) {
            let mut string = String::from(c);

            while let Some((_, c)) = lex_char(tree) {
                if is_ident_part_unicode(c) {
                    tree.consume_peaked();
                    string.push(c)
                } else if c == '\\' {
                    todo!("UCN")
                } else {
                    tree.reset();
                    break;
                }
            }

            break (
                PPToken {
                    sym: Symbol::intern_by_value(string),
                    ty: PPTokenType::Identifier,
                },
                pos,
            );
        } else if c == '\\' {
            todo!("UCN")
        } else if c == '0' {
            // int literal
            todo!("int literal")
        }
    };
    tree.consume_peaked();
    let end = tree.last_pos();
    let file = tree.file_name();
    let span = Span::new_simple(begin, end, file);

    Ok(Some(Spanned::new(tok, span)))
}
