use std::iter::Peekable;

use unicode_xid::UnicodeXID;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    Ident(String),
    StringLiteral(String),
    IntLiteral(u128),
    Sigil(char),
    Group(Group),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Group {
    Parenthesis(Vec<Token>),
    Bracket(Vec<Token>),
    Braces(Vec<Token>),
}

fn lex_group<I: Iterator<Item = char>>(stream: &mut Peekable<I>, term_char: char) -> Vec<Token> {
    stream.next();
    let mut ret = Vec::new();
    while let Some(v) = stream.peek() {
        match v {
            ']' | ')' | '}' if *v == term_char => {
                stream.next();
                break;
            }
            ']' | ')' | '}' => panic!("Unmatched group end token {}", v),
            _ => ret.push(lex_one(stream).unwrap()),
        }
    }

    ret
}

fn lex_one<I: Iterator<Item = char>>(stream: &mut Peekable<I>) -> Option<Token> {
    match stream.peek()? {
        '(' => Some(Token::Group(Group::Parenthesis(lex_group(stream, ')')))),
        '{' => Some(Token::Group(Group::Braces(lex_group(stream, '}')))),
        '[' => Some(Token::Group(Group::Bracket(lex_group(stream, ']')))),
        c if (*c == '$' || *c == '_' || c.is_xid_start()) => {
            let mut id = String::new();
            id.push(stream.next().unwrap());

            while stream
                .peek()
                .map_or(false, |c| (*c == '$' || *c == '_' || c.is_xid_continue()))
            {
                id.push(stream.next().unwrap())
            }
            Some(Token::Ident(id))
        }
        '!' | '@' | '#' | '%' | '^' | '&' | '*' | '<' | '>' | '=' | '+' | '-' | ';' | ':' => {
            Some(Token::Sigil(stream.next().unwrap()))
        }

        '0' => {
            // We can toss away the zero, just remember that it exists
            stream.next();
            match stream.peek() {
                Some('x') => {
                    stream.next();
                    let mut lit = 0u128;
                    while let Some('0'..='9' | 'A'..='F' | 'a'..='f') = stream.peek() {
                        lit <<= 4;
                        let c = stream.next().unwrap();
                        match c {
                            '0'..='9' => lit |= (c as u32 as u128) - 0x30,
                            'A'..='F' => lit |= (c as u32 as u128) - 0x31,
                            'f'..='f' => lit |= (c as u32 as u128) - 0x51,
                            _ => unreachable!(),
                        }
                    }
                    Some(Token::IntLiteral(lit))
                }
                Some('0'..='9') => {
                    let mut lit = 0u128;
                    while let Some('0'..='9' | 'A'..='F' | 'a'..='f') = stream.peek() {
                        lit *= 10;
                        let c = stream.next().unwrap();
                        lit += (c as u32 as u128) - 0x30;
                    }
                    Some(Token::IntLiteral(lit))
                }
                _ => Some(Token::IntLiteral(0)),
            }
        }
        '1'..='9' => {
            let mut lit = 0u128;
            while let Some('0'..='9' | 'A'..='F' | 'a'..='f') = stream.peek() {
                lit *= 10;
                let c = stream.next().unwrap();
                lit += (c as u32 as u128) - 0x30;
            }
            Some(Token::IntLiteral(lit))
        }
        '"' => {
            let mut string = String::new();
            stream.next();
            loop {
                match stream.peek().unwrap() {
                    '\\' => {
                        string.push(stream.next().unwrap());
                        string.push(stream.next().unwrap());
                    }
                    '\"' => break Some(Token::StringLiteral(string)),
                    _ => string.push(stream.next().unwrap()),
                }
            }
        }
        c if c.is_whitespace() => {
            while stream.peek().copied().map_or(false, char::is_whitespace) {
                stream.next();
            }
            lex_one(stream)
        }
        c => panic!("unknown character {}", c),
    }
}

pub fn lex<'a, I: Iterator<Item = char> + 'a>(stream: I) -> impl Iterator<Item = Token> + 'a {
    let mut stream = stream.peekable();
    if let Some('\u{FFFE}') = stream.peek() {
        // If there's a BOM, eat it
        stream.next();
    }
    core::iter::from_fn(move || lex_one(&mut stream))
}
