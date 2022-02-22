use std::{fmt::Write, iter::Peekable};
use xlang::ir::PathComponent;

#[derive(Debug)]
enum Token {
    Parens(Vec<Token>),
    Id(String),
    Integer(u128),
    Sigil(char),
}

fn lex_group<I: Iterator<Item = char>>(it: &mut Peekable<I>) -> Vec<Token> {
    let mut group = Vec::new();
    loop {
        match it.peek().unwrap() {
            ')' => {
                it.next();
                break group;
            }
            x if x.is_whitespace() => loop {
                match it.peek().unwrap() {
                    x if x.is_whitespace() => {
                        it.next();
                    }
                    _ => break,
                }
            },
            _ => group.push(lex_one(it).unwrap()),
        }
    }
}

fn lex_one<I: Iterator<Item = char>>(it: &mut Peekable<I>) -> Option<Token> {
    match it.next()? {
        '(' => Some(Token::Parens(lex_group(it))),

        x @ ('-' | '+' | '<' | '>' | '/' | '*' | '[' | ']' | ',' | '%') => Some(Token::Sigil(x)),
        x if x.is_whitespace() => loop {
            match it.peek() {
                Some(x) if x.is_whitespace() => {
                    it.next();
                }
                _ => break lex_one(it),
            }
        },
        x if x.is_ascii_digit() => {
            let mut num = x.to_digit(10).unwrap() as u128;
            while let Some(d) = it.peek().and_then(|c| c.to_digit(10)) {
                it.next();
                num *= 10;
                num += d as u128;
            }
            Some(Token::Integer(num))
        }
        x => {
            let mut name = String::new();
            name.push(x);
            loop {
                match it.peek() {
                    None
                    | Some('-' | '+' | '<' | '>' | '/' | '*' | '[' | ']' | ',' | '%' | ')') => {
                        break Some(Token::Id(name));
                    }
                    Some(_) => {
                        name.push(it.next().unwrap());
                    }
                }
            }
        }
    }
}

fn lex_sig(st: &str) -> impl Iterator<Item = Token> + '_ {
    let mut peekable = st.chars().peekable();
    core::iter::from_fn(move || lex_one(&mut peekable))
}

fn parse_type<I: Iterator<Item = Token>>(it: &mut Peekable<I>) -> Option<String> {
    match it.next() {
        Some(Token::Id(n)) if n == "void" => Some("v".to_string()),
        Some(tok) => todo!("{:?}", tok),
        None => None,
    }
}

/// Mangles a xir path acording to the Itanium ABI with the LCRust v0 extensions
pub fn mangle_itanium(path: &[PathComponent]) -> String {
    let mut name = String::from("_ZN");
    for p in path {
        match p {
            PathComponent::Root => unreachable!("No roots"),
            PathComponent::Text(n) => write!(name, "{}{}", n.len(), n).unwrap(),
            PathComponent::SpecialComponent(s) => {
                if s == "" {
                    break;
                } else if let Some(s) = s.strip_prefix("signature") {
                    name.push('E');
                    let mut tokens = lex_sig(s).peekable();
                    match tokens.next().unwrap() {
                        Token::Parens(g) => {
                            let mut iter = g.into_iter().peekable();
                            while let Some(s) = parse_type(&mut iter) {
                                name.push_str(&s);
                            }
                            match tokens.next() {
                                Some(Token::Sigil('-')) => match tokens.next().unwrap() {
                                    Token::Sigil('>') => {
                                        name += &parse_type(&mut tokens).unwrap();
                                        return name;
                                    }
                                    tok => panic!("Unexpected Token {:?}", tok),
                                },
                                None => return name,
                                Some(tok) => panic!("Unexpected Token {:?}", tok),
                            }
                        }
                        tok => panic!("Unexpected signature component {:?}", tok),
                    }
                } else {
                    todo!("Special component #{:?}", s)
                }
            }
        }
    }
    name
}
