use core::iter::Peekable;

// 6.4.1: Keywords
static KEYWORDS: [&str; 47] = [
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Decimal128",
    "_Decimal32",
    "_Decimal64",
    "_Generic",
    "_Imaginary",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
];

// 6.4.4.4: Character constants
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CharacterEncoding {
    Integer,
    Utf,
    Wide,
}

// 6.4.4: Constants
#[allow(dead_code)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Constant {
    Integer(String),
    Floating(String),
    Character(CharacterEncoding, String),
}

// 6.4.5: String literals
#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StringEncoding {
    Character,
    Utf,
    Wide,
}

// 6.4: Lexical elements
#[allow(dead_code)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Constant(Constant),
    StringLiteral(StringEncoding, String),
    Punctuator(String),
}

pub fn diagnostic() -> ! {
    eprintln!("Something didn't work. Better messages coming soon.");
    std::process::exit(1);
}

pub fn string_literal<I: Iterator<Item = char>>(file: &mut Peekable<I>) -> String {
    if file.next() == Some('"') {
        let mut result = String::new();
        let mut hit_end_quote = false;
        while let Some(&c) = file.peek() {
            if c == '\\' {
                todo!();
            } else if c == '"' {
                file.next();
                hit_end_quote = true;
                break;
            } else {
                file.next();
                result.push(c);
            }
        }
        if !hit_end_quote {
            diagnostic()
        }
        result
    } else {
        diagnostic()
    }
}

pub fn lex<I: Iterator<Item = char>>(file: &mut I) -> Vec<Token> {
    let mut file = file.peekable();
    let mut result = Vec::new();
    while let Some(&c) = file.peek() {
        match c {
            x if x.is_alphabetic() || x == '_' => {
                // 6.4.2: Identifiers
                file.next();
                let mut token = String::from(x);
                while let Some(&c) = file.peek() {
                    if c.is_alphabetic() || c == '_' || c.is_numeric() {
                        file.next();
                        token.push(c);
                    } else {
                        break;
                    }
                }
                if KEYWORDS.contains(&&*token) {
                    result.push(Token::Keyword(token));
                } else {
                    let c = file.peek();
                    if c == Some(&'\'') {
                        todo!();
                    } else if c == Some(&'"') {
                        let string = string_literal(&mut file);
                        let encoding = match &*token {
                            "u8" => StringEncoding::Utf,
                            "L" | "u" | "U" => StringEncoding::Wide,
                            _ => diagnostic(),
                        };
                        result.push(Token::StringLiteral(encoding, string));
                    } else {
                        result.push(Token::Identifier(token));
                    }
                }
            }
            x if x.is_whitespace() => {
                file.next();
            }
            '[' | ']' | '(' | ')' | '{' | '}' | '~' | '?' | ';' | ',' => {
                file.next();
                result.push(Token::Punctuator(String::from(c)));
            }
            '*' | '!' | '%' => {
                file.next();
                if file.peek() == Some(&'=') {
                    file.next();
                    result.push(Token::Punctuator(String::from(c) + "="));
                } else {
                    result.push(Token::Punctuator(String::from(c)));
                }
            }
            '"' => {
                result.push(Token::StringLiteral(
                    StringEncoding::Character,
                    string_literal(&mut file),
                ));
            }
            _ => todo!("{}", c),
        }
    }
    result
}
