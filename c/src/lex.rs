static KEYWORDS: [&'static str; 1] = ["auto"];

// 6.4: Lexical Elements
#[allow(dead_code)]
#[derive(Debug)]
pub enum Token {
    Keyword(String),
    Identifier(String),
    Constant(String),
    StringLiteral(String),
    Punctuator(String),
}

pub fn lex(file: &mut dyn Iterator<Item = char>) -> Vec<Token> {
    let mut result = Vec::new();
    while let Some(c) = file.next() {
        match c {
            x if x.is_alphabetic() || x == '_' => {
                let mut token = String::from(x);
                while let Some(c) = file.next() {
                    if c.is_alphabetic() || c == '_' || c.is_numeric() {
                        token.push(c);
                    } else {
                        break;
                    }
                }
                if KEYWORDS.contains(&&*token) {
                    result.push(Token::Keyword(token));
                } else {
                    result.push(Token::Identifier(token));
                }
            }
            _ => todo!(),
        }
    }
    result
}
