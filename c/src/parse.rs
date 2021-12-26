use crate::lex::Token;
use core::iter::Peekable;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveType {
    Char,
    Int,
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Function {
        ret: Box<Type>,
        parameters: Vec<(FullType, Option<String>)>,
    },
    Primitive(PrimitiveType),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Type {
    base: BaseType,
    constant: bool,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Pointer {
    constant: bool,
    restrict: bool,
    sub_ptr: Option<Box<Pointer>>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct FullType {
    inner: Type,
    pointer: Option<Pointer>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Expression {
    FunctionCall {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Identifier(String),
    String(String),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Initializer {
    Expression(Expression),
    Function(Vec<Statement>),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Declaration {
    ty: Type,
    pointer: Option<Pointer>,
    name: String,
    initializer: Option<Initializer>,
}

#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_const_for_fn)]
fn diagnostic() -> ! {
    panic!("Incorrect program (better messages coming later)")
}

fn parse_identifier<'a, I: Iterator<Item = &'a Token>>(tokens: &mut I) -> String {
    let next = tokens.next();
    if let Some(Token::Identifier(id)) = next {
        String::from(id)
    } else {
        dbg!(next);
        diagnostic()
    }
}

fn parse_type<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Type {
    let mut base = None;
    let mut constant = false;
    while let Some(&t) = tokens.peek() {
        if *t == Token::Keyword(String::from("char")) {
            tokens.next();
            if base.is_some() {
                diagnostic();
            }
            base = Some(BaseType::Primitive(PrimitiveType::Char));
        } else if *t == Token::Keyword(String::from("const")) {
            tokens.next();
            constant = true;
        } else if *t == Token::Keyword(String::from("int")) {
            tokens.next();
            if base.is_some() {
                diagnostic();
            }
            base = Some(BaseType::Primitive(PrimitiveType::Int));
        } else if let Token::Punctuator(_) = t {
            break;
        } else if let Token::Identifier(_) = t {
            if base.is_some() {
                break;
            } else {
                tokens.next();
                todo!();
            }
        } else {
            dbg!(t);
            diagnostic();
        }
    }
    let base = base.unwrap_or(BaseType::Primitive(PrimitiveType::Int));
    Type { base, constant }
}

fn parse_primary_expression<'a, I: Iterator<Item = &'a Token>>(
    tokens: &mut Peekable<I>,
) -> Expression {
    let next = tokens.next();
    if let Some(Token::Identifier(id)) = next {
        Expression::Identifier(id.clone())
    } else if let Some(Token::StringLiteral(_, str)) = next {
        Expression::String(str.clone())
    } else {
        diagnostic()
    }
}

fn parse_expression<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Expression {
    let mut lhs = parse_primary_expression(tokens);
    while let Some(t) = tokens.peek() {
        if let Token::Punctuator(p) = t {
            if p == ";" || p == "," || p == ")" {
                break;
            } else if p == "(" {
                tokens.next();
                let mut args = Vec::new();
                let mut comma_expected = false;
                let mut trailing_comma = false;
                while let Some(&t) = tokens.peek() {
                    if t == &Token::Punctuator(String::from(")")) {
                        if trailing_comma {
                            diagnostic();
                        }
                        tokens.next();
                        break;
                    } else if comma_expected {
                        if t == &Token::Punctuator(String::from(",")) {
                            tokens.next();
                            trailing_comma = true;
                            comma_expected = false;
                            continue;
                        }
                        diagnostic();
                    } else {
                        args.push(parse_expression(tokens));
                        trailing_comma = false;
                        comma_expected = true;
                    }
                }
                lhs = Expression::FunctionCall {
                    callee: Box::new(lhs),
                    args,
                };
            }
        } else {
            diagnostic();
        }
    }
    lhs
}

fn parse_statement<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Statement {
    let result = parse_expression(tokens);
    if tokens.next() != Some(&Token::Punctuator(String::from(";"))) {
        diagnostic();
    }
    Statement::Expression(result)
}

fn parse_code_block<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Vec<Statement> {
    let mut result = Vec::new();
    while let Some(&t) = tokens.peek() {
        if t == &Token::Punctuator(String::from("}")) {
            break;
        } else {
            result.push(parse_statement(tokens));
        }
    }
    if tokens.next() != Some(&Token::Punctuator(String::from("}"))) {
        diagnostic();
    }
    result
}

fn parse_declaration<'a, I: Iterator<Item = &'a Token>>(tokens: &mut Peekable<I>) -> Declaration {
    let mut ty = parse_type(tokens); // No implicit int for you (yet).
    let pointer = None;
    let name = parse_identifier(tokens); // No pointer for you (yet); also, no multiple variables on a line.
    let initializer = if let Some(Token::Punctuator(p)) = tokens.next() {
        if p == ";" {
            None
        } else if p == "=" {
            todo!()
        } else if p == "(" {
            let mut parameters = Vec::new();
            while let Some(&t) = tokens.peek() {
                if t == &Token::Punctuator(String::from(")")) {
                    tokens.next();
                    break;
                } else {
                    let ty = parse_type(tokens);
                    let mut pointer = None;
                    while let Some(Token::Punctuator(p)) = tokens.peek() {
                        if p != "*" {
                            break;
                        }
                        tokens.next();
                        let mut result_ptr = Pointer {
                            constant: false,
                            restrict: false,
                            sub_ptr: pointer.map(Box::new),
                        };
                        while let Some(Token::Keyword(kw)) = tokens.peek() {
                            if kw == "const" {
                                result_ptr.constant = true;
                            } else if kw == "restrict" {
                                result_ptr.restrict = true;
                            }
                        }
                        pointer = Some(result_ptr);
                    }
                    let name = if let Some(Token::Identifier(id)) = tokens.peek() {
                        tokens.next();
                        Some(id.clone())
                    } else {
                        None
                    };
                    parameters.push((FullType { inner: ty, pointer }, name));
                }
            }
            ty = Type {
                base: BaseType::Function {
                    ret: Box::new(ty),
                    parameters,
                },
                constant: false,
            };
            if let Some(Token::Punctuator(p)) = tokens.next() {
                if p == "{" {
                    Some(Initializer::Function(parse_code_block(tokens)))
                } else if p == ";" {
                    None
                } else {
                    diagnostic()
                }
            } else {
                diagnostic()
            }
        } else {
            diagnostic()
        }
    } else {
        todo!()
    };
    Declaration {
        ty,
        pointer,
        name,
        initializer,
    }
}

pub fn parse(tokens: &[Token]) -> Vec<Declaration> {
    let mut tokens = tokens.iter().peekable();
    let mut result = Vec::new();
    while tokens.peek().is_some() {
        result.push(parse_declaration(&mut tokens));
    }
    result
}
