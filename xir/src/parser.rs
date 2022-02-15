#![allow(dead_code)]

use std::{
    convert::{TryFrom, TryInto},
    iter::Peekable,
};

use xlang::abi::string::FromUtf8Error;
use xlang::{abi::string::String, abi::vec::Vec, prelude::v1::Pair};

use xlang::targets::Target;
use xlang_struct::{
    Abi, AnnotatedElement, Block, BlockItem, BranchCondition, CharFlags, Expr, File, FnType,
    FunctionDeclaration, MemberDeclaration, Path, PathComponent, PointerAliasingRule,
    PointerDeclarationType, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind,
    ScalarValidity, Scope, ScopeMember, StackItem, StringEncoding, Type, ValidRangeType, Value,
    Visibility,
};

use crate::lexer::{Group, Token};

pub fn parse_function_type<I: Iterator<Item = Token>>(stream: &mut Peekable<I>) -> FnType {
    match stream.next().unwrap() {
        Token::Ident(id) if id == "function" => match stream.next().unwrap() {
            Token::Group(Group::Parenthesis(tok)) => {
                let mut params = Vec::new();
                let mut it = tok.into_iter().peekable();
                loop {
                    if let Some(ty) = parse_type(&mut it) {
                        params.push(ty);
                        match it.peek() {
                            Some(Token::Sigil(',')) => continue,
                            Some(tok) => panic!("Unexpected token {:?}", tok),
                            None => break,
                        }
                    }
                }
                match stream.peek() {
                    Some(Token::Sigil('-')) => {
                        stream.next();
                        match stream.next().unwrap() {
                            Token::Sigil('>') => FnType {
                                ret: parse_type(stream).unwrap(),
                                params,
                                tag: Abi::C,
                            },
                            tok => panic!("Unexpected token {:?}", tok),
                        }
                    }
                    _ => FnType {
                        ret: Type::Null,
                        params,
                        tag: Abi::C,
                    },
                }
            }
            tok => panic!("Unexpected Token {:?}", tok),
        },
        Token::Ident(id) if id == "extern" => todo!("extern"),
        tok => panic!("Unexpected token {:?}", tok),
    }
}

#[allow(clippy::cognitive_complexity)] // TODO: refactor
#[allow(clippy::too_many_lines)] // TODO: refactor
pub fn parse_type<I: Iterator<Item = Token>>(stream: &mut Peekable<I>) -> Option<Type> {
    match stream.peek() {
        Some(Token::Group(Group::Parenthesis(_))) => {
            let group = if let Some(Token::Group(Group::Parenthesis(inner))) = stream.next() {
                inner
            } else {
                unreachable!()
            };
            let mut inner = group.into_iter().peekable();
            Some(parse_type(&mut inner).unwrap())
        }
        Some(Token::Ident(id)) => match &**id {
            "function" | "extern" => Some(Type::FnType(xlang::abi::boxed::Box::new(
                parse_function_type(stream),
            ))),
            "uint" | "int" => {
                let signed = id == "int";
                stream.next().unwrap();
                let mut min = i128::MIN;
                let mut max = i128::MAX;
                let mut header = ScalarTypeHeader {
                    bitsize: 0,
                    vectorsize: 0,
                    ..ScalarTypeHeader::default()
                };
                loop {
                    match stream.next() {
                        Some(Token::Ident(s)) if s == "nonzero" => {
                            header.validity |= ScalarValidity::NONZERO;
                        }
                        Some(Token::Ident(s)) if s == "min" => {
                            if let Some(Token::Group(Group::Parenthesis(toks))) = stream.next() {
                                match &*toks {
                                    [Token::IntLiteral(n)] => {
                                        min = i128::try_from(*n).expect(
                                            "oversized integer literals currently unsupported",
                                        );
                                    }
                                    [Token::Sigil('-'), Token::IntLiteral(n)] => {
                                        min = -i128::try_from(*n)
                                            .expect("integer literal too far negative");
                                    }
                                    toks => panic!("Unexpected tokens: {:?}", toks),
                                }
                            } else {
                                panic!("Unexpected Token");
                            }
                        }
                        Some(Token::Ident(s)) if s == "max" => {
                            if let Some(Token::Group(Group::Parenthesis(toks))) = stream.next() {
                                match &*toks {
                                    [Token::IntLiteral(n)] => {
                                        max = i128::try_from(*n).expect(
                                            "oversized integer literals currently unsupported",
                                        );
                                    }
                                    [Token::Sigil('-'), Token::IntLiteral(n)] => {
                                        max = -i128::try_from(*n)
                                            .expect("integer literal too far negative");
                                    }
                                    toks => panic!("Unexpected tokens: {:?}", toks),
                                }
                            } else {
                                panic!("Unexpected Token");
                            }
                        }
                        Some(Token::Ident(s)) if s == "vector" => {
                            if let Some(Token::Group(Group::Parenthesis(toks))) = stream.next() {
                                match &*toks {
                                    [Token::IntLiteral(n)] => {
                                        header.vectorsize = TryFrom::try_from(*n).unwrap();
                                    }
                                    toks => panic!("Unexpected tokens: {:?}", toks),
                                }
                            } else {
                                panic!("Unexpected Token");
                            }
                        }
                        Some(Token::Group(Group::Parenthesis(toks))) => match &*toks {
                            [Token::IntLiteral(n)] => {
                                header.bitsize = TryFrom::try_from(*n).unwrap();
                                break Some(Type::Scalar(ScalarType {
                                    header,
                                    kind: ScalarTypeKind::Integer { signed, min, max },
                                }));
                            }
                            toks => panic!("Unexpected tokens: {:?}", toks),
                        },
                        Some(tok) => panic!("Unexpected token: {:?}", tok),
                        None => panic!("Unexpected end of input"),
                    }
                }
            }
            "char" | "schar" => {
                let mut flags = CharFlags::empty();
                if id == "schar" {
                    flags |= CharFlags::SIGNED;
                }
                stream.next().unwrap();
                let mut header = ScalarTypeHeader {
                    bitsize: 0,
                    vectorsize: 0,
                    ..ScalarTypeHeader::default()
                };
                loop {
                    match stream.next() {
                        Some(Token::Ident(s)) if s == "unicode" => {
                            break Some(Type::Scalar(ScalarType {
                                header,
                                kind: ScalarTypeKind::Char { flags },
                            }))
                        }
                        Some(Token::Ident(s)) if s == "vector" => {
                            if let Some(Token::Group(Group::Parenthesis(toks))) = stream.next() {
                                match &*toks {
                                    [Token::IntLiteral(n)] => {
                                        header.vectorsize = TryFrom::try_from(*n).unwrap();
                                    }
                                    toks => panic!("Unexpected tokens: {:?}", toks),
                                }
                            } else {
                                panic!("Unexpected Token");
                            }
                        }
                        Some(Token::Group(Group::Parenthesis(toks))) => match &*toks {
                            [Token::IntLiteral(n)] => {
                                header.bitsize = TryFrom::try_from(*n).unwrap();
                                break Some(Type::Scalar(ScalarType {
                                    header,
                                    kind: ScalarTypeKind::Char { flags },
                                }));
                            }
                            toks => panic!("Unexpected tokens: {:?}", toks),
                        },
                        Some(tok) => panic!("Unexpected token: {:?}", tok),
                        None => panic!("Unexpected end of input"),
                    }
                }
            }
            s => todo!("{}", s),
        },
        Some(Token::Sigil('*')) => {
            stream.next();
            let mut alias = PointerAliasingRule::default();
            let valid_range = Pair::<ValidRangeType, u64>::default();
            let mut decl = PointerDeclarationType::default();
            loop {
                match stream.peek().unwrap() {
                    Token::Ident(id) if id == "const" => decl |= PointerDeclarationType::CONST,
                    Token::Ident(id) if id == "volatile" => {
                        decl |= PointerDeclarationType::VOLATILE;
                    }
                    Token::Ident(id) if id == "ref" => decl |= PointerDeclarationType::REF,
                    Token::Ident(id) if id == "nonnull" => alias |= PointerAliasingRule::NONNULL,
                    Token::Ident(id) if id == "invalid" => alias |= PointerAliasingRule::INVALID,
                    Token::Ident(id) if id == "null_or_invalid" => {
                        alias |= PointerAliasingRule::NULL_OR_INVALID;
                    }
                    Token::Ident(id) if id == "unique" => alias |= PointerAliasingRule::UNIQUE,
                    Token::Ident(id) if id == "read_only" => {
                        alias |= PointerAliasingRule::READ_ONLY;
                    }
                    Token::Ident(id) if id == "read_shallow" => {
                        alias |= PointerAliasingRule::READ_SHALLOW;
                    }
                    Token::Ident(id) if id == "dereferenceable" => todo!("*dereferenceable"),
                    Token::Ident(id) if id == "dereference_write" => todo!("*dereference_write"),
                    Token::Ident(id) if id == "write_only" => todo!("*write_only"),
                    Token::Ident(id) if id == "null_or_dereferenceable" => {
                        todo!("*null_or_dereferenceable")
                    }
                    Token::Ident(id) if id == "null_or_dereference_write" => {
                        todo!("*null_or_dereference_write")
                    }
                    Token::Ident(id) if id == "null_or_write_only" => todo!("*null_or_write_only"),
                    _ => {
                        break Some(Type::Pointer(PointerType {
                            alias,
                            valid_range,
                            decl,
                            inner: xlang::abi::boxed::Box::new(parse_type(stream).unwrap()),
                        }))
                    }
                }
                stream.next();
            }
        }
        tok => todo!("{:?}", tok),
    }
}

pub fn parse_attr_list<I: Iterator<Item = Token>>(_it: &mut Peekable<I>) -> AnnotatedElement {
    todo!()
}

pub fn parse_path<I: Iterator<Item = Token>>(it: &mut Peekable<I>) -> Path {
    let mut path = Vec::new();
    if it.peek() == Some(&Token::Sigil(':')) {
        it.next();
        path.push(PathComponent::Root);
        assert_eq!(it.next(), Some(Token::Sigil(':')));
    }
    loop {
        match it.next().unwrap() {
            Token::Sigil('#') => match it.next().unwrap() {
                Token::StringLiteral(s) => path.push(PathComponent::SpecialComponent(s)),
                tok => panic!("Unexpected token {:?}", tok),
            },
            Token::Ident(id) => path.push(PathComponent::Text(id)),
            Token::Sigil('%') => todo!("Dependant name"),
            Token::Sigil('<') => todo!("Generics"),
            tok => panic!("Unexpected token {:?}", tok),
        }

        if it.peek() == Some(&Token::Sigil(':')) {
            it.next();
            assert_eq!(it.next(), Some(Token::Sigil(':')));
        } else {
            break Path { components: path };
        }
    }
}

#[allow(clippy::too_many_lines)] // TODO: refactor
pub fn parse_scope_member<I: Iterator<Item = Token>>(
    it: &mut Peekable<I>,
) -> Option<(Path, ScopeMember)> {
    match it.peek()? {
        Token::Ident(id) if id == "public" => {
            it.next();
            let mut mem = parse_scope_member(it).unwrap();
            mem.1.vis = Visibility::Public;
            Some(mem)
        }
        Token::Ident(id) if id == "root" => {
            it.next();
            let mut mem = parse_scope_member(it).unwrap();
            mem.1.vis = Visibility::Origin;
            Some(mem)
        }
        Token::Ident(id) if id == "module" => {
            it.next();
            let mut mem = parse_scope_member(it).unwrap();
            mem.1.vis = Visibility::Module;
            Some(mem)
        }
        Token::Ident(id) if id == "private" => {
            it.next();
            let mut mem = parse_scope_member(it).unwrap();
            mem.1.vis = Visibility::Private;
            Some(mem)
        }
        Token::Ident(id) if id == "generic" => {
            todo!("generic declaration")
        }
        Token::Sigil('#') => {
            it.next();
            let attrs = parse_attr_list(it);
            let mut mem = parse_scope_member(it).unwrap();
            mem.1.annotations = attrs;
            Some(mem)
        }
        Token::Ident(id) if id == "function" => {
            it.next();
            let name = parse_path(it);

            match it.next().unwrap() {
                Token::Group(Group::Parenthesis(toks)) => {
                    let mut params = Vec::new();
                    let mut peekable = toks.into_iter().peekable();
                    loop {
                        match peekable.peek() {
                            Some(Token::Ident(id)) if id.starts_with('_') => {
                                // do validation later
                                peekable.next();
                                match peekable.next().unwrap() {
                                    Token::Sigil(':') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }

                                params.push(parse_type(&mut peekable).unwrap());
                            }
                            Some(_) => {
                                params.push(parse_type(&mut peekable).unwrap());
                            }
                            None => break,
                        }
                    }
                    drop(peekable);

                    let ret = match it.peek().unwrap() {
                        Token::Sigil('-') => {
                            it.next();
                            assert_eq!(it.next(), Some(Token::Sigil('>')));
                            parse_type(it).unwrap()
                        }

                        _ => Type::Void,
                    };

                    match it.next().unwrap() {
                        Token::Sigil(';') => Some((
                            name,
                            ScopeMember {
                                member_decl: MemberDeclaration::Function(FunctionDeclaration {
                                    ty: xlang_struct::FnType {
                                        ret,
                                        params,
                                        tag: Abi::C,
                                    },
                                    body: xlang::abi::option::None,
                                }),
                                ..ScopeMember::default()
                            },
                        )),
                        Token::Group(Group::Braces(toks)) => {
                            let mut peekable = toks.into_iter().peekable();
                            Some((
                                name,
                                ScopeMember {
                                    member_decl: MemberDeclaration::Function(FunctionDeclaration {
                                        ty: xlang_struct::FnType {
                                            ret,
                                            params,
                                            tag: Abi::C,
                                        },
                                        body: xlang::abi::option::Some(parse_block(&mut peekable)),
                                    }),
                                    ..ScopeMember::default()
                                },
                            ))
                        }
                        tok => panic!("Unexpected token {:?}", tok),
                    }
                }
                tok => panic!("Unexpceted token {:?}", tok),
            }
        }
        Token::Ident(id) if id == "static" => {
            it.next();
            let _name = parse_path(it);
            todo!()
        }

        tok => panic!("unexpected token {:?}", tok),
    }
}

pub fn parse_stack_items<I: Iterator<Item = Token>>(it: I) -> Vec<StackItem> {
    let mut peekable = it.peekable();
    let mut ret = Vec::new();
    loop {
        match peekable.peek() {
            Some(Token::Ident(id)) if id == "lvalue" => {
                peekable.next();
                ret.push(StackItem {
                    ty: parse_type(&mut peekable).unwrap(),
                    kind: xlang_struct::StackValueKind::LValue,
                });
            }
            Some(_) => {
                ret.push(StackItem {
                    ty: parse_type(&mut peekable).unwrap(),
                    kind: xlang_struct::StackValueKind::LValue,
                });
            }
            None => break ret,
        }
        match peekable.next() {
            Some(Token::Sigil(',')) => continue,
            None => break ret,
            Some(tok) => panic!("Unexpected token {:?}", tok),
        }
    }
}

pub fn parse_block<I: Iterator<Item = Token>>(it: &mut Peekable<I>) -> Block {
    let mut items = Vec::new();
    loop {
        match it.peek() {
            Some(Token::Ident(id)) if id == "end" => {
                it.next();
                match it.next().unwrap() {
                    Token::Ident(id) if id == "block" => {
                        it.next();
                        break Block { items };
                    }
                    tok => todo!("end {:?}", tok),
                }
            }
            None => break Block { items },
            Some(Token::Ident(id)) if id == "target" => {
                it.next();
                let target: u32 = match it.next().unwrap() {
                    Token::Sigil('@') => match it.next().unwrap() {
                        Token::IntLiteral(branch) => branch.try_into().unwrap(),
                        tok => panic!("Unexpected token {:?}", tok),
                    },
                    tok => panic!("Unexpected token {:?}", tok),
                };

                match it.next().unwrap() {
                    Token::Group(Group::Bracket(group)) => items.push(BlockItem::Target {
                        num: target,
                        stack: parse_stack_items(group.into_iter()),
                    }),
                    tok => panic!("Unexpected token {:?}", tok),
                }
            }
            Some(_) => items.push(BlockItem::Expr(parse_expr(it))),
        }
    }
}

fn literal_to_xir_bytes(s: &str) -> Result<String, Vec<u8>> {
    let mut buf = Vec::with_capacity(s.len());
    let mut has_hex_escapes = false;

    let mut chars = s.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next().unwrap() {
                'n' => buf.push(b'\n'),
                'r' => buf.push(b'\r'),
                't' => buf.push(b'\t'),
                'x' => {
                    has_hex_escapes = true;
                    let a1 = chars.next().unwrap();
                    let a2 = chars.next().unwrap();
                    assert!(a1.is_digit(16) && a2.is_digit(16));
                    let val = (a1.to_digit(16).unwrap() << 4) | (a2.to_digit(16).unwrap());
                    buf.push(val.try_into().unwrap());
                }
                'u' => {
                    todo!("Unicode escapes")
                }
                '\\' => buf.push(b'\\'),
                '"' => buf.push(b'"'),
                '\'' => buf.push(b'\''),
                c => panic!("Invalid escape sequence \\{}", c),
            }
        } else {
            buf.extend_from_slice(c.encode_utf8(&mut [0u8; 4]).as_bytes());
        }
    }

    if has_hex_escapes {
        // Validate the UTF-8
        String::from_utf8(buf).map_err(FromUtf8Error::into_bytes)
    } else {
        // Else, we have to use unsafe
        // SAFETY:
        // The bytes have come from a string, and are UTF-8 validated
        // As no hex escapes are included, arbitrary bytes couldn't have found there way in
        Ok(unsafe { String::from_utf8_unchecked(buf) })
    }
}

#[allow(clippy::cognitive_complexity)] // TODO: refactor
#[allow(clippy::match_wild_err_arm)] // TODO: find a better solution
#[allow(clippy::too_many_lines)] // TODO: refactor
pub fn parse_const<I: Iterator<Item = Token>>(it: &mut Peekable<I>) -> Value {
    match it.peek().unwrap() {
        Token::Ident(id) if id == "undef" => {
            it.next();
            match it.next().unwrap() {
                Token::Ident(id) if id == "uninit" => Value::Uninitialized(parse_type(it).unwrap()),
                Token::Ident(id) if id == "invalid" => {
                    Value::Uninitialized(parse_type(it).unwrap())
                }
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Token::Ident(id) if id == "global_address" => {
            it.next();
            Value::GlobalAddress {
                ty: Type::Null,
                item: parse_path(it),
            }
        }
        _ => {
            let ty = parse_type(it).unwrap();
            match it.next().unwrap() {
                Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                    Ok(utf8) => Value::String {
                        encoding: StringEncoding::Utf8,
                        utf8,
                        ty,
                    },
                    Err(content) => Value::ByteString { content },
                },
                Token::Ident(id) if id == "utf8" => match it.next().unwrap() {
                    Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                        Ok(utf8) => Value::String {
                            encoding: StringEncoding::Utf8,
                            utf8,
                            ty,
                        },
                        Err(_) => panic!("Required unicode string for encoding {}", id),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                Token::Ident(id) if id == "utf16le" => match it.next().unwrap() {
                    Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                        Ok(utf8) => Value::String {
                            encoding: StringEncoding::Utf16LE,
                            utf8,
                            ty,
                        },
                        Err(_) => panic!("Required unicode string for encoding {}", id),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                Token::Ident(id) if id == "utf16be" => match it.next().unwrap() {
                    Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                        Ok(utf8) => Value::String {
                            encoding: StringEncoding::Utf16BE,
                            utf8,
                            ty,
                        },
                        Err(_) => panic!("Required unicode string for encoding {}", id),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                Token::Ident(id) if id == "utf32le" => match it.next().unwrap() {
                    Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                        Ok(utf8) => Value::String {
                            encoding: StringEncoding::Utf32LE,
                            utf8,
                            ty,
                        },
                        Err(_) => panic!("Required unicode string for encoding {}", id),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                Token::Ident(id) if id == "utf32be" => match it.next().unwrap() {
                    Token::StringLiteral(utf8) => match literal_to_xir_bytes(&utf8) {
                        Ok(utf8) => Value::String {
                            encoding: StringEncoding::Utf32BE,
                            utf8,
                            ty,
                        },
                        Err(_) => panic!("Required unicode string for encoding {}", id),
                    },
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                Token::IntLiteral(i) => match ty {
                    Type::Scalar(ty) => Value::Integer { ty, val: i },
                    ty => panic!("Unexpected type {:?} expected a scalar type", ty),
                },
                Token::Sigil('-') => match it.next().unwrap() {
                    Token::IntLiteral(i) => match ty {
                        Type::Scalar(ty) => Value::Integer {
                            ty,
                            val: i.wrapping_neg(),
                        },
                        ty => panic!("Unexpected type {:?} expected a scalar type", ty),
                    },
                    tok => panic!("Unexpected token {:?}", tok),
                },
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
    }
}

pub fn parse_expr<I: Iterator<Item = Token>>(it: &mut Peekable<I>) -> Expr {
    match it.peek().unwrap() {
        Token::Ident(id) if id == "const" => {
            it.next();
            Expr::Const(parse_const(it))
        }
        Token::Ident(id) if id == "call" => {
            it.next();
            Expr::CallFunction(parse_function_type(it))
        }
        Token::Ident(id) if id == "pop" => {
            it.next();
            match it.peek() {
                Some(Token::IntLiteral(i)) => {
                    let val = *i;
                    it.next();
                    Expr::Pop(val.try_into().unwrap())
                }
                _ => Expr::Pop(1),
            }
        }
        Token::Ident(id) if id == "exit" => {
            it.next();
            match it.next().unwrap() {
                Token::Ident(id) if id == "block" => match it.next().unwrap() {
                    Token::Ident(id) if id.starts_with('$') => {
                        let blk = id[1..].parse::<u32>().unwrap();
                        match it.next().unwrap() {
                            Token::IntLiteral(values) => Expr::ExitBlock {
                                blk,
                                values: values.try_into().unwrap(),
                            },
                            tok => panic!("Unexpected Token {:?}", tok),
                        }
                    }
                    tok => panic!("Unexpected Token {:?}", tok),
                },
                tok => panic!("Unexpected Token {:?}", tok),
            }
        }
        Token::Ident(id) if id == "branch" => {
            it.next();
            let cond = match it.next().unwrap() {
                Token::Ident(id) if id == "always" => BranchCondition::Always,
                Token::Ident(id) if id == "never" => BranchCondition::Never,
                Token::Ident(id) if id == "equal" => BranchCondition::Equal,
                Token::Ident(id) if id == "not_equal" => BranchCondition::NotEqual,
                Token::Ident(id) if id == "less" => BranchCondition::Less,
                Token::Ident(id) if id == "greater" => BranchCondition::Greater,
                Token::Ident(id) if id == "less_equal" => BranchCondition::LessEqual,
                Token::Ident(id) if id == "greater_equal" => BranchCondition::GreaterEqual,
                tok => panic!("Unexpected token {:?}", tok),
            };
            match it.next().unwrap() {
                Token::Sigil('@') => match it.next().unwrap() {
                    Token::IntLiteral(target) => Expr::Branch {
                        cond,
                        target: target.try_into().unwrap(),
                    },
                    tok => panic!("Unexpected token {:?}", tok),
                },
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        tok => todo!("{:?}", tok),
    }
}

pub fn parse_file<I: Iterator<Item = Token>>(it: I, deftarg: Target) -> File {
    let mut peekable = it.peekable();
    match peekable.peek() {
        Some(Token::Ident(id)) if id == "target" => {
            peekable.next();
            match peekable.next() {
                Some(Token::StringLiteral(s)) => {
                    assert_eq!(deftarg, Target::from(target_tuples::Target::parse(&s)));
                }
                Some(tok) => panic!("Unexpected Token {:?}", tok),
                None => panic!("Unexpected End of File"),
            }

            assert!(peekable.next() == Some(Token::Sigil(';')));
        }
        _ => {}
    }

    File {
        target: deftarg,
        root: Scope {
            annotations: AnnotatedElement::default(),
            members: core::iter::from_fn(move || parse_scope_member(&mut peekable)).collect(),
        },
    }
}
