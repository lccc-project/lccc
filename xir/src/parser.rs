#![allow(dead_code)]

use std::convert::{TryFrom, TryInto};

use peekmore::{PeekMore as _, PeekMoreIterator};

use xlang::abi::string::FromUtf8Error;
use xlang::{abi::string::String, abi::vec::Vec, prelude::v1::Pair};

use xlang::targets::Target;
use xlang_struct::{
    Abi, AccessClass, AggregateCtor, AggregateDefinition, AggregateKind, AnnotatedElement,
    BinaryOp, Block, BlockItem, BranchCondition, CharFlags, ConversionStrength, Expr, File, FnType,
    FunctionBody, FunctionDeclaration, HashSwitch, LinearSwitch, MemberDeclaration,
    OverflowBehaviour, Path, PathComponent, PointerAliasingRule, PointerDeclarationType,
    PointerKind, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, ScalarValidity, Scope,
    ScopeMember, StackItem, StringEncoding, Switch, Type, UnaryOp, ValidRangeType, Value,
    Visibility,
};

use crate::lexer::{Group, Token};

pub fn parse_function_type<I: Iterator<Item = Token>>(stream: &mut PeekMoreIterator<I>) -> FnType {
    match stream.next().unwrap() {
        Token::Ident(id) if id == "function" => match stream.next().unwrap() {
            Token::Group(Group::Parenthesis(tok)) => {
                let mut params = Vec::new();
                let mut peekable = tok.into_iter().peekmore();
                let mut variadic = false;
                loop {
                    match peekable.peek() {
                        Some(Token::Sigil('.')) => {
                            peekable.next();
                            match peekable.next().unwrap() {
                                Token::Sigil('.') => {}
                                tok => panic!("Unexpected token {:?}", tok),
                            }
                            match peekable.next().unwrap() {
                                Token::Sigil('.') => {}
                                tok => panic!("Unexpected token {:?}", tok),
                            }

                            if let Some(tok) = peekable.next() {
                                panic!("Unexpected token {:?}", tok);
                            }
                            variadic = true;
                            break;
                        }
                        Some(_) => {
                            params.push(parse_type(&mut peekable).unwrap());
                        }
                        None => break,
                    }

                    match peekable.next() {
                        Some(Token::Sigil(',')) => continue,
                        Some(tok) => panic!("Unexpected token {:?}", tok),
                        None => break,
                    }
                }
                drop(peekable);
                match stream.peek() {
                    Some(Token::Sigil('-')) => {
                        stream.next();
                        match stream.next().unwrap() {
                            Token::Sigil('>') => FnType {
                                ret: parse_type(stream).unwrap(),
                                params,
                                tag: Abi::C,
                                variadic,
                            },
                            tok => panic!("Unexpected token {:?}", tok),
                        }
                    }
                    _ => FnType {
                        ret: Type::Null,
                        params,
                        tag: Abi::C,
                        variadic,
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
pub fn parse_type<I: Iterator<Item = Token>>(stream: &mut PeekMoreIterator<I>) -> Option<Type> {
    match stream.peek() {
        Some(Token::Group(Group::Parenthesis(_))) => {
            let group = if let Some(Token::Group(Group::Parenthesis(inner))) = stream.next() {
                inner
            } else {
                unreachable!()
            };
            let mut inner = group.into_iter().peekmore();
            Some(parse_type(&mut inner).unwrap())
        }
        Some(Token::Ident(id)) => match &**id {
            "function" | "extern" => Some(Type::FnType(xlang::abi::boxed::Box::new(
                parse_function_type(stream),
            ))),
            "void" => {
                stream.next();
                match stream.next() {
                    Some(Token::Group(Group::Parenthesis(v))) if v.is_empty() => Some(Type::Void),
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                    None => panic!("Unexpected EOF"),
                }
            }
            "uint" | "int" => {
                let signed = id == "int";
                stream.next().unwrap();
                let mut min = None;
                let mut max = None;
                let mut header = ScalarTypeHeader {
                    bitsize: 0,
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
                                        min = Some(i128::try_from(*n).expect(
                                            "oversized integer literals currently unsupported",
                                        ));
                                    }
                                    [Token::Sigil('-'), Token::IntLiteral(n)] => {
                                        min = Some(
                                            -i128::try_from(*n)
                                                .expect("integer literal too far negative"),
                                        );
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
                                        max = Some(i128::try_from(*n).expect(
                                            "oversized integer literals currently unsupported",
                                        ));
                                    }
                                    [Token::Sigil('-'), Token::IntLiteral(n)] => {
                                        max = Some(
                                            -i128::try_from(*n)
                                                .expect("integer literal too far negative"),
                                        );
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
                                        header.vectorsize =
                                            Some(TryFrom::try_from(*n).unwrap()).into();
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
                                    kind: ScalarTypeKind::Integer {
                                        signed,
                                        min: min.into(),
                                        max: max.into(),
                                    },
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
                                        header.vectorsize =
                                            Some(TryFrom::try_from(*n).unwrap()).into();
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
            "struct " => {
                stream.next();
                let defn = parse_aggregate_body(stream, AggregateKind::Struct).unwrap();
                Some(Type::Aggregate(defn))
            }
            "union" => {
                stream.next();
                let defn = parse_aggregate_body(stream, AggregateKind::Union).unwrap();
                Some(Type::Aggregate(defn))
            }
            _ => Some(Type::Named(parse_path(stream))),
        },
        Some(Token::Sigil('*')) => {
            stream.next();
            let mut alias = PointerAliasingRule::default();
            let valid_range = Pair::<ValidRangeType, u64>::default();
            let mut decl = PointerDeclarationType::default();
            let mut kind = PointerKind::default();
            let addr_space = 0;
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
                    Token::Ident(id) if id == "near" => {
                        kind = PointerKind::Near;
                    }
                    Token::Ident(id) if id == "far" => {
                        kind = PointerKind::Far;
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
                            kind,
                            addr_space,
                            inner: xlang::abi::boxed::Box::new(parse_type(stream).unwrap()),
                        }))
                    }
                }
                stream.next();
            }
        }
        Some(_) => Some(Type::Named(parse_path(stream))),
        None => None,
    }
}

pub fn parse_attr_list<I: Iterator<Item = Token>>(
    _it: &mut PeekMoreIterator<I>,
) -> AnnotatedElement {
    todo!()
}

pub fn parse_path<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> Path {
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

pub fn parse_aggregate_body<I: Iterator<Item = Token>>(
    it: &mut PeekMoreIterator<I>,
    kind: AggregateKind,
) -> Option<AggregateDefinition> {
    match it.next().unwrap() {
        Token::Sigil(';') => None,
        Token::Group(Group::Braces(tokens)) => {
            let mut it = tokens.into_iter().peekmore();

            let mut fields = Vec::new();
            loop {
                let id = match it.next() {
                    Some(Token::Ident(id)) => id,
                    None => break,
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                };

                match it.next().unwrap() {
                    Token::Sigil(':') => {}
                    tok => panic!("Unexpected token {:?}", tok),
                }

                let ty = parse_type(&mut it).unwrap();
                fields.push(Pair(id, ty));
                match it.next() {
                    Some(Token::Sigil(',')) => continue,
                    None => break,
                    Some(tok) => panic!("Unexpected token {:?}", tok),
                }
            }

            Some(AggregateDefinition {
                annotations: AnnotatedElement::default(),
                kind,
                fields,
            })
        }
        tok => panic!("Unexpected token {:?}", tok),
    }
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)] // TODO: refactor
pub fn parse_scope_member<I: Iterator<Item = Token>>(
    it: &mut PeekMoreIterator<I>,
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
                    let mut peekable = toks.into_iter().peekmore();
                    let mut variadic = false;
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
                            Some(Token::Sigil('.')) => {
                                peekable.next();
                                match peekable.next().unwrap() {
                                    Token::Sigil('.') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                                match peekable.next().unwrap() {
                                    Token::Sigil('.') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }

                                if let Some(tok) = peekable.next() {
                                    panic!("Unexpected token {:?}", tok);
                                }
                                variadic = true;
                                break;
                            }
                            Some(_) => {
                                params.push(parse_type(&mut peekable).unwrap());
                            }
                            None => break,
                        }

                        match peekable.next() {
                            Some(Token::Sigil(',')) => continue,
                            Some(tok) => panic!("Unexpected token {:?}", tok),
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
                                        variadic,
                                    },
                                    body: xlang::abi::option::None,
                                }),
                                ..ScopeMember::default()
                            },
                        )),
                        Token::Group(Group::Braces(toks)) => {
                            let mut peekable = toks.into_iter().peekmore();
                            let mut locals = Vec::new();
                            loop {
                                match peekable.peek() {
                                    Some(Token::Ident(id)) if id == "declare" => {
                                        peekable.next();
                                        match peekable.next().unwrap() {
                                            Token::Ident(id) if id.starts_with('_') => {
                                                // Assume there's increasing indecies for now
                                            }
                                            tok => panic!("Unrecognized token {:?}", tok),
                                        }
                                        match peekable.next().unwrap() {
                                            Token::Sigil(':') => {}
                                            tok => panic!("Unrecognized token {:?}", tok),
                                        }
                                        locals.push(parse_type(&mut peekable).unwrap());
                                        match peekable.next().unwrap() {
                                            Token::Sigil(';') => {}
                                            tok => panic!("Unrecognized token {:?}", tok),
                                        }
                                    }
                                    _ => break,
                                }
                            }
                            Some((
                                name,
                                ScopeMember {
                                    member_decl: MemberDeclaration::Function(FunctionDeclaration {
                                        ty: xlang_struct::FnType {
                                            ret,
                                            params,
                                            tag: Abi::C,
                                            variadic,
                                        },
                                        body: xlang::abi::option::Some(FunctionBody {
                                            locals,
                                            block: parse_block(&mut peekable),
                                        }),
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
        Token::Ident(id) if id == "struct" => {
            it.next();
            let name = parse_path(it);

            let aggr = parse_aggregate_body(it, AggregateKind::Struct);
            if let Some(aggr) = aggr {
                Some((
                    name,
                    ScopeMember {
                        member_decl: MemberDeclaration::AggregateDefinition(aggr),
                        ..ScopeMember::default()
                    },
                ))
            } else {
                Some((
                    name,
                    ScopeMember {
                        member_decl: MemberDeclaration::OpaqueAggregate(AggregateKind::Struct),
                        ..ScopeMember::default()
                    },
                ))
            }
        }
        tok => panic!("unexpected token {:?}", tok),
    }
}

pub fn parse_stack_items<I: Iterator<Item = Token>>(it: I) -> Vec<StackItem> {
    let mut peekable = it.peekmore();
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
                    kind: xlang_struct::StackValueKind::RValue,
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

pub fn parse_block<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> Block {
    let mut items = Vec::new();
    loop {
        match it.peek() {
            Some(Token::Ident(id)) if id == "end" => match it.peek_next().unwrap() {
                Token::Ident(id) if id == "block" => {
                    it.reset_cursor();
                    break Block { items };
                }
                tok => todo!("end {:?}", tok),
            },
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
pub fn parse_const<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> Value {
    match it.peek().unwrap() {
        Token::Ident(id) if id == "undef" => {
            it.next();
            match it.next().unwrap() {
                Token::Ident(id) if id == "uninit" => Value::Uninitialized(parse_type(it).unwrap()),
                Token::Ident(id) if id == "invalid" => Value::Invalid(parse_type(it).unwrap()),
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
        Token::Ident(id) if id == "label_address" => {
            it.next();
            match it.next().unwrap() {
                Token::Sigil('@') => {}
                tok => panic!("Unexpected token {:?}", tok),
            }

            match it.next().unwrap() {
                Token::IntLiteral(n) => Value::LabelAddress(n.try_into().unwrap()),
                tok => panic!("Unexpected token {:?}", tok),
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

pub fn parse_access_class<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> AccessClass {
    let mut acc = AccessClass::Normal;
    loop {
        match it.peek() {
            Some(Token::Ident(id)) if id == "atomic" => {
                if acc & AccessClass::ATOMIC_MASK != AccessClass::Normal {
                    panic!("Invalid access class, at most one atomic mode may be present");
                }
                it.next();
                match it.next().unwrap() {
                    Token::Ident(id) if id == "relaxed" => {
                        acc |= AccessClass::AtomicRelaxed;
                    }
                    Token::Ident(id) if id == "release" => {
                        acc |= AccessClass::AtomicRelease;
                    }
                    Token::Ident(id) if id == "acquire" => {
                        acc |= AccessClass::AtomicAcquire;
                    }
                    Token::Ident(id) if id == "acq_rel" => {
                        acc |= AccessClass::AtomicAcqRel;
                    }
                    Token::Ident(id) if id == "seq_cst" => {
                        acc |= AccessClass::AtomicSeqCst;
                    }
                    tok => panic!("Unexpected token {:?}", tok),
                }

                match it.peek() {
                    Some(Token::Ident(id)) if id == "fail" => {
                        it.next();
                        match it.next().unwrap() {
                            Token::Ident(id) if id == "relaxed" => {
                                acc |= AccessClass::AtomicFailRelaxed;
                            }
                            tok => panic!("Unexpected token {:?}", tok),
                        }
                    }
                    _ => {}
                }
            }
            Some(Token::Ident(id)) if id == "freeze" => {
                acc |= AccessClass::Freeze;
            }
            Some(Token::Ident(id)) if id == "volatile" => {
                acc |= AccessClass::Volatile;
            }
            Some(Token::Ident(id)) if id == "nontemporal" => {
                acc |= AccessClass::Nontemporal;
            }
            _ => break acc,
        }
    }
}

#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
pub fn parse_expr<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> Expr {
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
                Token::IntLiteral(values) => Expr::Exit {
                    values: values.try_into().unwrap(),
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
                Token::Ident(id) if id == "indirect" => return Expr::BranchIndirect,
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
        Token::Ident(id) if id == "dup" => {
            it.next();
            let n = match it.peek() {
                Some(Token::IntLiteral(n)) => {
                    let n = u32::try_from(*n).unwrap();
                    it.next();
                    n
                }
                _ => 1,
            };
            Expr::Dup(n)
        }
        Token::Ident(id) if id == "pivot" => {
            it.next();
            let n = match it.next().unwrap() {
                Token::IntLiteral(n) => u32::try_from(n).unwrap(),
                tok => panic!("Unexpected token {:?}", tok),
            };
            let m = match it.peek() {
                Some(Token::IntLiteral(n)) => {
                    let n = u32::try_from(*n).unwrap();
                    it.next();
                    n
                }
                _ => n,
            };
            Expr::Pivot(n, m)
        }
        Token::Ident(id)
            if id == "add"
                || id == "sub"
                || id == "mul"
                || id == "div"
                || id == "mod"
                || id == "lsh"
                || id == "rsh"
                || id == "bxor"
                || id == "bor"
                || id == "band"
                || id == "cmp"
                || id == "cmp_lt"
                || id == "cmp_gt"
                || id == "cmp_eq"
                || id == "cmp_ne"
                || id == "cmp_le"
                || id == "cmp_ge"
                || id == "cmp_int" =>
        {
            let op = match it.next().unwrap() {
                Token::Ident(id) if id == "add" => BinaryOp::Add,
                Token::Ident(id) if id == "sub" => BinaryOp::Sub,
                Token::Ident(id) if id == "mul" => BinaryOp::Mul,
                Token::Ident(id) if id == "div" => BinaryOp::Div,
                Token::Ident(id) if id == "mod" => BinaryOp::Mod,
                Token::Ident(id) if id == "lsh" => BinaryOp::Lsh,
                Token::Ident(id) if id == "rsh" => BinaryOp::Rsh,
                Token::Ident(id) if id == "band" => BinaryOp::BitAnd,
                Token::Ident(id) if id == "bor" => BinaryOp::BitOr,
                Token::Ident(id) if id == "bxor" => BinaryOp::BitXor,
                Token::Ident(id) if id == "cmp" => BinaryOp::Cmp,
                Token::Ident(id) if id == "cmp_int" => BinaryOp::CmpInt,
                Token::Ident(id) if id == "cmp_lt" => BinaryOp::CmpLt,
                Token::Ident(id) if id == "cmp_gt" => BinaryOp::CmpGt,
                Token::Ident(id) if id == "cmp_eq" => BinaryOp::CmpEq,
                Token::Ident(id) if id == "cmp_ne" => BinaryOp::CmpNe,
                Token::Ident(id) if id == "cmp_le" => BinaryOp::CmpLe,
                Token::Ident(id) if id == "cmp_ge" => BinaryOp::CmpGe,
                _ => unreachable!(),
            };
            let overflow = match it.peek() {
                Some(Token::Ident(id)) if id == "trap" => {
                    it.next();
                    OverflowBehaviour::Trap
                }
                Some(Token::Ident(id)) if id == "wrap" => {
                    it.next();
                    OverflowBehaviour::Trap
                }
                Some(Token::Ident(id)) if id == "checked" => {
                    it.next();
                    OverflowBehaviour::Checked
                }
                Some(Token::Ident(id)) if id == "unchecked" => {
                    it.next();
                    OverflowBehaviour::Unchecked
                }
                _ => OverflowBehaviour::Wrap,
            };
            Expr::BinaryOp(op, overflow)
        }
        Token::Ident(id) if id == "not" || id == "neg" || id == "bnot" => {
            let op = match it.next().unwrap() {
                Token::Ident(id) if id == "not" => UnaryOp::LogicNot,
                Token::Ident(id) if id == "bnot" => UnaryOp::BitNot,
                Token::Ident(id) if id == "neg" => UnaryOp::Minus,
                _ => unreachable!(),
            };
            let overflow = match it.peek() {
                Some(Token::Ident(id)) if id == "trap" => {
                    it.next();
                    OverflowBehaviour::Trap
                }
                Some(Token::Ident(id)) if id == "wrap" => {
                    it.next();
                    OverflowBehaviour::Trap
                }
                Some(Token::Ident(id)) if id == "checked" => {
                    it.next();
                    OverflowBehaviour::Checked
                }
                Some(Token::Ident(id)) if id == "unchecked" => {
                    it.next();
                    OverflowBehaviour::Unchecked
                }
                _ => OverflowBehaviour::Wrap,
            };

            Expr::UnaryOp(op, overflow)
        }
        Token::Ident(id) if id == "local" => {
            it.next();
            match it.next().unwrap() {
                Token::Ident(id) if id.starts_with('_') => {
                    let n = id[1..].parse::<u32>().unwrap();
                    Expr::Local(n)
                }
                tok => panic!("Unexpected Token {:?}", tok),
            }
        }
        Token::Ident(id) if id == "assign" => {
            it.next();
            let acc = parse_access_class(it);
            Expr::Assign(acc)
        }
        Token::Ident(id) if id == "as_rvalue" => {
            it.next();
            let acc = parse_access_class(it);
            Expr::AsRValue(acc)
        }
        Token::Ident(id) if id == "switch" => {
            it.next();

            let switch = match it.next().unwrap() {
                Token::Ident(id) if id == "hash" => {
                    let mut cases = Vec::new();
                    let mut default = None;
                    loop {
                        match it.peek().unwrap() {
                            Token::Ident(id) if id == "case" => {
                                it.next();
                                let val = parse_const(it);

                                match it.next().unwrap() {
                                    Token::Sigil(':') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                                match it.next().unwrap() {
                                    Token::Sigil('@') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }

                                let targ = match it.next().unwrap() {
                                    Token::IntLiteral(n) => u32::try_from(n).unwrap(),
                                    tok => panic!("Unexpected token {:?}", tok),
                                };
                                cases.push(Pair(val, targ));
                            }
                            Token::Ident(id) if id == "default" => {
                                it.next();

                                match it.next().unwrap() {
                                    Token::Sigil(':') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                                match it.next().unwrap() {
                                    Token::Sigil('@') => {}
                                    tok => panic!("Unexpected token {:?}", tok),
                                }

                                match it.next().unwrap() {
                                    Token::IntLiteral(n) => {
                                        default = Some(u32::try_from(n).unwrap());
                                    }
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => {
                                break Switch::Hash(HashSwitch {
                                    cases,
                                    default: default.unwrap(),
                                })
                            }
                        }
                    }
                }
                Token::Ident(id) if id == "linear" => {
                    let ty = parse_type(it).unwrap();
                    let min = match it.peek().unwrap() {
                        Token::Ident(id) if id == "min" => {
                            it.next();
                            match it.next().unwrap() {
                                Token::IntLiteral(n) => n,
                                Token::Sigil('-') => match it.next().unwrap() {
                                    Token::IntLiteral(n) => n.wrapping_neg(),
                                    tok => panic!("Unexpected token {:?}", tok),
                                },
                                tok => panic!("Unexpected token {:?}", tok),
                            }
                        }
                        _ => 0,
                    };
                    let scale = match it.peek().unwrap() {
                        Token::Ident(id) if id == "min" => {
                            it.next();
                            match it.next().unwrap() {
                                Token::IntLiteral(n) => u32::try_from(n).unwrap(),
                                tok => panic!("Unexpected token {:?}", tok),
                            }
                        }
                        _ => 1,
                    };
                    let default = match it.peek().unwrap() {
                        Token::Ident(id) if id == "default" => {
                            it.next();

                            match it.next().unwrap() {
                                Token::Sigil('@') => {}
                                tok => panic!("Unexpected token {:?}", tok),
                            }

                            match it.next().unwrap() {
                                Token::IntLiteral(n) => u32::try_from(n).unwrap(),
                                tok => panic!("Unexpected token {:?}", tok),
                            }
                        }
                        tok => panic!("Unexpected token {:?}", tok),
                    };

                    let mut cases = Vec::new();

                    loop {
                        match it.peek().unwrap() {
                            Token::Sigil('@') => {
                                it.next();
                                match it.next().unwrap() {
                                    Token::IntLiteral(n) => cases.push(u32::try_from(n).unwrap()),
                                    tok => panic!("Unexpected token {:?}", tok),
                                }
                            }
                            _ => {
                                break Switch::Linear(LinearSwitch {
                                    ty,
                                    min,
                                    scale,
                                    default,
                                    cases,
                                })
                            }
                        }
                    }
                }
                tok => panic!("Unexpected token {:?}", tok),
            };

            match it.next().unwrap() {
                Token::Ident(id) if id == "end" => {}
                tok => panic!("Unexpected token {:?}", tok),
            }
            match it.next().unwrap() {
                Token::Ident(id) if id == "switch" => Expr::Switch(switch),
                tok => panic!("Unexpected token {:?}", tok),
            }
        }
        Token::Ident(id) if id == "convert" => {
            it.next();
            let str = match it.next().unwrap() {
                Token::Ident(id) if id == "weak" => ConversionStrength::Weak,
                Token::Ident(id) if id == "strong" => ConversionStrength::Strong,
                Token::Ident(id) if id == "reinterpret" => ConversionStrength::Reinterpret,
                tok => panic!("Unexpected token {:?}", tok),
            };
            Expr::Convert(str, parse_type(it).unwrap())
        }
        Token::Ident(id) if id == "aggregate" => {
            it.next();
            let ty = parse_type(it).unwrap();
            let mut fields = Vec::new();
            match it.next().unwrap() {
                Token::Group(Group::Braces(inner)) => {
                    let mut it = inner.into_iter().peekmore();
                    loop {
                        match it.next() {
                            Some(Token::Ident(id)) => fields.push(id),
                            None => break,
                            Some(tok) => panic!("Unexpected token {:?}", tok),
                        }
                        match it.next() {
                            Some(Token::Sigil(',')) => continue,
                            None => break,
                            Some(tok) => panic!("Unexpected token {:?}", tok),
                        }
                    }
                }
                tok => panic!("Unexpected token {:?}", tok),
            }

            Expr::Aggregate(AggregateCtor { ty, fields })
        }
        Token::Ident(id) if id == "member" => {
            it.next();

            let indirect = matches!(it.peek().unwrap(), Token::Ident(id) if id == "indirect");

            let id = parse_ident_with_parens(it);

            if indirect {
                Expr::MemberIndirect(id)
            } else {
                Expr::Member(id)
            }
        }
        tok => todo!("{:?}", tok),
    }
}

pub fn parse_ident_with_parens<I: Iterator<Item = Token>>(it: &mut PeekMoreIterator<I>) -> String {
    match it.next().unwrap() {
        Token::Ident(id) => id,
        Token::Group(Group::Parenthesis(inner)) => {
            let mut it = inner.into_iter().peekmore();
            parse_ident_with_parens(&mut it)
        }
        tok => panic!("Unexpected token {:?}", tok),
    }
}

pub fn parse_file<I: Iterator<Item = Token>>(it: I, deftarg: Target) -> File {
    let mut peekable = it.peekmore();
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
