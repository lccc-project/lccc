#![allow(clippy::module_name_repetitions)]

use crate::sema::{
    Abi, BinaryOp, Declaration, Definition, Expression, FunctionSignature, Identifier, IntType,
    LangItem, Mangling, Mutability, Program, Safety, Statement, Type, Visibility,
};

use std::convert::TryInto; // TODO: Remove when we move to edition 2021

use xlang::{
    abi::{
        self,
        option::Option::{None as AbiNone, Some as AbiSome},
    },
    ir,
};

fn signature_component(signature: &FunctionSignature) -> ir::PathComponent {
    let mut result = abi::string::String::new();
    result.push_str("signature(");
    if signature.params.is_empty() {
        result.push_str("void");
    } else {
        todo!("functions with parameters")
    }
    result.push(')');
    if !signature.return_ty.is_unit() {
        todo!("functions with non-unit return types")
    }
    ir::PathComponent::SpecialComponent(result)
}

#[allow(clippy::single_match_else)]
fn identifier_to_path(id: Identifier, signature: Option<&FunctionSignature>) -> ir::Path {
    let Identifier::Basic {
        mangling,
        name,
        link_name,
    } = id;
    match (link_name, mangling) {
        (Some(name), _) => ir::Path {
            components: abi::vec![ir::PathComponent::Text(abi::string::String::from(&name))],
        },
        (None, Some(Mangling::C)) => ir::Path {
            components: abi::vec![ir::PathComponent::Text(abi::string::String::from(&name))],
        },
        (None, _) => {
            // Assume None == Mangling::Rust
            let mut components = abi::vec![
                ir::PathComponent::Text(abi::string::String::from("__crate_name_placeholder__")),
                ir::PathComponent::Text(abi::string::String::from(&name))
            ];
            if let Some(signature) = signature {
                components.push(signature_component(signature));
            }
            ir::Path { components }
        }
    }
}

fn irgen_type(ty: Type) -> ir::Type {
    match ty {
        Type::Array(ty, size) => ir::Type::Array(abi::boxed::Box::new(ir::ArrayType {
            ty: irgen_type(*ty),
            len: ir::Value::Integer {
                ty: if let ir::Type::Scalar(ty) = irgen_type(Type::Integer(IntType::Usize)) {
                    ty
                } else {
                    unreachable!()
                },
                val: size.try_into().unwrap(),
            },
        })),
        Type::Function(sig) => ir::Type::FnType(abi::boxed::Box::new(sig_to_fn_type(sig))),
        Type::Integer(ty) => ir::Type::Scalar(ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: match ty {
                    IntType::I8 | IntType::U8 => 8,
                    IntType::I16 | IntType::U16 => 16,
                    IntType::I32 | IntType::U32 => 32,
                    IntType::I64 | IntType::U64 | IntType::Isize | IntType::Usize => 64,
                    _ => todo!("unsupported integer type"),
                },
                ..ir::ScalarTypeHeader::default()
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: ty.is_signed(),
                min: xlang::abi::option::None,
                max: xlang::abi::option::None,
            },
        }),
        Type::Pointer {
            mutability,
            underlying,
        } => ir::Type::Pointer(ir::PointerType {
            alias: ir::PointerAliasingRule::default(),
            valid_range: abi::pair::Pair::default(),
            kind: ir::PointerKind::default(),
            addr_space: 0,
            decl: match mutability {
                Mutability::Const => ir::PointerDeclarationType::CONST,
                Mutability::Mut => ir::PointerDeclarationType::empty(),
            },
            inner: abi::boxed::Box::new(irgen_type((*underlying).clone())),
        }),
        Type::Reference {
            mutability,
            underlying,
        } => ir::Type::Pointer(ir::PointerType {
            alias: ir::PointerAliasingRule::READ_ONLY,
            valid_range: abi::pair::Pair(ir::ValidRangeType::Dereference, 0),
            kind: ir::PointerKind::default(),
            decl: match mutability {
                Mutability::Const => {
                    ir::PointerDeclarationType::CONST | ir::PointerDeclarationType::REF
                }
                Mutability::Mut => ir::PointerDeclarationType::REF,
            },
            addr_space: 0,
            inner: abi::boxed::Box::new(irgen_type((*underlying).clone())),
        }),
        Type::Struct { fields, .. } => ir::Type::Aggregate(ir::AggregateDefinition {
            annotations: ir::AnnotatedElement {
                annotations: abi::vec![ir::Annotation {
                    items: abi::vec![ir::AnnotationItem::Meta(
                        ir::Path {
                            components: abi::vec![ir::PathComponent::Text(
                                abi::string::String::from("sort_layout")
                            )]
                        },
                        abi::boxed::Box::new(ir::Annotation {
                            items: abi::vec![ir::AnnotationItem::Identifier(ir::Path {
                                components: abi::vec![ir::PathComponent::Text(
                                    abi::string::String::from("alignment")
                                )]
                            })]
                        })
                    )]
                }],
            },
            kind: ir::AggregateKind::Struct,
            fields: fields.map_or_else(&abi::vec::Vec::new, |x| {
                x.into_iter()
                    .map(|(name, ty)| {
                        abi::pair::Pair(abi::string::String::from(&name), irgen_type(ty))
                    })
                    .collect::<Vec<abi::pair::Pair<abi::string::String, ir::Type>>>()
                    .into()
            }),
        }),
        Type::Tuple(list) => ir::Type::Product(
            list.into_iter()
                .map(&irgen_type)
                .collect::<Vec<ir::Type>>()
                .into(),
        ),
        Type::Never => ir::Type::Scalar(ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: 0,
                validity: ir::ScalarValidity::NONZERO,
                ..ir::ScalarTypeHeader::default()
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: false,
                min: xlang::abi::option::None,
                max: xlang::abi::option::None,
            },
        }),
        x => todo!("{:?}", x),
    }
}

fn irgen_expr(
    expr: Expression,
    n: u32,
    locals: &mut Vec<(Identifier, ir::Type)>,
) -> Vec<ir::BlockItem> {
    match expr {
        Expression::BinaryExpression { lhs, op, rhs, .. } => {
            let mut result = irgen_expr(*lhs, n, locals);
            result.append(&mut irgen_expr(*rhs, n, locals));
            result.push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                match op {
                    BinaryOp::Add => ir::BinaryOp::Add,
                    BinaryOp::Divide => ir::BinaryOp::Div,
                    BinaryOp::Multiply => ir::BinaryOp::Mul,
                    BinaryOp::Subtract => ir::BinaryOp::Sub,
                    x => todo!("{}", x),
                },
                ir::OverflowBehaviour::Wrap,
            )));
            result
        }
        Expression::Cast { expr, target } => {
            let mut result = irgen_expr(*expr, n, locals);
            result.push(ir::BlockItem::Expr(ir::Expr::Convert(
                ir::ConversionStrength::Reinterpret,
                irgen_type(target),
            )));
            result
        }
        Expression::FunctionCall { func, args } => {
            let fn_type = if let Type::Function(sig) = func.ty() {
                sig_to_fn_type(sig)
            } else {
                unreachable!()
            };
            let mut result = Vec::new();
            result.append(&mut irgen_expr(*func, n, locals));
            for arg in args {
                result.append(&mut irgen_expr(arg, n, locals));
            }
            result.push(ir::BlockItem::Expr(ir::Expr::CallFunction(fn_type)));
            result
        }
        Expression::Identifier { id, ty: Some(ty) } => match id.mangling() {
            Some(Mangling::Local) => vec![
                ir::BlockItem::Expr(ir::Expr::Local(
                    locals
                        .iter()
                        .enumerate()
                        .rev()
                        .find(|(_, (local_id, _))| *local_id == id)
                        .unwrap()
                        .0
                        .try_into()
                        .unwrap(),
                )),
                ir::BlockItem::Expr(ir::Expr::AsRValue(ir::AccessClass::Normal)),
            ],
            _ => vec![ir::BlockItem::Expr(ir::Expr::Const(
                ir::Value::GlobalAddress {
                    ty: irgen_type(ty.clone()),
                    item: identifier_to_path(
                        id,
                        match &ty {
                            Type::Function(sig) => Some(sig),
                            _ => None,
                        },
                    ),
                },
            ))],
        },
        ref x @ Expression::IntegerLiteral { .. } => {
            let ty = irgen_type(x.ty());
            if let Expression::IntegerLiteral { val, .. } = x {
                vec![ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Integer {
                    ty: ty.try_into().unwrap(),
                    val: *val,
                }))]
            } else {
                unreachable!()
            }
        }
        Expression::FieldAccess { lhs, name, ty } => {
            let mut result = irgen_expr(*lhs, n, locals);
            result.push(ir::BlockItem::Expr(ir::Expr::Member((&name).into())));
            result.push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                ir::AccessClass::Normal,
            )));
            result
        }
        ref x @ Expression::StringLiteral { .. } => {
            let ty = irgen_type(x.ty());
            if let Expression::StringLiteral { val, .. } = x {
                vec![ir::BlockItem::Expr(ir::Expr::Const(ir::Value::String {
                    encoding: ir::StringEncoding::Utf8,
                    utf8: val.into(),
                    ty,
                }))]
            } else {
                unreachable!()
            }
        }
        Expression::StructInitializer { args, ty } => {
            let ty = irgen_type(ty);
            let mut fields = abi::vec::Vec::new();
            let mut result = Vec::new();
            for (name, arg) in args {
                fields.push((&name.to_string()).into());
                result.append(&mut irgen_expr(arg, n, locals));
            }
            result.push(ir::BlockItem::Expr(ir::Expr::Aggregate(
                ir::AggregateCtor { ty, fields },
            )));
            result
        }
        Expression::UnsafeBlock {
            block,
            ty: Some(ty),
        } => {
            let mut result = irgen_block(block, n + 1, locals, false);
            if ty.is_unit() {
                result.push(ir::BlockItem::Expr(ir::Expr::Aggregate(
                    ir::AggregateCtor {
                        ty: irgen_type(ty),
                        fields: abi::vec::Vec::new(),
                    },
                )));
            }
            result
        }
        _ => todo!("{:?}", expr),
    }
}

fn irgen_block(
    block: Vec<Statement>,
    block_num: u32,
    locals: &mut Vec<(Identifier, ir::Type)>,
    exit: bool,
) -> Vec<ir::BlockItem> {
    let mut result = Vec::new();
    let mut has_expr = false;
    for statement in block {
        match statement {
            Statement::Bind { target, value } => {
                has_expr = false;
                let ty = irgen_type(value.ty());
                result.push(ir::BlockItem::Expr(ir::Expr::Local(
                    locals.len().try_into().unwrap(),
                )));
                result.append(&mut irgen_expr(value, block_num, locals));
                result.push(ir::BlockItem::Expr(ir::Expr::Assign(
                    ir::AccessClass::Normal,
                )));
                locals.push((target, ty));
            }
            Statement::Discard(expr) => {
                has_expr = false;
                result.append(&mut irgen_expr(expr, block_num, locals));
                result.push(ir::BlockItem::Expr(ir::Expr::Pop(1)));
            }
            Statement::Expression(expr) => {
                has_expr = true;
                result.append(&mut irgen_expr(expr, block_num, locals));
            }
        }
    }
    if exit {
        if has_expr {
            result.push(ir::BlockItem::Expr(ir::Expr::Exit { values: 1 }));
        } else {
            result.push(ir::BlockItem::Expr(ir::Expr::Exit { values: 0 }));
        }
    }
    result.into()
}

fn sig_to_fn_type(sig: FunctionSignature) -> ir::FnType {
    ir::FnType {
        ret: irgen_type(*sig.return_ty),
        params: sig
            .params
            .into_iter()
            .map(&irgen_type)
            .collect::<Vec<ir::Type>>()
            .into(),
        tag: ir::Abi::C,
        variadic: false,
    }
}

pub fn irgen_definition(
    definition: &Definition,
    declarations: &[Declaration],
    file: &mut ir::File,
) {
    let Definition::Function { name, body, .. } = definition;
    if let Declaration::Function { sig, .. } = declarations
        .iter()
        .find(|x| x.name().matches(name))
        .unwrap()
    {
        let mut locals = Vec::new();
        let block = ir::Block {
            items: irgen_block(body.clone(), 0, &mut locals, true).into(),
        };
        file.root.members.insert(
            identifier_to_path(name.clone(), Some(sig)),
            ir::ScopeMember {
                vis: ir::Visibility::Public,
                member_decl: ir::MemberDeclaration::Function(ir::FunctionDeclaration {
                    ty: sig_to_fn_type(sig.clone()),
                    body: AbiSome(ir::FunctionBody {
                        locals: locals.into_iter().map(|x| x.1).collect(),
                        block,
                    }),
                }),
                ..ir::ScopeMember::default()
            },
        );
    } else {
        panic!("found declaration of function, but declaration wasn't of a function");
    }
}

pub fn irgen_main(main: &Identifier, declarations: &[Declaration], file: &mut ir::File) {
    if let Declaration::Function { sig: main_sig, .. } = declarations
        .iter()
        .find(|x| x.name().matches(main))
        .unwrap()
    {
        let body = vec![
            Statement::Discard(Expression::FunctionCall {
                func: Box::new(Expression::Identifier {
                    id: main.clone(),
                    ty: Some(Type::Function(main_sig.clone())),
                }),
                args: Vec::new(),
            }),
            Statement::Expression(Expression::IntegerLiteral {
                val: 0,
                ty: Some(IntType::I32),
            }),
        ];
        let name = Identifier::Basic {
            mangling: Some(Mangling::C),
            name: String::from("main"),
            link_name: Some(String::from("__lccc_main")),
        };
        let sig = FunctionSignature {
            abi: Abi::Rust,
            params: Vec::new(),
            return_ty: Box::new(Type::Integer(IntType::I32)),
            safety: Safety::Safe,
            visibility: Visibility::Pub,
        };
        irgen_definition(
            &Definition::Function {
                name: name.clone(),
                return_ty: (*sig.return_ty).clone(),
                body,
            },
            &[Declaration::Function {
                has_definition: true,
                name,
                sig,
            }],
            file,
        );
    } else {
        panic!("main is not a function?");
    }
}

pub fn irgen(program: &Program, file: &mut ir::File) {
    for declaration in &program.declarations {
        if declaration.has_definition() {
            continue;
        }
        file.root.members.insert(
            identifier_to_path(
                declaration.name().clone(),
                match declaration {
                    Declaration::Function { sig, .. } => Some(sig),
                    Declaration::Local { .. } => {
                        unreachable!("Excuse me, why is there a local at global scope?")
                    }
                },
            ),
            ir::ScopeMember {
                vis: ir::Visibility::Public, // TODO: support other visibilities
                member_decl: match declaration {
                    Declaration::Function { sig, .. } => {
                        ir::MemberDeclaration::Function(ir::FunctionDeclaration {
                            ty: sig_to_fn_type(sig.clone()),
                            body: AbiNone,
                        })
                    }
                    Declaration::Local { .. } => {
                        unreachable!("Excuse me, why is there a local at global scope?")
                    }
                },
                ..ir::ScopeMember::default()
            },
        );
    }
    for definition in &program.definitions {
        irgen_definition(definition, &program.declarations, file);
    }
    if let Some(main) = program.lang_items.get(&LangItem::Main) {
        irgen_main(main, &program.declarations, file);
    }
}
