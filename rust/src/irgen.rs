#![allow(clippy::module_name_repetitions)]

use crate::sema::{
    Abi, Declaration, Definition, Expression, FunctionSignature, Identifier, IntType, LangItem,
    Mangling, Mutability, Program, Safety, Statement, Type, Visibility,
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
        todo!()
    }
    result.push(')');
    if !signature.return_ty.is_unit() {
        todo!()
    }
    ir::PathComponent::SpecialComponent(result)
}

#[allow(clippy::single_match_else)]
fn identifier_to_path(id: Identifier, signature: Option<&FunctionSignature>) -> ir::Path {
    let Identifier::Basic { mangling, name } = id;
    match mangling {
        Some(Mangling::C) => ir::Path {
            components: abi::vec![ir::PathComponent::Text(abi::string::String::from(&name))],
        },
        _ => {
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
                    _ => todo!(),
                },
                ..ir::ScalarTypeHeader::default()
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: ty.is_signed(),
                min: i128::MIN,
                max: i128::MAX,
            },
        }),
        Type::Pointer {
            mutability,
            underlying,
        } => ir::Type::Pointer(ir::PointerType {
            alias: ir::PointerAliasingRule::default(),
            valid_range: abi::pair::Pair::default(),
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
            decl: match mutability {
                Mutability::Const => {
                    ir::PointerDeclarationType::CONST | ir::PointerDeclarationType::REF
                }
                Mutability::Mut => ir::PointerDeclarationType::REF,
            },
            inner: abi::boxed::Box::new(irgen_type((*underlying).clone())),
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
                min: 0,
                max: 0,
            },
        }),
        x => todo!("{:?}", x),
    }
}

fn irgen_expr(expr: Expression, n: u32) -> Vec<ir::BlockItem> {
    match expr {
        Expression::Cast { expr, target } => {
            let mut result = irgen_expr(*expr, n);
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
            result.append(&mut irgen_expr(*func, n));
            for arg in args {
                result.append(&mut irgen_expr(arg, n));
            }
            result.push(ir::BlockItem::Expr(ir::Expr::CallFunction(fn_type)));
            result
        }
        Expression::Identifier { id, ty: Some(ty) } => {
            vec![ir::BlockItem::Expr(ir::Expr::Const(
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
            ))]
        }
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
        Expression::UnsafeBlock {
            block,
            ty: Some(ty),
        } => {
            let mut result = vec![ir::BlockItem::Expr(ir::Expr::Block {
                n: n + 1,
                block: irgen_block(block, n + 1),
            })];
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

fn irgen_block(block: Vec<Statement>, n: u32) -> ir::Block {
    let mut result = Vec::new();
    let mut has_expr = false;
    for statement in block {
        match statement {
            Statement::Bind { .. } => todo!(),
            Statement::Discard(expr) => {
                has_expr = false;
                result.append(&mut irgen_expr(expr, n));
                result.push(ir::BlockItem::Expr(ir::Expr::Pop(1)));
            }
            Statement::Expression(expr) => {
                has_expr = true;
                result.append(&mut irgen_expr(expr, n));
            }
        }
    }
    if has_expr {
        result.push(ir::BlockItem::Expr(ir::Expr::ExitBlock {
            blk: n,
            values: 1,
        }));
    } else {
        result.push(ir::BlockItem::Expr(ir::Expr::ExitBlock {
            blk: n,
            values: 0,
        }));
    }
    ir::Block {
        items: result.into(),
    }
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
    }
}

pub fn irgen_definition(
    definition: &Definition,
    declarations: &[Declaration],
    file: &mut ir::File,
) {
    let Definition::Function { name, body, .. } = definition;
    let Declaration::Function { sig, .. } = declarations
        .iter()
        .find(|x| x.name().matches(name))
        .unwrap();
    file.root.members.insert(
        identifier_to_path(name.clone(), Some(sig)),
        ir::ScopeMember {
            vis: ir::Visibility::Public,
            member_decl: ir::MemberDeclaration::Function(ir::FunctionDeclaration {
                ty: sig_to_fn_type(sig.clone()),
                body: AbiSome(irgen_block(body.clone(), 0)),
            }),
            ..ir::ScopeMember::default()
        },
    );
}

pub fn irgen_main(main: &Identifier, declarations: &[Declaration], file: &mut ir::File) {
    let Declaration::Function { sig: main_sig, .. } = declarations
        .iter()
        .find(|x| x.name().matches(main))
        .unwrap();
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
        name: String::from("__lccc_main"),
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
