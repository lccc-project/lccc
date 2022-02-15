use crate::sema::{
    Declaration, Definition, Expression, FunctionSignature, Identifier, IntType, Mutability,
    Program, Statement, Type,
};

use std::convert::TryInto; // TODO: Remove when we move to edition 2021

use xlang::{
    abi::{
        self,
        option::Option::{None as AbiNone, Some as AbiSome},
    },
    ir,
};

fn identifier_to_path(id: Identifier) -> ir::Path {
    let Identifier::Basic(id) = id;
    ir::Path {
        components: abi::vec![ir::PathComponent::Text(abi::string::String::from(&id))],
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
                    ty: irgen_type(ty),
                    item: identifier_to_path(id),
                },
            ))]
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
    for statement in block {
        match statement {
            Statement::Bind { .. } => todo!(),
            Statement::Discard(expr) => {
                result.append(&mut irgen_expr(expr, n));
                result.push(ir::BlockItem::Expr(ir::Expr::Pop(1)));
            }
            Statement::Expression(expr) => result.append(&mut irgen_expr(expr, n)),
        }
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

pub fn irgen(program: &Program, file: &mut ir::File) {
    for declaration in &program.declarations {
        if declaration.has_definition() {
            continue;
        }
        file.root.members.insert(
            identifier_to_path(declaration.name().clone()),
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
        let Definition::Function { name, body, .. } = definition;
        let Declaration::Function { sig, .. } = program
            .declarations
            .iter()
            .find(|x| x.name() == name)
            .unwrap();
        file.root.members.insert(
            identifier_to_path(name.clone()),
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
}
