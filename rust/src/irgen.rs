use crate::sema::{
    Declaration, Definition, FunctionSignature, Identifier, IntType, Mutability, Program,
    Statement, Type,
};

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
        Type::Integer(ty) => ir::Type::Scalar(ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: match ty {
                    IntType::I8 | IntType::U8 => 8,
                    IntType::I16 | IntType::U16 => 16,
                    IntType::I32 | IntType::U32 => 32,
                    IntType::I64 | IntType::U64 => 64,
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
            inner: abi::boxed::Box::new(irgen_type((&*underlying).clone())),
        }),
        x => todo!("{:?}", x),
    }
}

fn irgen_block(block: Vec<Statement>) -> ir::Block {
    let result = abi::vec::Vec::new();
    for statement in block {
        todo!("{:?}", statement);
    }
    ir::Block { items: result }
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
            .filter(|x| x.name() == name)
            .next()
            .unwrap();
        file.root.members.insert(
            identifier_to_path(name.clone()),
            ir::ScopeMember {
                vis: ir::Visibility::Public,
                member_decl: ir::MemberDeclaration::Function(ir::FunctionDeclaration {
                    ty: sig_to_fn_type(sig.clone()),
                    body: AbiSome(irgen_block(body.clone())),
                }),
                ..ir::ScopeMember::default()
            },
        );
    }
    println!("{:#?}", file);
}
