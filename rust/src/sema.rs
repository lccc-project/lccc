use crate::parse::{FnParam, Item};
pub use crate::parse::{Safety, Visibility};

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Identifier {
    Basic(String),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    I128,
    Iptr,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Uptr,
    Usize,
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mutability {
    Const,
    Mut,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Character,
    Float(FloatType),
    Integer(IntType),
    Pointer(Box<Self>),
    Str,
    Tuple(Vec<Self>),
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Abi {
    C,              // "C"
    CUnwind,        // "C-unwind"
    Fastcall,       // "fastcall"
    FastcallUnwind, // "fastcall-unwind"
    Rust,           // "Rust"
    RustCall,       // "rust-call"
    RustcallV0,     // "rustcall-v0", not to be confused with "rust-call"
    RustIntrinsic,  // "rust-intrinsic"
    Stdcall,        // "stdcall"
    StdcallUnwind,  // "stdcall-unwind"
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mangling {
    C,
    Rust,
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct FunctionSignature {
    abi: Abi,
    mangling: Mangling,
    params: Vec<Type>,
    return_ty: Type,
    safety: Safety,
    visibility: Visibility,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Declaration {
    Function {
        has_definition: bool,
        name: Identifier,
        sig: FunctionSignature,
    },
}

#[derive(Clone, Debug, Hash)]
pub struct Program {
    named_types: Vec<Type>,
    declarations: Vec<Declaration>,
}

fn iter_in_scope<F: FnMut(&Item, Option<&str>)>(items: &[Item], _abi: Option<&str>, _func: F) {
    for item in items {
        todo!("{:?}", item);
    }
}

pub fn convert_ty(_named_types: &[Type], _orig: &crate::parse::Type) -> Type {
    todo!()
}

pub fn convert(items: &[Item]) -> Program {
    // Pass 1: list types
    // Since we don't have a prelude to give us standard types yet, we list them
    // inline.
    let named_types = vec![
        Type::Boolean,
        Type::Character,
        Type::Float(FloatType::F32),
        Type::Float(FloatType::F64),
        Type::Integer(IntType::I8),
        Type::Integer(IntType::I16),
        Type::Integer(IntType::I32),
        Type::Integer(IntType::I64),
        Type::Integer(IntType::I128),
        Type::Integer(IntType::Iptr),
        Type::Integer(IntType::Isize),
        Type::Integer(IntType::U8),
        Type::Integer(IntType::U16),
        Type::Integer(IntType::U32),
        Type::Integer(IntType::U64),
        Type::Integer(IntType::U128),
        Type::Integer(IntType::Uptr),
        Type::Integer(IntType::Usize),
        Type::Str,
    ];
    // Pass 2: list declarations
    let mut declarations = Vec::new();
    iter_in_scope(items, None, |item, abi| match item {
        Item::FnDeclaration {
            visibility,
            safety,
            name,
            params,
            return_ty,
            block,
        } => {
            declarations.push(Declaration::Function {
                has_definition: block.is_some(),
                name: Identifier::Basic(name.clone()),
                sig: FunctionSignature {
                    abi: abi.map_or(Abi::Rust, |x| match x {
                        "C" => Abi::C,
                        "Rust" => Abi::Rust,
                        _ => todo!(),
                    }),
                    mangling: if abi.is_some() {
                        Mangling::Rust
                    } else {
                        Mangling::C
                    },
                    params: params
                        .iter()
                        .map(|FnParam { ty, .. }| convert_ty(&named_types, ty))
                        .collect(),
                    return_ty: return_ty.as_ref().map_or_else(
                        || Type::Tuple(Vec::new()),
                        |ty| convert_ty(&named_types, ty),
                    ),
                    safety: *safety,
                    visibility: *visibility,
                },
            });
        }
        Item::ExternBlock { .. } => {
            unreachable!("Should have been descended into by calling function");
        }
    });
    Program {
        named_types,
        declarations,
    }
}
