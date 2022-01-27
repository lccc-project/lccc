use crate::parse::{FnParam, Item};
pub use crate::parse::{Mutability, Safety, Visibility};

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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Char,
    Float(FloatType),
    Integer(IntType),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>
    },
    Str,
    Tuple(Vec<Self>),
}

impl Type {
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::Boolean => String::from("bool"),
            Self::Char => String::from("char"),
            Self::Float(ty) => String::from(match ty {
                FloatType::F32 => "f32",
                FloatType::F64 => "f64",
            }),
            Self::Integer(ty) => String::from(match ty {
                IntType::I8 => "i8",
                IntType::I16 => "i16",
                IntType::I32 => "i32",
                IntType::I64 => "i64",
                IntType::I128 => "i128",
                IntType::Iptr => "iptr",
                IntType::Isize => "isize",
                IntType::U8 => "u8",
                IntType::U16 => "u16",
                IntType::U32 => "u32",
                IntType::U64 => "u64",
                IntType::U128 => "u128",
                IntType::Uptr => "uptr",
                IntType::Usize => "usize",
            }),
            _ => todo!("{:?}", self),
        }
    }
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

pub fn convert_ty(named_types: &[Type], orig: &crate::parse::Type) -> Type {
    match orig {
        crate::parse::Type::Name(name) => {
            for ty in named_types {
                if *name == ty.name() {
                    return ty.clone();
                }
            }
            panic!("Unknown type {}", name);
        }
        crate::parse::Type::Pointer { mutability, underlying } => Type::Pointer { mutability: *mutability, underlying: Box::new(convert_ty(named_types, &**underlying)) },
    }
}

fn iter_in_scope<F: FnMut(&Item, Option<&str>)>(items: &[Item], abi: Option<&str>, func: &mut F) {
    for item in items {
        match item {
            Item::ExternBlock { abi: new_abi, items } => {
                assert!(abi.is_none(), "`extern` blocks can't be nested");
                iter_in_scope(items, Some(new_abi.as_deref().unwrap_or("C")), func);
            }
            _ => func(item, abi),
        }
    }
}

pub fn convert(items: &[Item]) -> Program {
    // Pass 1: list types
    // Since we don't have a prelude to give us standard types yet, we list them
    // inline.
    let named_types = vec![
        Type::Boolean,
        Type::Char,
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
    iter_in_scope(items, None, &mut |item, abi| match item {
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
