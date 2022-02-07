use std::collections::HashSet;

use arch_ops::x86::{features::X86Feature, X86Register, X86RegisterClass};
use target_tuples::Target;
use xlang::targets::properties::TargetProperties;
use xlang_struct::{Abi, FnType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type};

use xlang_backend::ty::type_size;

use crate::ValLocation;

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeClass {
    Float,
    X87,
    Sse,
    Integer,
    Memory,
    Zero,
}

pub fn classify_type(ty: &Type) -> Option<TypeClass> {
    match ty {
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    vectorsize: 1..=65535,
                    ..
                },
            ..
        }) => Some(TypeClass::Sse),
        Type::Scalar(ScalarType {
            header: ScalarTypeHeader { bitsize: 80, .. },
            kind: ScalarTypeKind::LongFloat { .. },
            ..
        }) => Some(TypeClass::X87),
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::Float),
        Type::Scalar(_) | Type::Pointer(_) => Some(TypeClass::Integer),
        Type::Void | Type::FnType(_) => None,
        Type::Null => None,
        Type::Array(ty) => classify_type(&ty.ty),
        Type::TaggedType(_, ty) => classify_type(ty),
        Type::Product(tys) => {
            let mut infected = TypeClass::Zero;
            for ty in tys {
                infected = match (classify_type(ty)?, infected) {
                    (a, TypeClass::Zero) => a,
                    (_, TypeClass::Memory) => TypeClass::Memory,
                    (TypeClass::Float, TypeClass::Sse) => TypeClass::Sse,
                    (TypeClass::Float, TypeClass::X87) => TypeClass::X87,
                    (a, b) if a == b => a,
                    _ => TypeClass::Memory,
                };
            }
            Some(infected)
        }
        Type::Aligned(_, _) => todo!(),
    }
}

pub trait X86CallConv {
    fn prepare_stack(&self, ty: &FnType, frame_size: usize) -> usize;
    fn find_parameter(&self, off: u32, ty: &FnType) -> ValLocation;
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation>;
    fn pass_return_place(&self, ty: &Type, frame_size: usize) -> Option<ValLocation>;
    fn with_tag(&self, tag: Abi) -> Option<Box<dyn X86CallConv>>;
}

#[derive(Clone, Debug)]
pub struct SysV64CC(&'static TargetProperties, HashSet<X86Feature>);

impl X86CallConv for SysV64CC {
    fn prepare_stack(&self, _ty: &FnType, _frame_size: usize) -> usize {
        todo!()
    }

    fn find_parameter(&self, off: u32, ty: &FnType) -> ValLocation {
        let mut int_regs: &[X86Register] = &[
            X86Register::Rdi,
            X86Register::Rsi,
            X86Register::Rdx,
            X86Register::Rcx,
            X86Register::R8,
            X86Register::R9,
        ];
        let mut _xmm_regs: &[X86Register] = &[
            X86Register::Xmm(0),
            X86Register::Xmm(1),
            X86Register::Xmm(2),
            X86Register::Xmm(3),
            X86Register::Xmm(4),
            X86Register::Xmm(5),
        ];

        let has_return_param = self.pass_return_place(&ty.ret, 0).is_some();
        if has_return_param {
            int_regs = &int_regs[1..];
        }
        let mut last_val = ValLocation::Unassigned(0);
        for ty in ty.params.iter().take(off as usize + 1) {
            match (classify_type(ty).unwrap(), type_size(ty, self.0).unwrap()) {
                (TypeClass::Zero, 0) => last_val = ValLocation::Null,
                (TypeClass::Integer, 0) => last_val = ValLocation::Null,
                (TypeClass::Integer, 1) => {
                    if let Some(reg) = int_regs.get(0) {
                        int_regs = &int_regs[1..];
                        let reg = X86Register::from_class(X86RegisterClass::ByteRex, reg.regnum())
                            .unwrap();
                        last_val = ValLocation::Register(reg);
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 2) => {
                    if let Some(reg) = int_regs.get(0) {
                        int_regs = &int_regs[1..];
                        let reg =
                            X86Register::from_class(X86RegisterClass::Word, reg.regnum()).unwrap();
                        last_val = ValLocation::Register(reg);
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 4) | (TypeClass::Integer, 3) => {
                    if let Some(reg) = int_regs.get(0) {
                        int_regs = &int_regs[1..];
                        let reg = X86Register::from_class(X86RegisterClass::Double, reg.regnum())
                            .unwrap();
                        last_val = ValLocation::Register(reg);
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 5..=8) => {
                    if let Some(reg) = int_regs.get(0) {
                        int_regs = &int_regs[1..];
                        last_val = ValLocation::Register(*reg);
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 9..=16) => {
                    if let Some(reg) = int_regs.get(0..2) {
                        int_regs = &int_regs[2..];
                        last_val = ValLocation::Regs(reg.to_owned());
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 17..=24) => {
                    if let Some(reg) = int_regs.get(0..3) {
                        int_regs = &int_regs[3..];
                        last_val = ValLocation::Regs(reg.to_owned());
                    } else {
                        todo!()
                    }
                }
                (TypeClass::Integer, 25..=32) => {
                    if let Some(reg) = int_regs.get(0..4) {
                        int_regs = &int_regs[4..];
                        last_val = ValLocation::Regs(reg.to_owned());
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            }
        }

        last_val
    }

    #[allow(clippy::unnested_or_patterns)]
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation> {
        match (classify_type(ty), type_size(ty, self.0)) {
            (None, None) => None,
            (None, Some(_)) | (Some(_), None) => unreachable!(),
            (Some(TypeClass::Zero), Some(0)) => Some(ValLocation::Null),
            (Some(TypeClass::Zero), Some(_)) => {
                panic!("Impossible situation (type has zst class, but has a size)")
            }
            (Some(TypeClass::Integer), Some(1)) => Some(ValLocation::Register(X86Register::Al)),
            (Some(TypeClass::Integer), Some(2)) => Some(ValLocation::Register(X86Register::Ax)),
            (Some(TypeClass::Integer), Some(4)) => Some(ValLocation::Register(X86Register::Eax)),
            (Some(TypeClass::Integer), Some(8)) => Some(ValLocation::Register(X86Register::Rax)),
            (Some(TypeClass::Integer), Some(16)) => {
                Some(ValLocation::Regs(vec![X86Register::Rax, X86Register::Rdx]))
            }
            (Some(TypeClass::X87), Some(16)) => Some(ValLocation::Register(X86Register::Fp(0))),
            (Some(TypeClass::Float), Some(4))
            | (Some(TypeClass::Float), Some(8))
            | (Some(TypeClass::Float), Some(16)) => {
                Some(ValLocation::Register(X86Register::Xmm(0)))
            }
            (Some(TypeClass::Sse), Some(4))
            | (Some(TypeClass::Sse), Some(8))
            | (Some(TypeClass::Sse), Some(16)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::Sse), Some(32)) if self.1.contains(&X86Feature::Avx) => {
                Some(ValLocation::Register(X86Register::Ymm(0)))
            }
            (Some(TypeClass::Sse), Some(64)) if self.1.contains(&X86Feature::Avx512f) => {
                Some(ValLocation::Register(X86Register::Zmm(0)))
            }
            _ => None,
        }
    }

    fn pass_return_place(&self, _ty: &Type, _frame_size: usize) -> Option<ValLocation> {
        None // For now
    }

    fn with_tag(&self, _: Abi) -> Option<Box<dyn X86CallConv>> {
        Some(Box::new((*self).clone()))
    }
}

pub fn get_callconv(
    _tag: Abi,
    target: Target,
    features: HashSet<X86Feature>,
) -> Option<Box<dyn X86CallConv>> {
    target_tuples::match_targets! {
        match (target){
            x86_64-*-linux-gnu => Some(Box::new(SysV64CC(xlang::targets::properties::get_properties(target.into())?,features))),
            x86_64-*-linux-gnux32 => Some(Box::new(SysV64CC(xlang::targets::properties::get_properties(target.into())?,features)))
        }
    }
}
