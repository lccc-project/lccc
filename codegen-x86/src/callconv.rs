use std::collections::HashSet;

use arch_ops::x86::{features::X86Feature, X86Register};
use xlang::targets::properties::TargetProperties;
use xlang_struct::{FnType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type};

use crate::{get_type_size, ValLocation};

#[allow(dead_code)]
pub enum TypeClass {
    Float,
    X87,
    SSE,
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
        }) => Some(TypeClass::SSE),
        Type::Scalar(ScalarType {
            header: ScalarTypeHeader { bitsize: 80, .. },
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::X87),
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::Float),
        Type::Scalar(_) => Some(TypeClass::Integer),
        Type::Void => None,
        Type::FnType(_) => None,
        Type::Pointer(_) => Some(TypeClass::Integer),
    }
}

pub trait X86CallConv {
    fn prepare_stack(&self, ty: &FnType, frame_size: usize) -> usize;
    fn find_parameter(&self, off: u32, ty: &FnType) -> ValLocation;
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation>;
    fn pass_return_place(&self, ty: &Type, frame_size: usize) -> Option<ValLocation>;
}

pub struct SysV64CC(&'static TargetProperties, HashSet<X86Feature>);

impl X86CallConv for SysV64CC {
    fn prepare_stack(&self, _ty: &FnType, _frame_size: usize) -> usize {
        todo!()
    }

    fn find_parameter(&self, mut _off: u32, _ty: &FnType) -> ValLocation {
        todo!()
    }

    fn find_return_val(&self, ty: &Type) -> Option<ValLocation> {
        match (classify_type(ty), get_type_size(ty, self.0)) {
            (None, None) => None,
            (None, Some(_)) => unreachable!(),
            (Some(_), None) => unreachable!(),
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
            (Some(TypeClass::Float), Some(4)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::Float), Some(8)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::Float), Some(16)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::SSE), Some(4)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::SSE), Some(8)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::SSE), Some(16)) => Some(ValLocation::Register(X86Register::Xmm(0))),
            (Some(TypeClass::SSE), Some(32)) if self.1.contains(&X86Feature::Avx) => {
                Some(ValLocation::Register(X86Register::Ymm(0)))
            }
            (Some(TypeClass::SSE), Some(64)) if self.1.contains(&X86Feature::Avx512f) => {
                Some(ValLocation::Register(X86Register::Zmm(0)))
            }
            _ => None,
        }
    }

    fn pass_return_place(&self, _ty: &Type, _frame_size: usize) -> Option<ValLocation> {
        todo!()
    }
}
