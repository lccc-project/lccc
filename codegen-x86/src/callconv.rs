use std::collections::HashSet;

use arch_ops::x86::{features::X86Feature, X86Register};
use xlang::targets::properties::TargetProperties;
use xlang_struct::{FnType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type};

use crate::{get_type_size, ValLocation};

#[allow(dead_code)]
pub enum TypeClass {
    Float,
    X87,
    Sse,
    Integer,
    Memory,
    Zero,
}

pub const fn classify_type(ty: &Type) -> Option<TypeClass> {
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
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::X87),
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::Float),
        Type::Scalar(_) | Type::Pointer(_) => Some(TypeClass::Integer),
        Type::Void | Type::FnType(_) => None,
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

    fn find_parameter(&self, off: u32, ty: &FnType) -> ValLocation {
        let mut int_regs: &[X86Register] = &[
            X86Register::Rdi,
            X86Register::Rsi,
            X86Register::Rdx,
            X86Register::Rcx,
            X86Register::R8,
            X86Register::R9,
        ];
        let mut xmm_regs: &[X86Register] = &[
            X86Register::Xmm(0),
            X86Register::Xmm(1),
            X86Register::Xmm(2),
            X86Register::Xmm(3),
            X86Register::Xmm(4),
            X86Register::Xmm(5),
        ];

        let has_return_param = self.pass_return_place(&ty.ret, 0).is_some();
        let param = &ty.params[off as usize];

        match (
            classify_type(param).unwrap(),
            get_type_size(param, self.0).unwrap(),
        ) {
            (_, 0) => ValLocation::Null,
        }
    }

    #[allow(clippy::unnested_or_patterns)]
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation> {
        match (classify_type(ty), get_type_size(ty, self.0)) {
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
        todo!()
    }
}
