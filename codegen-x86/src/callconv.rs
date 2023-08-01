use std::{collections::HashSet, rc::Rc};

use arch_ops::x86::{features::X86Feature, X86Register, X86RegisterClass};

use xlang::{
    prelude::v1::{Pair, Some as XLangSome},
    targets::properties::TargetProperties,
};
use xlang_backend::{callconv::CallingConvention, ty::TypeInformation};
use xlang_struct::{
    AggregateDefinition, FnType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type,
};

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

#[allow(clippy::missing_panics_doc)] // TODO: remove todo!()
#[must_use]
pub fn classify_type(ty: &Type) -> Option<TypeClass> {
    match ty {
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    vectorsize: XLangSome(1..=65535),
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
        Type::Void | Type::FnType(_) | Type::Null => None,
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
        Type::Aggregate(AggregateDefinition { fields, .. }) => {
            let mut infected = TypeClass::Zero;
            for ty in fields.iter().map(|Pair(_, ty)| ty) {
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
        Type::Named(path) => todo!("named type {:?}", path),
    }
}

pub trait X86CallConv {
    fn prepare_stack(&self, ty: &FnType, frame_size: usize) -> usize;
    fn find_parameter(&self, off: u32, ty: &FnType, infn: bool) -> ValLocation;
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation>;
    fn pass_return_place(&self, ty: &Type, frame_size: usize) -> Option<ValLocation>;
    fn with_tag(&self, tag: &str) -> Option<Box<dyn X86CallConv>>;
    fn callee_saved(&self) -> &[X86Register];
}

#[derive(Clone, Debug)]
pub struct SysV64CC<S: std::hash::BuildHasher + Clone = std::collections::hash_map::RandomState>(
    &'static TargetProperties<'static>,
    HashSet<X86Feature, S>,
    Rc<TypeInformation>,
);

impl<S: std::hash::BuildHasher + Clone + 'static> X86CallConv for SysV64CC<S> {
    fn prepare_stack(&self, _ty: &FnType, _frame_size: usize) -> usize {
        todo!()
    }

    #[allow(clippy::no_effect_underscore_binding)] // TODO: use xmm_regs
    fn find_parameter(&self, off: u32, ty: &FnType, _: bool) -> ValLocation {
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
            match (classify_type(ty).unwrap(), self.2.type_size(ty).unwrap()) {
                (_, 0) => last_val = ValLocation::Null,
                (TypeClass::Integer, 1) => {
                    int_regs.get(0).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[1..];
                            let reg =
                                X86Register::from_class(X86RegisterClass::ByteRex, reg.regnum())
                                    .unwrap();
                            last_val = ValLocation::Register(reg);
                        },
                    );
                }
                (TypeClass::Integer, 2) => {
                    int_regs.get(0).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[1..];
                            let reg = X86Register::from_class(X86RegisterClass::Word, reg.regnum())
                                .unwrap();
                            last_val = ValLocation::Register(reg);
                        },
                    );
                }
                (TypeClass::Integer, 3 | 4) => {
                    int_regs.get(0).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[1..];
                            let reg =
                                X86Register::from_class(X86RegisterClass::Double, reg.regnum())
                                    .unwrap();
                            last_val = ValLocation::Register(reg);
                        },
                    );
                }
                (TypeClass::Integer, 5..=8) => {
                    int_regs.get(0).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[1..];
                            last_val = ValLocation::Register(*reg);
                        },
                    );
                }
                (TypeClass::Integer, 9..=16) => {
                    int_regs.get(0..2).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[2..];
                            last_val = ValLocation::Regs(reg.to_owned());
                        },
                    );
                }
                (TypeClass::Integer, 17..=24) => {
                    int_regs.get(0..3).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[3..];
                            last_val = ValLocation::Regs(reg.to_owned());
                        },
                    );
                }
                (TypeClass::Integer, 25..=32) => {
                    int_regs.get(0..4).map_or_else(
                        || todo!(),
                        |reg| {
                            int_regs = &int_regs[4..];
                            last_val = ValLocation::Regs(reg.to_owned());
                        },
                    );
                }
                _ => todo!(),
            }
        }

        last_val
    }

    #[allow(clippy::unnested_or_patterns)]
    fn find_return_val(&self, ty: &Type) -> Option<ValLocation> {
        match (classify_type(ty), self.2.type_size(ty)) {
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

    fn with_tag(&self, _: &str) -> Option<Box<dyn X86CallConv>> {
        Some(Box::new((*self).clone()))
    }

    fn callee_saved(&self) -> &[X86Register] {
        &[
            X86Register::Rbx,
            X86Register::Rbp,
            X86Register::Rsp, // note: This is hardcoded in the codegen
            X86Register::R12,
            X86Register::R13,
            X86Register::R14,
            X86Register::R15,
        ]
    }
}

impl<'a> CallingConvention for dyn X86CallConv + 'a {
    type Loc = ValLocation;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        self.pass_return_place(ty, 0)
    }

    fn find_param(&self, fnty: &FnType, _: &FnType, param: u32, infn: bool) -> Self::Loc {
        self.find_parameter(param, fnty, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        self.find_return_val(&fnty.ret).unwrap()
    }
}

#[allow(clippy::module_name_repetitions)]
pub fn get_callconv<S: std::hash::BuildHasher + Clone + 'static>(
    _tag: &str,
    target: &'static TargetProperties<'static>,
    features: HashSet<X86Feature, S>,
    tys: Rc<TypeInformation>,
) -> Option<Box<dyn X86CallConv>> {
    Some(Box::new(SysV64CC(target, features, tys)))
}
