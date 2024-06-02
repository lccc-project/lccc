use arch_ops::x86::{X86Mode, X86Register, X86RegisterClass};
use xlang_backend::callconv::{
    CallConvInfo, ClassifyAggregateDisposition, ParamPosition, RegisterDisposition,
    ReturnPointerBehaviour, StackedParamsOrder, Tag,
};

use xlang::abi::option::Some as XLangSome;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86TypeClass {
    NoClass,
    X87,
    X87Up,
    Sse,
    SseUp,
    SseWide(X86RegisterClass),
    Float,
    Integer,
    Memory,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86Tag {
    SysV64,
}

impl X86Tag {
    pub fn volatile_regs(&self) -> &'static [X86Register] {
        match self {
            X86Tag::SysV64 => &[
                X86Register::Rax,
                X86Register::Rcx,
                X86Register::Rdx,
                X86Register::Rsi,
                X86Register::Rdi,
                X86Register::R8,
                X86Register::R9,
            ],
        }
    }
}

impl Tag for X86Tag {
    type Register = X86Register;
    type TypeClass = X86TypeClass;

    fn tag_name(&self) -> &'static str {
        match self {
            Self::SysV64 => "SysV64",
        }
    }

    fn param_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register] {
        match (self, cl) {
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Tmm)) => &[],
            (X86Tag::SysV64, X86TypeClass::Sse)
            | (X86Tag::SysV64, X86TypeClass::SseUp)
            | (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Xmm))
            | (X86Tag::SysV64, X86TypeClass::Float) => &[
                X86Register::Xmm(0),
                X86Register::Xmm(1),
                X86Register::Xmm(2),
                X86Register::Xmm(3),
                X86Register::Xmm(4),
                X86Register::Xmm(5),
            ],
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Ymm)) => &[
                X86Register::Ymm(0),
                X86Register::Ymm(1),
                X86Register::Ymm(2),
                X86Register::Ymm(3),
                X86Register::Ymm(4),
                X86Register::Ymm(5),
            ],
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Zmm)) => &[
                X86Register::Zmm(0),
                X86Register::Zmm(1),
                X86Register::Zmm(2),
                X86Register::Zmm(3),
                X86Register::Zmm(4),
                X86Register::Zmm(5),
            ],
            (X86Tag::SysV64, X86TypeClass::Integer) => &[
                X86Register::Rdi,
                X86Register::Rsi,
                X86Register::Rdx,
                X86Register::Rcx,
                X86Register::R8,
                X86Register::R9,
            ],
            (X86Tag::SysV64, _) => &[],
        }
    }

    fn return_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register] {
        match (self, cl) {
            (X86Tag::SysV64, X86TypeClass::X87) => &[X86Register::Fp(0), X86Register::Fp(1)],
            (X86Tag::SysV64, X86TypeClass::Sse)
            | (X86Tag::SysV64, X86TypeClass::SseUp)
            | (X86Tag::SysV64, X86TypeClass::Float) => &[X86Register::Xmm(0), X86Register::Xmm(1)],
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Tmm)) => &[],
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Xmm)) => {
                &[X86Register::Xmm(0)]
            }
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Ymm)) => {
                &[X86Register::Ymm(0)]
            }
            (X86Tag::SysV64, X86TypeClass::SseWide(X86RegisterClass::Zmm)) => {
                &[X86Register::Zmm(0)]
            }
            (X86Tag::SysV64, X86TypeClass::Integer) => &[X86Register::Rax, X86Register::Rdx],
            (X86Tag::SysV64, _) => &[],
        }
    }

    fn replace_param_with_pointer(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass> {
        eprintln!("replace_param_with_pointer({:?})", cl);
        match self {
            Self::SysV64 => {
                if cl.len() > 2 {
                    Some(X86TypeClass::Integer)
                } else if cl.iter().any(|x| {
                    *x == X86TypeClass::Memory
                        || *x == X86TypeClass::X87
                        || *x == X86TypeClass::X87Up
                }) {
                    Some(X86TypeClass::Integer)
                } else {
                    None
                }
            }
        }
    }

    fn combine_wide(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass> {
        match cl {
            [X86TypeClass::Sse, X86TypeClass::SseUp] => {
                Some(X86TypeClass::SseWide(X86RegisterClass::Xmm))
            }
            [X86TypeClass::Sse, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp] => {
                Some(X86TypeClass::SseWide(X86RegisterClass::Ymm))
            }
            [X86TypeClass::Sse, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp, X86TypeClass::SseUp] => {
                Some(X86TypeClass::SseWide(X86RegisterClass::Zmm))
            }
            [X86TypeClass::X87, X86TypeClass::X87Up]
            | [X86TypeClass::X87, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up]
            | [X86TypeClass::X87, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up, X86TypeClass::X87Up] => {
                Some(X86TypeClass::X87)
            }
            _ => None,
        }
    }

    fn replace_return_with_pointer(
        &self,
        cl: &[Self::TypeClass],
    ) -> Option<ReturnPointerBehaviour<Self::Register, Self::TypeClass>> {
        match self {
            Self::SysV64 => {
                if cl.len() > 2 {
                    Some(ReturnPointerBehaviour::Param(
                        ParamPosition::First,
                        X86TypeClass::Integer,
                    ))
                } else if cl.iter().any(|x| *x == X86TypeClass::Memory) {
                    Some(ReturnPointerBehaviour::Param(
                        ParamPosition::First,
                        X86TypeClass::Integer,
                    ))
                } else {
                    None
                }
            }
        }
    }

    fn replace_class_as_varargs(&self, _: &Self::TypeClass) -> Option<Self::TypeClass> {
        None
    }

    fn register_disposition(&self, cl: &Self::TypeClass) -> RegisterDisposition {
        match self {
            X86Tag::SysV64 => match cl {
                X86TypeClass::Sse
                | X86TypeClass::SseUp
                | X86TypeClass::SseWide(_)
                | X86TypeClass::Float => RegisterDisposition::Consume,
                _ => RegisterDisposition::Interleave,
            },
        }
    }

    fn stacked_params_order(&self) -> StackedParamsOrder {
        match self {
            X86Tag::SysV64 => StackedParamsOrder::Rtl,
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct X86CallConvInfo {
    pub mode: X86Mode,
}

impl CallConvInfo for X86CallConvInfo {
    type Tag = X86Tag;

    type TypeClass = X86TypeClass;

    type Register = X86Register;

    fn get_tag(&self, tag: &str) -> Self::Tag {
        match (self.mode, tag) {
            (X86Mode::Long, "SysV64") => X86Tag::SysV64,
            (mode, tag) => todo!("{} in {:?}", tag, mode),
        }
    }

    fn no_class(&self) -> Self::TypeClass {
        X86TypeClass::NoClass
    }

    fn classify_scalar(&self, sty: xlang_struct::ScalarType) -> Vec<Self::TypeClass> {
        let width = self.mode.width();
        let scalar_regs = ((sty.header.bitsize + (width - 1)) as usize) >> (width.trailing_zeros());

        if let XLangSome(elements) = sty.header.vectorsize {
            let total_regs = (scalar_regs) * (elements as usize);
            if total_regs > 0 {
                let mut regs = Vec::with_capacity(total_regs);
                regs.push(X86TypeClass::Sse);
                for _ in 1..total_regs {
                    regs.push(X86TypeClass::SseUp)
                }
                regs
            } else {
                vec![X86TypeClass::NoClass]
            }
        } else {
            if scalar_regs == 0 {
                vec![X86TypeClass::NoClass]
            } else {
                match sty.kind {
                    xlang_struct::ScalarTypeKind::Empty => vec![X86TypeClass::NoClass; scalar_regs],
                    xlang_struct::ScalarTypeKind::Integer { .. }
                    | xlang_struct::ScalarTypeKind::Fixed { .. }
                    | xlang_struct::ScalarTypeKind::Char { .. } => {
                        vec![X86TypeClass::Integer; scalar_regs]
                    }
                    xlang_struct::ScalarTypeKind::Float {
                        format: xlang::ir::FloatFormat::IeeeExtPrecision,
                    } if sty.header.bitsize < 80 => {
                        let mut regs = Vec::with_capacity(scalar_regs);
                        regs.push(X86TypeClass::X87);
                        for _ in 1..scalar_regs {
                            regs.push(X86TypeClass::X87Up)
                        }
                        regs
                    }
                    xlang_struct::ScalarTypeKind::Float { .. } => {
                        vec![X86TypeClass::Float; scalar_regs]
                    }
                    xlang_struct::ScalarTypeKind::Posit => vec![X86TypeClass::Float; scalar_regs],
                }
            }
        }
    }

    fn classify_pointer(&self, _: xlang::ir::PointerKind) -> Self::TypeClass {
        X86TypeClass::Integer
    }

    fn classify_aggregate_disposition(
        &self,
    ) -> xlang_backend::callconv::ClassifyAggregateDisposition<Self::TypeClass> {
        ClassifyAggregateDisposition::SplitFlat((self.mode.width() as u64) >> 3)
    }

    fn merge_class(&self, left: Self::TypeClass, right: Self::TypeClass) -> Self::TypeClass {
        if left == right {
            left
        } else {
            match (left, right) {
                (X86TypeClass::NoClass, other) | (other, X86TypeClass::NoClass) => other,
                (X86TypeClass::Memory, _) | (_, X86TypeClass::Memory) => X86TypeClass::Memory,
                (X86TypeClass::Sse, X86TypeClass::SseUp)
                | (X86TypeClass::SseUp, X86TypeClass::Sse) => X86TypeClass::Sse,
                (X86TypeClass::Integer, _) | (_, X86TypeClass::Integer) => X86TypeClass::Integer,
                _ => X86TypeClass::Memory,
            }
        }
    }

    fn adjust_classes_after_combine(&self, _: &mut [Self::TypeClass]) {}
}
