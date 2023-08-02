use super::{hir::HirExpr, ty::IntType};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstExpr {
    HirVal(Box<HirExpr>),
    IntConst(IntType, u128),
}

impl core::fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ConstExpr::HirVal(val) => f.write_fmt(format_args!("const {{ {} }}", val)),
            ConstExpr::IntConst(ity, val) => f.write_fmt(format_args!("{}_{}", val, ity)),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstEvalError {
    EvaluatorError,
}
