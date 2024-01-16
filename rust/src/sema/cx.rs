use super::{hir::HirExpr, ty::IntType, DefId};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstExpr {
    #[allow(dead_code)]
    HirVal(Box<HirExpr>),
    IntConst(IntType, u128),
    Const(DefId),
}

impl core::fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ConstExpr::HirVal(val) => f.write_fmt(format_args!("const {{ {} }}", val)),
            ConstExpr::IntConst(ity, val) => f.write_fmt(format_args!("{}_{}", val, ity)),
            ConstExpr::Const(def) => def.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstEvalError {
    EvaluatorError,
}
