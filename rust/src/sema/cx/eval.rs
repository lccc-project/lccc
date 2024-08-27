use crate::{ast::Mutability, sema::mir::RefKind};

use super::ConstExpr;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct AllocId(u32);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CxEvalValue {
    Const(ConstExpr),
    LocalAddr(RefKind, Mutability, AllocId),
}

pub struct MirEvaluator {}
