use crate::ast::Mutability;

use super::{ty::Type, Spanned};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct RegionId(u32);

impl core::fmt::Display for RegionId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("'{}", self.0))
    }
}

impl core::fmt::Debug for RegionId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("'{}", self.0))
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct BasicBlockId(u32);

impl core::fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("@{}", self.0))
    }
}

impl core::fmt::Debug for BasicBlockId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("@{}", self.0))
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SsaVarId(u32);

impl core::fmt::Display for SsaVarId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("_{}", self.0))
    }
}

impl core::fmt::Debug for SsaVarId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("_{}", self.0))
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirExpr {
    Unreachable,
    Var(SsaVarId),
    Read(Box<Spanned<MirExpr>>),
    Alloca(Mutability, Type, Box<Spanned<MirExpr>>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirStatement {
    Write(Spanned<MirExpr>, Spanned<MirExpr>),
    Declare {
        var: Spanned<SsaVarId>,
        ty: Spanned<Type>,
        init: Spanned<MirExpr>,
    },
    StoreDead(SsaVarId),
    EndRegion(RegionId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirTerminator {
    Call(MirCallInfo),
    Tailcall(MirTailcallInfo),
    Return(Spanned<MirExpr>),
    Jump(MirJumpInfo),
    Unreachable,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirCallInfo {
    pub targ: Spanned<MirExpr>,
    pub retplace: Spanned<SsaVarId>, // id of the return place, which is made live in the next
    pub next: MirJumpInfo,
    pub unwind: Option<MirJumpInfo>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirTailcallInfo {
    pub targ: Spanned<MirExpr>,
    pub unwind: Option<MirJumpInfo>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirJumpInfo {
    pub targbb: BasicBlockId,
    pub remaps: Vec<(SsaVarId, SsaVarId)>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirBasicBlock {
    pub id: BasicBlockId,
    pub stats: Vec<Spanned<MirStatement>>,
    pub term: Spanned<MirTerminator>,
}
