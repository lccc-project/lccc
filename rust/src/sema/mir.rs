use xlang::abi::collection::HashMap;

use crate::{
    ast::{Mutability, StringType},
    helpers::TabPrinter,
    interning::Symbol,
};

use super::{
    ty::{IntType, Type},
    DefId, Spanned, Definitions,
};

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

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum RefKind {
    Raw,
    Ref,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirExpr {
    Unreachable,
    Var(SsaVarId),
    Read(Box<Spanned<MirExpr>>),
    Alloca(Mutability, Type, Box<Spanned<MirExpr>>),
    ConstInt(IntType, Spanned<u128>),
    ConstString(StringType, Spanned<Symbol>),
    Const(DefId),
    Retag(RefKind, Mutability, Box<Spanned<MirExpr>>),
    Cast(Box<Spanned<MirExpr>>, Type),
}

impl core::fmt::Display for MirExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            MirExpr::Unreachable => f.write_str("unreachable"),
            MirExpr::Var(var) => var.fmt(f),
            MirExpr::Read(expr) => f.write_fmt(format_args!("read(*{})", expr.body)),
            MirExpr::Alloca(mt, ty, val) => {
                f.write_fmt(format_args!("alloca {} {} ({})", mt, ty, val.body))
            }
            MirExpr::ConstInt(ity, val) => f.write_fmt(format_args!("{}_{}", val.body, ity)),
            MirExpr::ConstString(_, val) => {
                f.write_fmt(format_args!("\"{}\"", val.escape_default()))
            }
            MirExpr::Const(defid) => defid.fmt(f),
            MirExpr::Retag(rk, mt, inner) => {
                f.write_str("&")?;
                if *rk == RefKind::Raw {
                    f.write_str("raw ")?
                }
                if *mt == Mutability::Mut {
                    f.write_str("mut ")?;
                }
                f.write_str("*")?;
                inner.body.fmt(f)
            }
            MirExpr::Cast(inner, ty) => f.write_fmt(format_args!("({}) as {}", inner.body, ty)),
        }
    }
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
    pub retplace: Spanned<SsaVarId>, // id of the return place, which is made live in the next
    pub targ: Spanned<MirExpr>,
    pub params: Vec<Spanned<MirExpr>>,
    pub next: MirJumpInfo,
    pub unwind: Option<MirJumpInfo>,
}

impl core::fmt::Display for MirCallInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.retplace.body.fmt(f)?;
        f.write_str(" = ")?;
        self.targ.body.fmt(f)?;
        f.write_str("(")?;
        let mut sep = "";

        for param in &self.params {
            f.write_str(sep)?;
            sep = ", ";
            param.body.fmt(f)?;
        }

        f.write_str(")")?;

        f.write_str(" next ")?;

        self.next.fmt(f)?;

        if let Some(unwind) = &self.unwind {
            f.write_str(" unwind ")?;
            unwind.fmt(f)?;
        }

        Ok(())
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirTailcallInfo {
    pub targ: Spanned<MirExpr>,
    pub params: Vec<Spanned<MirExpr>>,
    pub unwind: Option<MirJumpInfo>,
}

impl core::fmt::Display for MirTailcallInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.targ.body.fmt(f)?;
        f.write_str("(")?;
        let mut sep = "";

        for param in &self.params {
            f.write_str(sep)?;
            sep = ", ";
            param.body.fmt(f)?;
        }

        f.write_str(")")?;

        if let Some(unwind) = &self.unwind {
            f.write_str(" unwind ")?;
            unwind.fmt(f)?;
        }

        Ok(())
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirJumpInfo {
    pub targbb: BasicBlockId,
    pub remaps: Vec<(SsaVarId, SsaVarId)>,
}

impl core::fmt::Display for MirJumpInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.targbb.fmt(f)?;
        f.write_str(" [")?;

        let mut sep = "";

        for (l, r) in &self.remaps {
            f.write_str(sep)?;
            sep = ", ";
            l.fmt(f)?;
            f.write_str(" => ")?;
            r.fmt(f)?;
        }

        f.write_str("]")
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirBasicBlock {
    pub id: BasicBlockId,
    pub stmts: Vec<Spanned<MirStatement>>,
    pub term: Spanned<MirTerminator>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirFunctionBody {
    pub bbs: Vec<MirBasicBlock>,
    pub vardbg_info: HashMap<SsaVarId, Spanned<Symbol>>,
}

impl MirFunctionBody {
    pub fn display_body(
        &self,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        for bb in &self.bbs {
            self.display_block(bb, f, tabs.nest())?;
        }
        Ok(())
    }

    fn display_block(
        &self,
        bb: &MirBasicBlock,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        bb.id.fmt(f)?;
        f.write_str(": {\n")?;

        for stmt in &bb.stmts {
            self.display_stmt(stmt, f, tabs.nest())?;
        }

        self.display_term(&bb.term, f, tabs.nest())?;

        tabs.fmt(f)?;
        f.write_str("}\n")?;
        Ok(())
    }

    fn display_stmt(
        &self,
        stmt: &MirStatement,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        tabs.fmt(f)?;
        match stmt {
            MirStatement::Write(ptr, val) => {
                f.write_fmt(format_args!("write(*{},{})", ptr.body, val.body))
            }
            MirStatement::Declare { var, ty, init } => f.write_fmt(format_args!(
                "let {}: {} = {};",
                var.body, ty.body, init.body
            )),
            MirStatement::StoreDead(var) => f.write_fmt(format_args!("store dead {};", var)),
            MirStatement::EndRegion(region) => f.write_fmt(format_args!("end region {};", region)),
        }
    }

    fn display_term(
        &self,
        term: &MirTerminator,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        tabs.fmt(f)?;

        match term {
            MirTerminator::Call(call) => f.write_fmt(format_args!("call {}", call)),
            MirTerminator::Tailcall(tc) => f.write_fmt(format_args!("tailcall {}", tc)),
            MirTerminator::Return(expr) => f.write_fmt(format_args!("return {}", expr.body)),
            MirTerminator::Jump(jmp) => f.write_fmt(format_args!("jump {}", jmp)),
            MirTerminator::Unreachable => f.write_str("unreachable"),
        }
    }
}

pub struct MirConverter<'a> {
    defs: &'a Definitions,
}

impl<'a> MirConverter<'a> {
    pub fn new(defs: &'a Definitions) -> Self {
        Self { defs }
    }
}
