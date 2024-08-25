use xlang::abi::{
    collection::{HashMap, HashSet},
    pair::Pair,
};

use crate::{
    ast::{CharType, Mutability, StringType},
    helpers::{FetchIncrement, TabPrinter},
    interning::Symbol,
    lex::Error,
    span::{synthetic, Span},
};

use super::{
    cx,
    generics::GenericArgs,
    hir::HirVarId,
    intrin::IntrinsicDef,
    ty::{FieldName, FnType, IntType, Type},
    tyck::{Movability, ThirBlock, ThirExpr, ThirExprInner, ThirStatement, ThirVarDef},
    DefId, Definitions, SemaHint, Spanned,
};

pub mod mir_macro;

pub mod match_lower;

pub mod transform;

pub use mir_macro::{mir, mir_basic_block, mir_expr, mir_fnty, mir_stmt, mir_type};

pub use crate::sema::hir::{BinaryOp, UnaryOp};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct RegionId(pub(crate) u32);

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct BasicBlockId(pub(crate) u32);

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SsaVarId(pub(crate) u32);

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

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum DropFlagState {
    Init,
    Uninit,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirExpr {
    Unreachable,
    Uninit(Type),
    Var(SsaVarId),
    Read(Box<Spanned<MirExpr>>),
    Alloca(Mutability, Type, Box<Spanned<MirExpr>>),
    AllocaDrop(Type, DropFlagState),
    ConstInt(IntType, u128),
    ConstString(StringType, Symbol),
    ConstChar(CharType, u32),
    Const(DefId, GenericArgs),
    // Also used for const generic params
    InlineConst(cx::ConstExpr),
    Retag(RefKind, Mutability, Box<Spanned<MirExpr>>),
    Cast(Box<Spanned<MirExpr>>, Type),
    Tuple(Vec<Spanned<MirExpr>>),
    Intrinsic(IntrinsicDef, GenericArgs),
    FieldProject(Box<Spanned<MirExpr>>, FieldName),
    GetSubobject(Box<Spanned<MirExpr>>, FieldName),
    Ctor(MirConstructor),
    BinaryExpr(
        Spanned<BinaryOp>,
        Box<Spanned<MirExpr>>,
        Box<Spanned<MirExpr>>,
    ),
    UnaryExpr(Spanned<UnaryOp>, Box<Spanned<MirExpr>>),
    GetSymbol(DefId),
}

impl MirExpr {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        match self {
            MirExpr::Unreachable
            | MirExpr::Var(_)
            | MirExpr::GetSymbol(_)
            | MirExpr::ConstInt(_, _)
            | MirExpr::ConstString(_, _)
            | MirExpr::ConstChar(_, _) => self.clone(),
            MirExpr::Read(expr) => MirExpr::Read(Box::new(
                expr.copy_span(|expr| expr.substitute_generics(args)),
            )),
            MirExpr::FieldProject(base, field) => MirExpr::FieldProject(
                Box::new(base.copy_span(|expr| expr.substitute_generics(args))),
                *field,
            ),
            MirExpr::GetSubobject(base, field) => MirExpr::GetSubobject(
                Box::new(base.copy_span(|expr| expr.substitute_generics(args))),
                *field,
            ),
            MirExpr::Uninit(ty) => MirExpr::Uninit(ty.substitute_generics(args)),
            MirExpr::Alloca(mt, ty, init) => MirExpr::Alloca(
                *mt,
                ty.substitute_generics(args),
                Box::new(init.copy_span(|expr| expr.substitute_generics(args))),
            ),
            MirExpr::AllocaDrop(ty, st) => MirExpr::AllocaDrop(ty.substitute_generics(args), *st),
            MirExpr::Const(def, generics) => {
                MirExpr::Const(*def, generics.substitute_generics(args))
            }
            MirExpr::InlineConst(cx) => MirExpr::InlineConst(cx.substitute_generics(args)),
            MirExpr::Retag(_, _, _) => todo!(),
            MirExpr::Cast(_, _) => todo!(),
            MirExpr::Tuple(_) => todo!(),
            MirExpr::Intrinsic(_, _) => todo!(),
            MirExpr::Ctor(_) => todo!(),
            MirExpr::BinaryExpr(_, _, _) => todo!(),
            MirExpr::UnaryExpr(_, _) => todo!(),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirConstructor {
    pub ctor_def: DefId,
    pub fields: Vec<(FieldName, Spanned<MirExpr>)>,
    pub rest_init: Option<Box<Spanned<MirExpr>>>,
}

impl MirConstructor {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
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
    Discard(Spanned<MirExpr>),
    Dealloca(MirExpr),
    MarkAll(MirExpr, DropFlagState),
    MarkDropState(MirExpr, FieldName, DropFlagState),
    CaptureException(SsaVarId),
}

impl MirStatement {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirTerminator {
    Call(MirCallInfo),
    Tailcall(MirTailcallInfo),
    Return(Spanned<MirExpr>),
    Jump(MirJumpInfo),
    Unreachable,
    Resume,
    DropInPlace(MirDropInfo),
    Branch(MirBranchInfo),
    SwitchInt(MirSwitchIntInfo),
}

impl MirTerminator {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirSwitchIntInfo {
    pub expr: Spanned<MirExpr>,
    pub ty: IntType,
    pub cases: Vec<(u128, MirJumpInfo)>,
    pub default: Option<MirJumpInfo>,
}

impl MirSwitchIntInfo {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirBranchInfo {
    pub cond: Box<Spanned<MirExpr>>,
    pub if_block: MirJumpInfo,
    pub else_block: MirJumpInfo,
}

impl MirBranchInfo {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct MirJumpInfo {
    pub targbb: BasicBlockId,
    pub remaps: Vec<(SsaVarId, SsaVarId)>,
    pub fallthrough: bool,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirDropInfo {
    /// The target of the drop-in-place. Must be a pointer or reference type, and must dereference to a mutable place
    pub target: MirExpr,
    /// The drop flags point. If present, be a pointer to the `Type::DropFlags(ty)` type where `ty` is the type of `*target`
    /// If absent, the drop target is presumed to be statically known to be live, and thus dropped unconditionally.
    pub flags: Option<MirExpr>,
    /// The Next basic Block when the drop call returns successfully
    pub next: MirJumpInfo,
    /// The basic block to unwind to if any destructor unwinds.
    pub unwind: Option<MirJumpInfo>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirCallInfo {
    pub retplace: Spanned<SsaVarId>, // id of the return place, which is made live in the next basic block
    pub targ: Spanned<MirExpr>,
    pub fnty: Box<FnType>,
    pub params: Vec<Spanned<MirExpr>>,
    pub next: MirJumpInfo,
    pub unwind: Option<MirJumpInfo>,
}

impl MirCallInfo {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirTailcallInfo {
    pub targ: Spanned<MirExpr>,
    pub fnty: Box<FnType>,
    pub params: Vec<Spanned<MirExpr>>,
    pub unwind: Option<MirJumpInfo>,
}

impl MirTailcallInfo {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum BorrowckErrorCategory {
    MovedFrom,
    NotAssigned,
    CannotMove,
    CannotAssign,
}

impl BasicBlockId {
    pub const UNUSED: BasicBlockId = BasicBlockId(!0);

    pub const fn id(self) -> u32 {
        self.0
    }

    pub const fn is_successor(self, other: BasicBlockId) -> bool {
        other.0 != !0 && self.0.saturating_add(1) == other.0
    }

    #[doc(hidden)]
    pub const fn __new_unchecked(x: u32) -> Self {
        Self(x)
    }
}

impl Default for BasicBlockId {
    fn default() -> Self {
        Self::UNUSED
    }
}

impl core::borrow::Borrow<u32> for BasicBlockId {
    fn borrow(&self) -> &u32 {
        &self.0
    }
}

impl SsaVarId {
    pub const INVALID: SsaVarId = Self(!0);
    pub const fn id(self) -> u32 {
        self.0
    }

    #[doc(hidden)]
    pub const fn __new_unchecked(x: u32) -> Self {
        Self(x)
    }
}

impl core::fmt::Display for DropFlagState {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Init => f.write_str("init"),
            Self::Uninit => f.write_str("uninit"),
        }
    }
}

impl core::fmt::Display for MirConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ctor_def.fmt(f)?;
        f.write_str("{ ")?;

        let mut sep = "";

        for (field, init) in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            field.fmt(f)?;
            f.write_str(": (")?;
            init.body.fmt(f)?;
            f.write_str(")")?;
        }

        if let Some(rest_init) = &self.rest_init {
            f.write_str(sep)?;
            f.write_str("..(")?;
            rest_init.body.fmt(f)?;
            f.write_str(")")?;
        }
        f.write_str(" }")
    }
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
            MirExpr::ConstInt(ity, val) => f.write_fmt(format_args!("{}_{}", val, ity)),
            MirExpr::ConstString(_, val) => {
                f.write_fmt(format_args!("\"{}\"", val.escape_default()))
            }
            MirExpr::ConstChar(CharType::Default, val) => {
                f.write_str("'")?;
                if let Some(c) = char::from_u32(*val) {
                    c.escape_default().fmt(f)?;
                } else {
                    f.write_fmt(format_args!("\\u{{invalid char: {:04x}}}", val))?;
                }

                f.write_str("'")
            }
            MirExpr::ConstChar(CharType::Byte, val) => {
                f.write_str("'")?;
                match val {
                    &val @ 0x20..=0x7F => (val as u8 as char).fmt(f)?,
                    val => f.write_fmt(format_args!("\\x{:02x}", val))?,
                }
                f.write_str("'")
            }
            MirExpr::Const(defid, generics) => f.write_fmt(format_args!("{}{}", defid, generics)),
            MirExpr::InlineConst(expr) => expr.fmt(f),
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
            MirExpr::Tuple(vals) => {
                f.write_str("(")?;

                let mut sep = "";

                for val in vals {
                    f.write_str(sep)?;
                    sep = ", ";
                    val.body.fmt(f)?;
                }
                f.write_str(")")
            }
            MirExpr::Intrinsic(intrin, generics) => {
                f.write_fmt(format_args!("{}{}", intrin.name(), generics))
            }
            MirExpr::FieldProject(base, field) => {
                f.write_fmt(format_args!("&raw (*{}).{}", base.body, field))
            }
            MirExpr::GetSubobject(base, field) => {
                f.write_fmt(format_args!("({}).{}", base.body, field))
            }
            MirExpr::Ctor(ctor) => ctor.fmt(f),
            MirExpr::BinaryExpr(op, lhs, rhs) => {
                f.write_fmt(format_args!("({} {} {})", lhs.body, op.body, rhs.body))
            }
            MirExpr::AllocaDrop(ty, state) => {
                f.write_fmt(format_args!("alloca_drop {} {}", ty, state))
            }
            MirExpr::Uninit(ty) => f.write_fmt(format_args!("uninit {}", ty)),
            MirExpr::GetSymbol(sym) => f.write_fmt(format_args!("get_symbol {}", sym)),
            MirExpr::UnaryExpr(op, expr) => {
                f.write_fmt(format_args!("({} {})", op.body, expr.body))
            }
        }
    }
}

impl core::fmt::Display for MirDropInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.target.fmt(f)?;
        f.write_str(" ")?;
        if let Some(flags) = &self.flags {
            f.write_fmt(format_args!("{{{}}} ", flags))?;
        }

        f.write_fmt(format_args!("next {}", self.next))?;
        if let Some(unwind) = &self.unwind {
            f.write_fmt(format_args!(" unwind {}", unwind))?;
        }
        Ok(())
    }
}

impl core::fmt::Display for MirCallInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.retplace.body.fmt(f)?;
        f.write_str(" = ")?;
        self.targ.body.fmt(f)?;
        f.write_str(": ")?;
        self.fnty.fmt(f)?;
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

impl core::fmt::Display for MirTailcallInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.targ.body.fmt(f)?;
        f.write_str(": ")?;
        self.fnty.fmt(f)?;
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

impl core::fmt::Display for MirJumpInfo {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.fallthrough {
            f.write_str("fallthrough ")?;
        }

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
    pub incoming_vars: Vec<(SsaVarId, Type)>,
    pub id: BasicBlockId,
    pub stmts: Vec<Spanned<MirStatement>>,
    pub term: Spanned<MirTerminator>,
}

impl MirBasicBlock {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirFunctionBody {
    pub bbs: Vec<MirBasicBlock>,
    pub vardbg_info: HashMap<SsaVarId, Spanned<Symbol>>,
    pub localitems: Vec<(Symbol, DefId)>,
}

impl MirFunctionBody {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        todo!()
    }

    pub fn display_body(
        &self,
        defs: &Definitions,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        for &(name, defid) in &self.localitems {
            let nested = tabs.nest();
            defs.display_item(f, defid, &name, nested)?;
        }
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
        tabs.fmt(f)?;
        bb.id.fmt(f)?;
        f.write_str(": { [")?;

        let mut sep = "";

        for (var, ty) in &bb.incoming_vars {
            f.write_fmt(format_args!("{}{}: {}", sep, var, ty))?;
            sep = ", ";
        }

        f.write_str("]\n")?;
        for stmt in &bb.stmts {
            self.display_stmt(stmt, f, tabs.nest())?;
            f.write_str("\n")?;
        }

        self.display_term(&bb.term, f, tabs.nest())?;
        f.write_str("\n")?;

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
            MirStatement::Discard(expr) => f.write_fmt(format_args!("{};", expr.body)),
            MirStatement::Dealloca(expr) => f.write_fmt(format_args!("dealloca {};", expr)),
            MirStatement::MarkAll(expr, state) => {
                f.write_fmt(format_args!("mark_all({},{});", expr, state))
            }
            MirStatement::MarkDropState(expr, field, state) => {
                f.write_fmt(format_args!("mark_drop({}.{}, {});", expr, field, state))
            }
            MirStatement::CaptureException(varid) => {
                f.write_fmt(format_args!("let {} = current_exception();", varid))
            }
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
            MirTerminator::Resume => f.write_str("resume"),
            MirTerminator::DropInPlace(drop) => f.write_fmt(format_args!("drop_in_place {}", drop)),
            MirTerminator::Branch(branch) => {
                f.write_str("branch if ")?;
                branch.cond.body.fmt(f)?;
                f.write_str(" then ")?;
                branch.if_block.fmt(f)?;
                f.write_str(" else ")?;
                branch.else_block.fmt(f)
            }
            MirTerminator::SwitchInt(s) => {
                let nested = tabs.nest();
                f.write_fmt(format_args!("switch {}: {} ", s.expr.body, s.ty))?;
                f.write_str("{\n")?;
                for (val, jmp) in &s.cases {
                    f.write_fmt(format_args!("{}{} => {}\n", nested, val, jmp))?;
                }
                if let Some(default) = &s.default {
                    f.write_fmt(format_args!("{}default => {}\n", nested, default))?;
                }
                tabs.fmt(f)?;
                f.write_str("}")
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirVarAssignment {
    pub cur_var: SsaVarId,
    pub var_kind: SsaVarKind,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaVarKind {
    Register,
    Alloca,
}

pub struct UnbuiltBasicBlock {
    id: BasicBlockId,
    stmts: Vec<Spanned<MirStatement>>,
    incoming_vars: Vec<(SsaVarId, Type)>,
}

impl UnbuiltBasicBlock {
    pub const fn new() -> Self {
        Self {
            id: BasicBlockId(0),
            stmts: vec![],
            incoming_vars: Vec::new(),
        }
    }

    pub const fn unused() -> Self {
        Self {
            id: BasicBlockId::UNUSED,
            stmts: vec![],
            incoming_vars: Vec::new(),
        }
    }

    pub fn finish(self, term: Spanned<MirTerminator>) -> MirBasicBlock {
        MirBasicBlock {
            id: self.id,
            stmts: self.stmts,
            term,
            incoming_vars: self.incoming_vars,
        }
    }

    pub fn reset(&mut self) {
        *self = Self::unused();
    }

    pub fn finish_and_reset(&mut self, term: Spanned<MirTerminator>) -> MirBasicBlock {
        core::mem::replace(self, Self::unused()).finish(term)
    }
}

pub struct MirConverter<'a> {
    defs: &'a Definitions,
    at_item: DefId,
    curmod: DefId,
    basic_blocks: Vec<MirBasicBlock>,
    cur_basic_block: UnbuiltBasicBlock,
    var_names: HashMap<HirVarId, HirVarAssignment>,
    moved_from_locs: HashMap<HirVarId, Span>,
    hir_var_map: HashMap<HirVarId, ThirVarDef>,
    hir_def_spans: HashMap<HirVarId, Span>,
    mir_debug_map: HashMap<SsaVarId, Spanned<Symbol>>,
    nextvar: u32,
    nextbb: u32,
    localitems: Vec<(Symbol, DefId)>,
}

impl<'a> MirConverter<'a> {
    pub fn new(
        defs: &'a Definitions,
        at_item: DefId,
        curmod: DefId,
        hir_var_map: HashMap<HirVarId, ThirVarDef>,
        localitems: Vec<(Symbol, DefId)>,
        fnty: FnType,
    ) -> Self {
        let mut result = Self {
            defs,
            at_item,
            curmod,
            basic_blocks: vec![],
            cur_basic_block: UnbuiltBasicBlock::new(),
            var_names: HashMap::new(),
            moved_from_locs: HashMap::new(),
            hir_var_map,
            hir_def_spans: HashMap::new(),
            mir_debug_map: HashMap::new(),
            nextvar: 0,
            nextbb: 1,
            localitems,
        };

        for (id, param) in fnty.paramtys.into_iter().enumerate() {
            let hir_id = HirVarId(id as u32);
            let newvarid = SsaVarId(result.nextvar.fetch_increment());
            result.var_names.insert(
                hir_id,
                HirVarAssignment {
                    cur_var: newvarid,
                    var_kind: SsaVarKind::Register,
                },
            );
            result
                .cur_basic_block
                .incoming_vars
                .push((newvarid, param.body));
        }

        result
    }

    fn diag_lvalue_kind(val: &ThirExprInner) -> &'static str {
        match val {
            ThirExprInner::Var(_) => "local variable",
            ThirExprInner::MemberAccess(_, _) => "struct field",
            ThirExprInner::Const(_, _)
            | ThirExprInner::ConstInt(_, _)
            | ThirExprInner::ConstString(_, _)
            | ThirExprInner::ConstChar(_, _)
            | ThirExprInner::Cast(_, _)
            | ThirExprInner::Tuple(_)
            | ThirExprInner::Read(_)
            | ThirExprInner::Unreachable
            | ThirExprInner::Ctor(_)
            | ThirExprInner::BinaryExpr(_, _, _)
            | ThirExprInner::Array(_)
            | ThirExprInner::UnaryExpr(_, _)
            | ThirExprInner::Index(_, _) => todo!(),
        }
    }

    fn lower_register(
        &mut self,
        val: Spanned<ThirExpr>,
        force_lvalue: bool,
    ) -> super::Result<(Spanned<MirExpr>, SsaVarKind, SsaVarId)> {
        match val.cat {
            super::tyck::ValueCategory::Lvalue(_, _) => {
                let ty = val.ty.clone();
                let (val, kind) = self.lower_lvalue_subexpr(val, force_lvalue)?;

                match val {
                    Spanned {
                        body: MirExpr::Var(var),
                        span,
                    } => Ok((
                        Spanned {
                            body: MirExpr::Var(var),
                            span,
                        },
                        kind,
                        var,
                    )),
                    val => todo!("Not in a register {}", val.body),
                }
            }
            super::tyck::ValueCategory::Rvalue => {
                todo!("rvalue")
            }
        }
    }

    fn lower_lvalue_subexpr(
        &mut self,
        val: Spanned<ThirExpr>,
        force_lvalue: bool,
    ) -> super::Result<(Spanned<MirExpr>, SsaVarKind)> {
        let span = val.span;
        let ty = val.body.ty;
        let (mv, mt) = match val.body.cat {
            super::tyck::ValueCategory::Lvalue(mv, mt) => (mv, mt),
            _ => panic!("Only lvalues can be accessed"),
        };

        match val.body.inner {
            ThirExprInner::Var(hirvar) => {
                if let Some(loc) = self.moved_from_locs.get(&hirvar).copied() {
                    let name = self
                        .hir_var_map
                        .get(&hirvar)
                        .and_then(|val| val.debug_name)
                        .map(|val| val.body)
                        .unwrap_or_else(|| {
                            Symbol::intern_by_val(format!("{{internal variable {}}}", hirvar))
                        });
                    return Err(super::Error {
                        span,
                        text: format!("Variable `{}` is used after move", name),
                        category: super::ErrorCategory::BorrowckError(
                            BorrowckErrorCategory::MovedFrom,
                        ),
                        at_item: self.at_item,
                        containing_item: self.curmod,
                        relevant_item: self.at_item,
                        hints: vec![SemaHint {
                            text: format!("`{}` is moved from here", name),
                            itemref: self.at_item,
                            refspan: loc,
                        }, SemaHint{
                            text: format!("move occurs because type `{}` does not implement the `Copy` trait", ty),
                            itemref: self.defs.type_defid(&ty),
                            refspan: loc,
                        }],
                    });
                }

                if !self.defs.is_copy(&ty) {
                    self.moved_from_locs.insert(hirvar, span);
                }

                if let Some(var_name) = self.var_names.get(&hirvar) {
                    let inner = Spanned {
                        body: MirExpr::Var(var_name.cur_var),
                        span,
                    };
                    if force_lvalue && var_name.var_kind == SsaVarKind::Register {
                        let alloca = Spanned {
                            body: MirExpr::Alloca(mt, ty.clone(), Box::new(inner)),
                            span,
                        };
                        let newvarid = SsaVarId(self.nextvar.fetch_increment());
                        let init_stat = Spanned {
                            body: MirStatement::Declare {
                                var: Spanned {
                                    body: newvarid,
                                    span,
                                },
                                ty: Spanned { body: ty, span },
                                init: alloca,
                            },
                            span,
                        };
                        self.cur_basic_block.stmts.push(init_stat);
                        let assign = HirVarAssignment {
                            cur_var: newvarid,
                            var_kind: SsaVarKind::Alloca,
                        };

                        *self.var_names.get_mut(&hirvar).unwrap() = assign;

                        let inner = Spanned {
                            body: MirExpr::Var(newvarid),
                            span,
                        };
                        Ok((inner, SsaVarKind::Alloca))
                    } else {
                        Ok((inner, var_name.var_kind))
                    }
                } else {
                    let name = self
                        .hir_var_map
                        .get(&hirvar)
                        .and_then(|val| val.debug_name)
                        .map(|val| val.body)
                        .unwrap_or_else(|| {
                            Symbol::intern_by_val(format!("{{internal variable {}}}", hirvar))
                        });

                    Err(super::Error {
                        span,
                        text: format!(
                            "Variable `{}` (`{}`) is used but is not initialized",
                            name, hirvar
                        ),
                        category: super::ErrorCategory::BorrowckError(
                            BorrowckErrorCategory::NotAssigned,
                        ),
                        at_item: self.at_item,
                        containing_item: self.curmod,
                        relevant_item: self.at_item,
                        hints: self.hir_def_spans.get(&hirvar).map_or_else(
                            || vec![],
                            |x| {
                                vec![SemaHint {
                                    text: format!("Declared but not initalized here"),
                                    itemref: self.at_item,
                                    refspan: *x,
                                }]
                            },
                        ),
                    })
                }
            }
            ThirExprInner::MemberAccess(inner, mem) => {
                let (inner, kind) = self.lower_lvalue_subexpr(*inner, force_lvalue)?;
                let span = val.span;
                match kind {
                    SsaVarKind::Register => Ok((
                        Spanned {
                            span,
                            body: MirExpr::GetSubobject(Box::new(inner), *mem),
                        },
                        SsaVarKind::Register,
                    )),
                    SsaVarKind::Alloca => Ok((
                        Spanned {
                            span,
                            body: MirExpr::FieldProject(Box::new(inner), *mem),
                        },
                        SsaVarKind::Alloca,
                    )),
                }
            }
            ThirExprInner::Read(_)
            | ThirExprInner::Unreachable
            | ThirExprInner::Const(_, _)
            | ThirExprInner::ConstInt(_, _)
            | ThirExprInner::ConstString(_, _)
            | ThirExprInner::Cast(_, _)
            | ThirExprInner::Tuple(_)
            | ThirExprInner::Ctor(_)
            | ThirExprInner::BinaryExpr(_, _, _)
            | ThirExprInner::Array(_)
            | ThirExprInner::UnaryExpr(_, _)
            | ThirExprInner::Index(_, _)
            | ThirExprInner::ConstChar(_, _) => unreachable!("cannot access"),
        }
    }

    pub fn lower_read(&mut self, val: Spanned<ThirExpr>) -> super::Result<Spanned<MirExpr>> {
        let span = val.span;
        let ty = val.ty.clone();
        let (mv, mt) = match val.body.cat {
            super::tyck::ValueCategory::Lvalue(mv, mt) => (mv, mt),
            _ => panic!("Only lvalues can be read"),
        };

        if mv != Movability::Movable && !self.defs.is_copy(&ty) {
            return Err(super::Error {
                span,
                text: format!(
                    "Cannot move out of `{}`",
                    Self::diag_lvalue_kind(&val.body.inner)
                ), // todo: Improve this diagnostis
                category: super::ErrorCategory::BorrowckError(BorrowckErrorCategory::CannotMove),
                at_item: self.at_item,
                containing_item: self.curmod,
                relevant_item: self.at_item,
                hints: vec![SemaHint {
                    text: format!(
                        "Move occurs because `{}` does not implement the `Copy` trait",
                        ty
                    ),
                    itemref: self.defs.type_defid(&ty),
                    refspan: span,
                }],
            });
        }
        eprint!("Lowered {} ->", val.body);
        let (expr, kind) = self.lower_lvalue_subexpr(val, false)?;

        eprintln!("{}: {:?}", expr.body, kind);

        match kind {
            SsaVarKind::Alloca => Ok(Spanned {
                body: MirExpr::Read(Box::new(expr)),
                span,
            }),
            SsaVarKind::Register => Ok(expr),
        }
    }

    pub fn lower_expr(&mut self, val: Spanned<ThirExpr>) -> super::Result<Spanned<MirExpr>> {
        let span = val.span;
        match val.body.inner {
            super::tyck::ThirExprInner::Read(inner) => self.lower_read(*inner),
            super::tyck::ThirExprInner::Unreachable => Ok(Spanned {
                span,
                body: MirExpr::Unreachable,
            }),
            super::tyck::ThirExprInner::Var(_) => {
                panic!("only top level rvalues get lowered by `lower_expr`")
            }
            super::tyck::ThirExprInner::Const(defid, generics) => {
                if let Some(intrin) = self.defs.get_intrinsic(defid) {
                    Ok(Spanned {
                        span,
                        body: MirExpr::Intrinsic(intrin, generics),
                    })
                } else {
                    Ok(Spanned {
                        span,
                        body: MirExpr::Const(defid, generics),
                    })
                }
            }
            super::tyck::ThirExprInner::ConstInt(intty, ival) => {
                let intty = intty
                    .map(|ity| ity.body)
                    .unwrap_or_else(|| match &val.body.ty {
                        Type::Int(ity) => *ity,
                        _ => panic!("wrong type assigned to literal"),
                    });
                Ok(Spanned {
                    span,
                    body: MirExpr::ConstInt(intty, ival),
                })
            }
            super::tyck::ThirExprInner::ConstString(sty, val) => Ok(Spanned {
                span,
                body: MirExpr::ConstString(sty, *val),
            }),
            super::tyck::ThirExprInner::ConstChar(cty, val) => Ok(Spanned {
                span,
                body: MirExpr::ConstChar(cty, val),
            }),
            super::tyck::ThirExprInner::Cast(inner, ty) => {
                let inner = self.lower_expr(*inner)?;

                Ok(Spanned {
                    span,
                    body: MirExpr::Cast(Box::new(inner), ty.body),
                })
            }
            super::tyck::ThirExprInner::Tuple(vals) => {
                let vals = vals
                    .into_iter()
                    .map(|expr| self.lower_expr(expr))
                    .collect::<super::Result<Vec<_>>>()?;

                Ok(Spanned {
                    span,
                    body: MirExpr::Tuple(vals),
                })
            }
            super::tyck::ThirExprInner::Ctor(ctor) => {
                let fields = ctor
                    .body
                    .fields
                    .into_iter()
                    .map(|(fname, expr)| Ok((fname.body, self.lower_expr(expr)?)))
                    .collect::<super::Result<Vec<_>>>()?;

                let rest_init = ctor
                    .body
                    .rest_init
                    .map(|expr| self.lower_expr(*expr))
                    .transpose()?
                    .map(Box::new);

                let ctor_def = ctor.body.base;

                Ok(Spanned {
                    span,
                    body: MirExpr::Ctor(MirConstructor {
                        ctor_def,
                        fields,
                        rest_init,
                    }),
                })
            }
            super::tyck::ThirExprInner::BinaryExpr(op, lhs, rhs) => Ok(Spanned {
                span,
                body: MirExpr::BinaryExpr(
                    op,
                    Box::new(self.lower_expr(*lhs)?),
                    Box::new(self.lower_expr(*rhs)?),
                ),
            }),
            super::tyck::ThirExprInner::Array(_) => todo!("array"),
            super::tyck::ThirExprInner::Index(_, _) => todo!("index"),
            super::tyck::ThirExprInner::MemberAccess(_, _) => {
                panic!("only top level rvalues get lowered by `lower_expr`")
            }
            super::tyck::ThirExprInner::UnaryExpr(op, lhs) => Ok(Spanned {
                span,
                body: MirExpr::UnaryExpr(op, Box::new(self.lower_expr(*lhs)?)),
            }),
        }
    }

    pub fn lower_write(
        &mut self,
        dest: Spanned<ThirExpr>,
        val: Spanned<MirExpr>,
        op: Option<Spanned<BinaryOp>>,
        stat_span: Span,
    ) -> super::Result<()> {
        let span = val.span;
        let ty = dest.body.ty;
        let (mv, mt) = match dest.body.cat {
            super::tyck::ValueCategory::Lvalue(mv, mt) => (mv, mt),
            _ => panic!("Only lvalues can be written to"),
        };

        if mt != Mutability::Mut && mv != Movability::Movable {
            // Some funny buisness with assignments
            return Err(super::Error {
                span,
                text: format!(
                    "Cannot write to {}",
                    Self::diag_lvalue_kind(&dest.body.inner)
                ),
                category: super::ErrorCategory::BorrowckError(BorrowckErrorCategory::CannotAssign),
                at_item: self.at_item,
                containing_item: self.curmod,
                relevant_item: self.at_item,
                hints: vec![],
            });
        }

        let val = match op {
            Some(op) => todo!("{}=", op.body),
            None => val,
        };

        match dest.body.inner {
            ThirExprInner::Var(hirvar) => {
                if let Some(assign) = self.var_names.get_mut(&hirvar) {
                    if mt != Mutability::Mut {
                        let name = self
                            .hir_var_map
                            .get(&hirvar)
                            .and_then(|val| val.debug_name)
                            .map(|val| val.body)
                            .unwrap_or_else(|| {
                                Symbol::intern_by_val(format!("{{internal variable {}}}", hirvar))
                            });

                        return Err(super::Error {
                            span,
                            text: format!(
                                "Cannot assign to `{}` (an immutable local variable)",
                                name
                            ),
                            category: super::ErrorCategory::BorrowckError(
                                BorrowckErrorCategory::CannotAssign,
                            ),
                            at_item: self.at_item,
                            containing_item: self.curmod,
                            relevant_item: self.at_item,
                            hints: vec![],
                        });
                    }

                    self.moved_from_locs.remove(&hirvar);

                    match assign.var_kind {
                        SsaVarKind::Alloca => {
                            let inner = MirExpr::Var(assign.cur_var);
                            let stat = MirStatement::Write(Spanned { body: inner, span }, val);

                            self.cur_basic_block.stmts.push(Spanned {
                                body: stat,
                                span: stat_span,
                            });
                        }
                        SsaVarKind::Register => {
                            let newvar = SsaVarId(self.nextvar.fetch_increment());

                            let stat = MirStatement::Declare {
                                var: Spanned { span, body: newvar },
                                ty: Spanned { body: ty, span },
                                init: val,
                            };

                            self.cur_basic_block.stmts.push(Spanned {
                                body: stat,
                                span: stat_span,
                            });

                            let stat = MirStatement::StoreDead(core::mem::replace(
                                &mut assign.cur_var,
                                newvar,
                            ));
                        }
                    }
                } else {
                    let newvar = SsaVarId(self.nextvar.fetch_increment());
                    let (val, kind) = if mt == Mutability::Mut {
                        // mut requires a stable address in most cases, eagerly `alloca` it
                        let inner = MirExpr::Alloca(mt, ty.clone(), Box::new(val));

                        (Spanned { body: inner, span }, SsaVarKind::Alloca)
                    } else {
                        (val, SsaVarKind::Register)
                    };

                    let stat = MirStatement::Declare {
                        var: Spanned { span, body: newvar },
                        ty: Spanned { body: ty, span },
                        init: val,
                    };

                    self.var_names.insert(
                        hirvar,
                        HirVarAssignment {
                            cur_var: newvar,
                            var_kind: kind,
                        },
                    );

                    self.cur_basic_block.stmts.push(Spanned {
                        body: stat,
                        span: stat_span,
                    });
                }
            }
            ThirExprInner::MemberAccess(inner, field) => {
                let (inner, _) = self.lower_lvalue_subexpr(*inner, true)?;
                let proj = MirExpr::FieldProject(Box::new(inner), field.body);

                let stat = MirStatement::Write(Spanned { body: proj, span }, val);

                self.cur_basic_block.stmts.push(Spanned {
                    body: stat,
                    span: stat_span,
                });
            }

            ThirExprInner::Read(_)
            | ThirExprInner::Unreachable
            | ThirExprInner::Const(_, _)
            | ThirExprInner::ConstInt(_, _)
            | ThirExprInner::ConstString(_, _)
            | ThirExprInner::ConstChar(_, _)
            | ThirExprInner::Cast(_, _)
            | ThirExprInner::Tuple(_)
            | ThirExprInner::Ctor(_)
            | ThirExprInner::BinaryExpr(_, _, _)
            | ThirExprInner::Array(_)
            | ThirExprInner::Index(_, _)
            | ThirExprInner::UnaryExpr(_, _) => unreachable!(),
        }

        Ok(())
    }

    fn make_jump_with_assigns(
        &mut self,
        bb_id: BasicBlockId,
        existing_assignments: &HashMap<HirVarId, HirVarAssignment>,
    ) -> MirJumpInfo {
        let mut remaps = Vec::new();
        for Pair(hirvar, hirassign) in &self.var_names {
            if let Some(existing_assign) = existing_assignments.get(hirvar) {
                remaps.push((hirassign.cur_var, existing_assign.cur_var))
            }
        }

        MirJumpInfo {
            targbb: bb_id,
            remaps,
            fallthrough: false,
        }
    }

    fn make_jump(
        &mut self,
        bb_id: BasicBlockId,
    ) -> (
        MirJumpInfo,
        HashMap<HirVarId, HirVarAssignment>,
        Vec<(SsaVarId, Type)>,
    ) {
        println!("{}", bb_id);
        let mut assignments = HashMap::new();
        let mut remaps = Vec::new();
        let mut incoming_vars = Vec::new();
        for Pair(hirvar, hirassign) in &self.var_names {
            if !self.moved_from_locs.get(hirvar).is_some()
                || hirassign.var_kind == SsaVarKind::Alloca
            {
                let newvar = SsaVarId(self.nextvar.fetch_increment());
                assignments.insert(
                    *hirvar,
                    HirVarAssignment {
                        cur_var: newvar,
                        ..*hirassign
                    },
                );
                let ty = self.hir_var_map[&hirvar].ty.body.clone();
                if ty.is_inference() {
                    dbg!(&self.defs.defs[&self.at_item].inner.span);
                    dbg!(&self.hir_var_map);
                    panic!("oops.");
                }
                remaps.push((hirassign.cur_var, newvar));
                incoming_vars.push((newvar, ty));
            }
        }

        (
            MirJumpInfo {
                targbb: bb_id,
                remaps,
                fallthrough: false,
            },
            assignments,
            incoming_vars,
        )
    }

    fn make_many_jumps(
        &mut self,
        assign_sets: &[HashMap<HirVarId, HirVarAssignment>],
        targbb: BasicBlockId,
    ) -> (
        Vec<(Vec<MirStatement>, MirJumpInfo)>,
        HashMap<HirVarId, HirVarAssignment>,
        Vec<(SsaVarId, Type)>,
    ) {
        let mut gather_set = HashSet::<HirVarId>::new();
        let mut placement = HashMap::<HirVarId, SsaVarKind>::new();

        for assign_set in assign_sets {
            gather_set.extend(assign_set.keys().copied());

            assign_set.iter().for_each(|&Pair(key, assign)| {
                match (
                    placement.get_or_insert_mut(key, assign.var_kind),
                    assign.var_kind,
                ) {
                    (val @ SsaVarKind::Register, SsaVarKind::Alloca) => *val = SsaVarKind::Alloca,
                    _ => {}
                }
            });
        }

        for assign_set in assign_sets {
            gather_set.retain(|k| assign_set.contains_key(k));
        }
        let mut var_order = vec![];
        let mut incoming = vec![];
        let mut new_assignments = HashMap::new();

        for var in gather_set {
            var_order.push(var);
            let ty = self.hir_var_map[&var].ty.body.clone();
            let new_var = SsaVarId(self.nextvar.fetch_increment());
            incoming.push((new_var, ty));
            let assign = HirVarAssignment {
                cur_var: new_var,
                var_kind: placement[&var],
            };
            new_assignments.insert(var, assign);
        }

        let mut jump_map = Vec::new();

        for assign_set in assign_sets {
            let mut remaps = Vec::new();
            let mut reinit_stat = Vec::new();
            for &var in &var_order {
                let assign = assign_set[&var];
                let new_var = new_assignments[&var].cur_var;
                if assign.var_kind != placement[&var] {
                    let ThirVarDef { ty, mt, .. } = self.hir_var_map[&var].clone();
                    let mut var = SsaVarId(self.nextvar.fetch_increment());
                    reinit_stat.push(MirStatement::Declare {
                        var: synthetic(var),
                        ty: ty.clone(),
                        init: synthetic(MirExpr::Alloca(
                            *mt,
                            ty.body,
                            Box::new(synthetic(MirExpr::Var(assign.cur_var))),
                        )),
                    });
                    remaps.push((var, new_var));
                } else {
                    remaps.push((assign.cur_var, new_var));
                }
            }

            jump_map.push((
                reinit_stat,
                MirJumpInfo {
                    targbb,
                    remaps,
                    fallthrough: false,
                },
            ))
        }

        (jump_map, new_assignments, incoming)
    }

    pub fn write_block(&mut self, thir_block: Spanned<ThirBlock>) -> super::Result<()> {
        if self.cur_basic_block.id == BasicBlockId::UNUSED {
            return Ok(());
        }
        match thir_block.body {
            super::tyck::ThirBlock::Normal(stmts) | super::tyck::ThirBlock::Unsafe(stmts) => {
                let newbb = BasicBlockId(self.nextbb.fetch_increment());
                let (jmp, assignments, incoming) = self.make_jump(newbb);
                let term = MirTerminator::Jump(jmp);

                let bb = self.cur_basic_block.finish_and_reset(Spanned {
                    body: term,
                    span: thir_block.span,
                });
                self.basic_blocks.push(bb);
                self.cur_basic_block.id = newbb;
                self.cur_basic_block.incoming_vars = incoming;
                self.var_names = assignments;
                for stmt in stmts {
                    self.write_statement(stmt)?;
                }

                if self.cur_basic_block.id == BasicBlockId::UNUSED {
                    return Ok(());
                }

                let newbb = BasicBlockId(self.nextbb.fetch_increment());
                let (jmp, assignments, incoming) = self.make_jump(newbb);

                let term = MirTerminator::Jump(jmp);

                let bb = self.cur_basic_block.finish_and_reset(Spanned {
                    body: term,
                    span: thir_block.span,
                });
                self.basic_blocks.push(bb);

                self.cur_basic_block.id = newbb;
                self.cur_basic_block.incoming_vars = incoming;
                self.var_names = assignments;
            }
            super::tyck::ThirBlock::Loop(stmts) => {
                let newbb = BasicBlockId(self.nextbb.fetch_increment());
                let (jmp, assignments, incoming) = self.make_jump(newbb);
                let term = MirTerminator::Jump(jmp);

                let bb = self.cur_basic_block.finish_and_reset(Spanned {
                    body: term.clone(),
                    span: thir_block.span,
                });
                self.basic_blocks.push(bb);
                self.cur_basic_block.id = newbb;
                self.var_names = assignments;
                for stmt in stmts {
                    self.write_statement(stmt)?;
                }

                if self.cur_basic_block.id == BasicBlockId::UNUSED {
                    return Ok(());
                }

                let bb = self.cur_basic_block.finish_and_reset(Spanned {
                    body: term,
                    span: thir_block.span,
                });
                self.basic_blocks.push(bb);
            }
            super::tyck::ThirBlock::If {
                cond,
                block,
                elseblock,
            } => {
                let cond = self.lower_expr(cond)?;
                let ifbb = BasicBlockId(self.nextbb.fetch_increment());

                let (ifjmp, assignments, incoming) = self.make_jump(ifbb);
                let mut start_block =
                    core::mem::replace(&mut self.cur_basic_block, UnbuiltBasicBlock::unused());
                self.cur_basic_block.id = ifbb;
                self.cur_basic_block.incoming_vars = incoming;
                let mut cur_assigns = core::mem::replace(&mut self.var_names, assignments);

                self.write_block(*block)?;

                let mut if_end_block =
                    core::mem::replace(&mut self.cur_basic_block, UnbuiltBasicBlock::unused());

                let elsebb = BasicBlockId(self.nextbb.fetch_increment());

                let if_assigns = core::mem::replace(&mut self.var_names, cur_assigns);

                let (elsejmp, assignments, incoming) = self.make_jump(elsebb);
                self.cur_basic_block.id = elsebb;
                self.cur_basic_block.incoming_vars = incoming;
                cur_assigns = core::mem::replace(&mut self.var_names, assignments);

                self.basic_blocks.push(start_block.finish(Spanned {
                    body: MirTerminator::Branch(MirBranchInfo {
                        cond: Box::new(cond),
                        if_block: ifjmp,
                        else_block: elsejmp,
                    }),
                    span: thir_block.span,
                }));

                if let Some(elseblock) = elseblock {
                    self.write_block(*elseblock)?;
                }
                let nextbb = BasicBlockId(self.nextbb.fetch_increment());

                let else_assigns = core::mem::take(&mut self.var_names);

                let (mut muljmps, assigns, incoming) =
                    self.make_many_jumps(&[if_assigns, else_assigns], nextbb);
                let mut diverges = true;
                if if_end_block.id != BasicBlockId::UNUSED {
                    let (ifinitstats, ifjmp) = core::mem::take(&mut muljmps[0]);
                    if_end_block
                        .stmts
                        .extend(ifinitstats.into_iter().map(|stmt| Spanned {
                            body: stmt,
                            span: thir_block.span,
                        }));
                    self.basic_blocks.push(if_end_block.finish(Spanned {
                        body: MirTerminator::Jump(ifjmp),
                        span: thir_block.span,
                    }));
                    diverges = false;
                }
                if self.cur_basic_block.id != BasicBlockId::UNUSED {
                    let (elseinitstats, elsejmp) = core::mem::take(&mut muljmps[1]);
                    self.cur_basic_block
                        .stmts
                        .extend(elseinitstats.into_iter().map(|stmt| Spanned {
                            body: stmt,
                            span: thir_block.span,
                        }));
                    self.basic_blocks
                        .push(self.cur_basic_block.finish_and_reset(Spanned {
                            body: MirTerminator::Jump(elsejmp),
                            span: thir_block.span,
                        }));
                    diverges = false;
                }
                if !diverges {
                    self.cur_basic_block.id = nextbb;
                    self.cur_basic_block.incoming_vars = incoming;
                    self.var_names = assigns;
                }
            }
            super::tyck::ThirBlock::Match(m) => {
                let ty = m.discriminee.ty.clone();
                let (discriminee, pat_var_kind, pat_var) =
                    self.lower_register(m.discriminee, false)?;
                let assignments = self.var_names.clone();
                let mut end_blocks = Vec::new();
                let mut match_builder = match_lower::MatchBuilder::new(self.defs);
                let mut end_assign_set = Vec::new();
                let mut arm_jumps = Vec::new();
                let mut cur_bb =
                    core::mem::replace(&mut self.cur_basic_block, UnbuiltBasicBlock::unused());
                for arm in m.arms {
                    match_builder.write_arm(arm.discrim.body.matcher)?;

                    if let Some(guard) = arm.guard {
                        todo!("match guard")
                    }
                    let newbb = BasicBlockId(self.nextbb.fetch_increment());

                    let (mut jmp, assigns, incoming) = self.make_jump(newbb);

                    self.cur_basic_block.id = newbb;
                    self.cur_basic_block.incoming_vars = incoming;

                    self.var_names = assigns;

                    let mut binding_var = None;

                    for binding in arm.discrim.body.bindings {
                        let mut binding_var = if let Some(binding_var) = binding_var {
                            binding_var
                        } else {
                            for &(mapped, to) in &jmp.remaps {
                                if mapped == pat_var {
                                    binding_var = Some(to);
                                    break;
                                }
                            }
                            if let Some(binding_var) = binding_var {
                                binding_var
                            } else {
                                let var = SsaVarId(self.nextvar.fetch_increment());
                                self.cur_basic_block.incoming_vars.push((var, ty.clone()));
                                jmp.remaps.push((pat_var, var));
                                binding_var = Some(var);
                                var
                            }
                        };

                        if binding.path.elements.len() != 0 {
                            let real_binding_var = SsaVarId(self.nextvar.fetch_increment());
                            let mut init_expr = Spanned {
                                body: MirExpr::Var(binding_var),
                                span: binding.span,
                            };
                            for elemen in binding.body.path.elements {
                                match elemen {
                                    crate::sema::hir::PatternPathFragment::Field(field) => {
                                        match pat_var_kind {
                                            SsaVarKind::Register => {
                                                init_expr = Spanned {
                                                    body: MirExpr::GetSubobject(
                                                        Box::new(init_expr),
                                                        field,
                                                    ),
                                                    span: binding.span,
                                                }
                                            }
                                            SsaVarKind::Alloca => {
                                                init_expr = Spanned {
                                                    body: MirExpr::FieldProject(
                                                        Box::new(init_expr),
                                                        field,
                                                    ),
                                                    span: binding.span,
                                                }
                                            }
                                        }
                                    }
                                    crate::sema::hir::PatternPathFragment::SliceElement(_) => {
                                        todo!("slice element")
                                    }
                                    crate::sema::hir::PatternPathFragment::SliceHead(_) => {
                                        todo!("slice head")
                                    }
                                    crate::sema::hir::PatternPathFragment::SliceTail(_) => {
                                        todo!("slice tail")
                                    }
                                }
                            }
                            let mut ty = self.hir_var_map[&*binding.body.var].ty.clone();
                            if pat_var_kind == SsaVarKind::Alloca {
                                let mt = self.hir_var_map[&*binding.body.var].mt;
                                ty = Spanned {
                                    body: Type::Pointer(mt, Box::new(ty)),
                                    span: binding.span,
                                };
                            }

                            self.cur_basic_block.stmts.push(Spanned {
                                body: MirStatement::Declare {
                                    var: Spanned {
                                        body: real_binding_var,
                                        span: binding.span,
                                    },
                                    ty,
                                    init: init_expr,
                                },
                                span: binding.span,
                            });
                            binding_var = real_binding_var;
                        }

                        self.var_names.insert(
                            binding.body.var.body,
                            HirVarAssignment {
                                cur_var: binding_var,
                                var_kind: pat_var_kind,
                            },
                        );
                    }
                    arm_jumps.push(jmp);
                    for stmt in arm.expansion.body {
                        self.write_statement(stmt)?;
                    }
                    let end_assigns = core::mem::replace(&mut self.var_names, assignments.clone());
                    let end_block =
                        core::mem::replace(&mut self.cur_basic_block, UnbuiltBasicBlock::unused());

                    if end_block.id != BasicBlockId::UNUSED {
                        end_blocks.push(end_block);
                        end_assign_set.push(end_assigns);
                    }
                }

                let term = match_builder.build_match_term(discriminee, arm_jumps);

                self.basic_blocks.push(cur_bb.finish(Spanned {
                    body: term,
                    span: thir_block.span,
                }));

                if !end_blocks.is_empty() {
                    let end_block_id = BasicBlockId(self.nextbb.fetch_increment());
                    self.cur_basic_block.id = end_block_id;

                    let (jumps, assigns, incoming) =
                        self.make_many_jumps(&end_assign_set, end_block_id);
                    self.cur_basic_block.incoming_vars = incoming;

                    self.var_names = assigns;

                    for (mut end_block, (init_stats, jmp)) in end_blocks.into_iter().zip(jumps) {
                        let span = thir_block.span;
                        end_block.stmts.extend(
                            init_stats
                                .into_iter()
                                .map(|stat| Spanned { body: stat, span }),
                        );
                        self.basic_blocks.push(end_block.finish(Spanned {
                            body: MirTerminator::Jump(jmp),
                            span,
                        }));
                    }
                }
            }
        }

        Ok(())
    }

    pub fn write_statement(&mut self, thir_stat: Spanned<ThirStatement>) -> super::Result<()> {
        if self.cur_basic_block.id == BasicBlockId::UNUSED {
            return Ok(()); // short circuit out if we're unreachable
        }
        let span = thir_stat.span;

        match thir_stat.body {
            ThirStatement::Assign { dest, val, op } => {
                let val = self.lower_expr(val)?;
                self.lower_write(dest, val, op, span)?;
            }
            ThirStatement::Define { var, .. } => {
                self.hir_def_spans.insert(var.body, var.span);
            }
            ThirStatement::Return(expr) => {
                let expr = self.lower_expr(expr)?;

                let bb = self.cur_basic_block.finish_and_reset(Spanned {
                    body: MirTerminator::Return(expr),
                    span,
                });
                self.basic_blocks.push(bb);
            }
            ThirStatement::Block(blk) => self.write_block(blk)?,
            ThirStatement::Discard(expr) => {
                let expr = self.lower_expr(expr)?;

                self.cur_basic_block.stmts.push(Spanned {
                    body: MirStatement::Discard(expr),
                    span,
                });
            }
            ThirStatement::Call {
                retplace,
                fnexpr,
                method_name,
                params,
            } => {
                let fnty = match &fnexpr.ty {
                    Type::FnItem(fnty, _, _) | Type::FnPtr(fnty) => fnty.clone(),
                    _ => unreachable!(),
                };

                let targ = self.lower_expr(fnexpr)?;

                if let MirExpr::Intrinsic(intrin, _) = &targ.body {
                    match intrin {
                        IntrinsicDef::__builtin_unreachable => {
                            let bb = self.cur_basic_block.finish_and_reset(Spanned {
                                body: MirTerminator::Unreachable,
                                span,
                            });
                            self.basic_blocks.push(bb);
                            return Ok(());
                        }
                        IntrinsicDef::impl_id => {
                            if let Some(retplace) = retplace {
                                self.lower_write(
                                    retplace,
                                    Spanned {
                                        span,
                                        body: MirExpr::ConstString(
                                            StringType::Default,
                                            Symbol::intern(concat!(
                                                "lccc (",
                                                env!("CARGO_PKG_VERSION"),
                                                ")"
                                            )),
                                        ),
                                    },
                                    None,
                                    span,
                                )?;

                                return Ok(());
                            }
                        }
                        _ => {}
                    }
                }

                let params = params
                    .into_iter()
                    .map(|expr| self.lower_expr(expr))
                    .collect::<super::Result<Vec<_>>>()?;

                if fnty.retty.body == Type::Never {
                    // Always tailcall if `!`

                    let tailcall = MirTailcallInfo {
                        targ,
                        fnty,
                        params,
                        unwind: None,
                    };
                    let bb = self.cur_basic_block.finish_and_reset(Spanned {
                        body: MirTerminator::Tailcall(tailcall),
                        span,
                    });
                    self.basic_blocks.push(bb);
                } else {
                    let varspan = retplace
                        .as_ref()
                        .map(|s| s.span)
                        .unwrap_or(Span::synthetic());
                    let mut nextbb = BasicBlockId(self.nextbb.fetch_increment());
                    let (jmp, assignments, mut incoming) = self.make_jump(nextbb);

                    let retvar = SsaVarId(self.nextvar.fetch_increment());

                    incoming.push((retvar, fnty.retty.body.clone()));

                    let call = MirCallInfo {
                        retplace: Spanned {
                            body: retvar,
                            span: varspan,
                        },
                        targ,
                        fnty,
                        params,
                        next: jmp,
                        unwind: None,
                    };

                    let bb = self.cur_basic_block.finish_and_reset(Spanned {
                        body: MirTerminator::Call(call),
                        span,
                    });
                    self.basic_blocks.push(bb);
                    self.cur_basic_block.id = nextbb;
                    self.cur_basic_block.incoming_vars = incoming;
                    self.var_names = assignments;

                    if let Some(retplace) = retplace {
                        let mut retval = MirExpr::Var(retvar);
                        self.lower_write(
                            retplace,
                            Spanned {
                                body: retval,
                                span: varspan,
                            },
                            None,
                            span,
                        )?;
                        self.cur_basic_block.stmts.push(Spanned {
                            body: MirStatement::StoreDead(retvar),
                            span: varspan,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    pub fn finish(mut self) -> MirFunctionBody {
        self.basic_blocks.sort_by_key(|bb| bb.id.0);
        MirFunctionBody {
            bbs: self.basic_blocks,
            vardbg_info: self.mir_debug_map,
            localitems: self.localitems,
        }
    }
}
