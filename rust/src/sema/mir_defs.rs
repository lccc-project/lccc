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
    Const(DefId, GenericArgs),
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirConstructor {
    pub ctor_def: DefId,
    pub fields: Vec<(FieldName, Spanned<MirExpr>)>,
    pub rest_init: Option<Box<Spanned<MirExpr>>>,
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum MirTerminator {
    Call(MirCallInfo),
    Tailcall(MirTailcallInfo),
    Return(Spanned<MirExpr>),
    Jump(MirJumpInfo),
    Unreachable,
    ResumeUnwind,
    DropInPlace(MirDropInfo),
    Branch(MirBranchInfo),
    SwitchInt(MirSwitchIntInfo),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirSwitchIntInfo {
    pub expr: Spanned<MirExpr>,
    pub ty: IntType,
    pub cases: Vec<(u128, MirJumpInfo)>,
    pub default: Option<MirJumpInfo>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirBranchInfo {
    pub conds: Vec<(Spanned<MirExpr>, MirJumpInfo)>,
    pub else_block: MirJumpInfo,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirJumpInfo {
    pub targbb: BasicBlockId,
    pub remaps: Vec<(SsaVarId, SsaVarId)>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirDropInfo {
    /// The target of the drop-in-place. Must be a pointer or reference type, and must dereference to a mutable place
    pub target: MirExpr,
    /// The drop flags point. If present, be a pointer to the `Type::DropFlags(ty)` type where `ty` is the type of `*target`
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct MirTailcallInfo {
    pub targ: Spanned<MirExpr>,
    pub fnty: Box<FnType>,
    pub params: Vec<Spanned<MirExpr>>,
    pub unwind: Option<MirJumpInfo>,
}
