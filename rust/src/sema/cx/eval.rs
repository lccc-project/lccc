use std::num::NonZeroU16;

use xlang::prelude::v1::HashMap;

use crate::{
    ast::Mutability,
    helpers::FetchIncrement,
    sema::{
        generics::GenericArgs,
        mir::{RefKind, SsaVarId},
        ty::{self, FieldName},
        DefId, Definitions,
    },
};

use super::{ConstEvalError, ConstExpr, Result};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct AllocId(u32);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PointerId(NonZeroU16);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CxEvalValue {
    Const(ConstExpr),
    LocalAddr(RefKind, Mutability, AllocId),
    Constructor(DefId, GenericArgs, HashMap<FieldName, CxEvalValue>),
    UnionBlob(DefId, Vec<CxEvalByte>),
    Tuple(Vec<CxEvalValue>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CxEvalByte {
    Uninit,
    Init(u8, Option<PointerId>),
}

pub struct CxPointer {
    alloc: AllocId,
}

pub struct Allocation {
    align: u64,
    bytes: Vec<CxEvalByte>,
}

pub struct MirEvaluator<'a> {
    allocs: HashMap<AllocId, Allocation>,
    pointers: HashMap<PointerId, CxPointer>,
    next_allocid: u32,
    next_pointerid: u16,
    defs: &'a Definitions,
}

#[inline(always)]
fn identity(val: &CxEvalByte) -> CxEvalByte {
    *val
}

#[inline]
fn freeze(val: &CxEvalByte) -> CxEvalByte {
    match val {
        // Choose 0xFF here - we could use nondeterminisim to try to defeat stuff, but I don't wanna, and
        // 0xFF bytes are most likely to hit most trivial validity invariants
        CxEvalByte::Uninit => CxEvalByte::Init(0xFF, None),
        b => *b,
    }
}

fn take<const N: usize>(
    alloc: &[CxEvalByte],
    transform: impl Fn(&CxEvalByte) -> CxEvalByte,
) -> Result<[CxEvalByte; N]> {
    let mut elems = [CxEvalByte::Uninit; N];

    for (ret, alloc_byte) in elems.iter_mut().zip(
        alloc
            .get(..N)
            .ok_or(ConstEvalError::UbError(super::UbType::OutOfBoundsAccess))?,
    ) {
        *ret = transform(alloc_byte);
    }

    Ok(elems)
}

impl<'a> MirEvaluator<'a> {
    pub fn new(defs: &'a Definitions) -> MirEvaluator {
        Self {
            allocs: HashMap::new(),
            pointers: HashMap::new(),
            next_allocid: 0,
            next_pointerid: 1,
            defs,
        }
    }

    pub fn read(
        &self,
        alloc: &[CxEvalByte],
        ty: &ty::Type,
        transform: impl Fn(&CxEvalByte) -> CxEvalByte,
    ) -> Result<CxEvalValue> {
        match ty {
            ty::Type::Bool => match take(alloc, transform)? {
                [CxEvalByte::Init(0, _)] => Ok(CxEvalValue::Const(ConstExpr::BoolConst(false))),
                [CxEvalByte::Init(1, _)] => Ok(CxEvalValue::Const(ConstExpr::BoolConst(true))),
                [b] => Err(ConstEvalError::UbError(super::UbType::ValidityCheckFailed)),
            },
            ty::Type::Int(int_type) => todo!(),
            ty::Type::Float(float_type) => todo!(),
            ty::Type::Char => todo!(),
            ty::Type::Str => todo!(),
            ty::Type::Never => todo!(),
            ty::Type::Tuple(vec) => todo!(),
            ty::Type::FnPtr(fn_type) => todo!(),
            ty::Type::FnItem(fn_type, def_id, generic_args) => todo!(),
            ty::Type::UserType(def_id, generic_args) => todo!(),
            ty::Type::UnresolvedLangItem(lang_item, generic_args) => todo!(),
            ty::Type::IncompleteAlias(def_id) => todo!(),
            ty::Type::Pointer(spanned, spanned1) => todo!(),
            ty::Type::Array(spanned, spanned1) => todo!(),
            ty::Type::Inferable(infer_id) => todo!(),
            ty::Type::InferableInt(infer_id) => todo!(),
            ty::Type::Reference(spanned, spanned1, spanned2) => todo!(),
            ty::Type::Param(param_id) => todo!(),
            ty::Type::TraitSelf(def_id) => todo!(),
            ty::Type::DropFlags(_) => todo!(),
        }
    }
}
