#![allow(missing_docs)]
use std::rc::Rc;

use xlang::ir::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaTerminator {
    Fallthrough(u32, Vec<OpaqueLocation>),
    Jump(u32, Vec<OpaqueLocation>),
    Exit(u16),
    Tailcall(CallTarget, Vec<OpaqueLocation>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallTarget {
    pub ptr: OpaquePtr,
    pub real_ty: FnType,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum OpaquePtr {
    Symbol(String),
    Pointer(OpaqueLocation),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OpaqueLocation {
    pub ty: Rc<Type>,
    pub num: u32,
}
