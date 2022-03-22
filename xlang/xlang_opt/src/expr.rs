use xlang::{
    ir::{Path, PointerType, ScalarType, StringEncoding, Type, Value},
    prelude::v1::HashMap,
};

use crate::provenance::Provenance;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum VStackItem {
    Const(Value),
    LValue(Type, LValue),
    Pointer(PointerType, LValue),
    OpaqueScalar(ScalarType),
    OpaqueAggregate(Type),
    AggregatePieced(Type, HashMap<String, VStackItem>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LValue {
    GlobalAddress(Path),
    Local(u32),
    Label(u32),
    String(StringEncoding, String),
    ByteString(Vec<u8>),
    OpaquePointer(Option<u32>),
    Field(Box<LValue>, String),
    Offset(Box<LValue>, Box<VStackItem>),
    Null,
    Unify(Box<LValue>, Box<LValue>),
    Reduce(Box<LValue>),
    Link(Box<LValue>, Provenance),
}
