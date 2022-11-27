use xlang::{
    ir::{ArrayType, Path, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type, Value},
    prelude::v1::*,
};

use core::{fmt::Debug, hash::Hash};
use std::num::NonZeroU128;

use crate::str::Encoding;

/// Represents the location of opaque values both as locals and on the value stack
pub trait ValLocation: Eq + Debug + Clone {
    /// Checks if this location is addressable (is not a register)
    fn addressible(&self) -> bool;

    /// Gets an unassigned location, used by [`super::FunctionCodegen`] to keep track of values before asking the raw codegen to assign locations.
    fn unassigned(n: usize) -> Self;
}

/// The pointee of a pointer
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LValue<Loc: ValLocation> {
    /// An LValue from an Opaque pointer stored in `Loc`
    OpaquePointer(Loc),
    /// A pointer to a temporary value (`as_temporary`)
    Temporary(Box<VStackValue<Loc>>),
    /// A pointer to a local variable
    Local(u32),
    /// A pointer to a global static/function
    GlobalAddress(Path),
    /// A pointer to a Label
    Label(u32),
    /// Aggregate Element Field
    Field(Type, Box<LValue<Loc>>, String),
    /// A pointer to a string literal
    StringLiteral(Encoding, Vec<u8>),

    /// Offset (in bytes) to some other lvalue
    Offset(Box<LValue<Loc>>, u64),

    /// A Null pointer
    Null,

    /// A raw Address
    TransparentAddr(NonZeroU128),
}

impl<Loc: ValLocation> core::fmt::Display for LValue<Loc> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            LValue::OpaquePointer(loc) => f.write_fmt(format_args!("opaque({:?})", loc)),
            LValue::Temporary(val) => f.write_fmt(format_args!("temporary({})", val)),
            LValue::Local(n) => f.write_fmt(format_args!("_{}", n)),
            LValue::GlobalAddress(path) => f.write_fmt(format_args!("global_addr({})", path)),
            LValue::Label(n) => f.write_fmt(format_args!("&&@{}", n)),
            LValue::Field(ty, lval, name) => {
                f.write_fmt(format_args!("{}.({})::{}", lval, ty, name))
            }
            LValue::StringLiteral(enc, bytes) => {
                core::fmt::Display::fmt(enc, f)?;
                f.write_str(" ")?;
                match core::str::from_utf8(bytes) {
                    Ok(s) => f.write_fmt(format_args!(" \"{}\"", s.escape_default())),
                    Err(mut e) => {
                        let mut bytes = &bytes[..];
                        f.write_str(" \"")?;
                        while !bytes.is_empty() {
                            let (l, r) = bytes.split_at(e.valid_up_to());
                            core::fmt::Display::fmt(
                                &core::str::from_utf8(l).unwrap().escape_default(),
                                f,
                            )?;
                            if let core::option::Option::Some(len) = e.error_len() {
                                let (ebytes, rest) = r.split_at(len);
                                bytes = rest;
                                for b in ebytes {
                                    f.write_fmt(format_args!("\\x{:02x}", b))?;
                                }
                            } else {
                                let ebytes = core::mem::take(&mut bytes);
                                for b in ebytes {
                                    f.write_fmt(format_args!("\\x{:02x}", b))?;
                                }
                            }
                            match core::str::from_utf8(bytes) {
                                Ok(s) => {
                                    core::fmt::Display::fmt(&s.escape_default(), f)?;
                                    break;
                                }
                                Err(next_err) => e = next_err,
                            }
                        }
                        f.write_str("\"")
                    }
                }
            }
            LValue::Offset(loc, off) => f.write_fmt(format_args!("{}+{}", loc, off)),
            LValue::Null => f.write_str("null"),
            LValue::TransparentAddr(addr) => f.write_fmt(format_args!("{:#x}", addr)),
        }
    }
}

/// Represents a value on the stack for codegen
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum VStackValue<Loc: ValLocation> {
    /// Represents a Constant (statically known) value
    Constant(Value),
    /// Represents an LValue that can be read from/written to
    LValue(Type, LValue<Loc>),
    /// Represents a pointer to the given LValue
    Pointer(PointerType, LValue<Loc>),
    /// Represents a scalar value which is stored in memory
    OpaqueScalar(ScalarType, Loc),

    /// Represents an aggregate type that is stored piecewise
    AggregatePieced(Type, HashMap<String, VStackValue<Loc>>),

    /// Represents an aggregate which is stored in memory
    OpaqueAggregate(Type, Loc),

    /// The result of the `cmp` instruction
    CompareResult(Box<VStackValue<Loc>>, Box<VStackValue<Loc>>),

    /// Placeholder for a value that's already caused a [`Trap`]
    Trapped,

    /// Array repeat constant
    ArrayRepeat(Box<VStackValue<Loc>>, Value),
}

impl<Loc: ValLocation> core::fmt::Display for VStackValue<Loc> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            VStackValue::Constant(val) => f.write_fmt(format_args!("const {}", val)),
            VStackValue::LValue(ty, lval) => f.write_fmt(format_args!("lvalue {} => {}", ty, lval)),
            VStackValue::Pointer(pty, lval) => f.write_fmt(format_args!("{} => {}", pty, lval)),
            VStackValue::OpaqueScalar(ty, loc) => {
                f.write_fmt(format_args!("{} (opaque({:?})", ty, loc))
            }
            VStackValue::AggregatePieced(ty, fields) => {
                core::fmt::Display::fmt(ty, f)?;
                f.write_str(" {")?;

                let mut fields = fields.iter();

                if let core::option::Option::Some(first) = fields.next() {
                    f.write_fmt(format_args!("{}: {}", first.0, first.1))?;
                }

                for Pair(name, val) in fields {
                    f.write_fmt(format_args!(", {}: {}", name, val))?;
                }
                f.write_str("}")
            }
            VStackValue::OpaqueAggregate(ty, loc) => {
                f.write_fmt(format_args!("{} (opaque({:?}))", ty, loc))
            }
            VStackValue::CompareResult(l, r) => {
                f.write_fmt(format_args!("cmp result ({},{})", l, r))
            }
            VStackValue::Trapped => f.write_str("trapped"),
            VStackValue::ArrayRepeat(value, count) => {
                f.write_fmt(format_args!("[{};{}]", value, count))
            }
        }
    }
}

impl<Loc: ValLocation> VStackValue<Loc> {
    /// If the value is opaque, returns the location the value is present in
    /// Otherwise returns [`None`]
    pub fn opaque_location(&self) -> Option<&Loc> {
        match self {
            VStackValue::LValue(_, LValue::OpaquePointer(loc))
            | VStackValue::Pointer(_, LValue::OpaquePointer(loc))
            | VStackValue::OpaqueAggregate(_, loc)
            | VStackValue::OpaqueScalar(_, loc) => Some(loc),
            _ => None,
        }
    }

    /// Obtains the type of the value
    pub fn value_type(&self) -> Type {
        match self {
            VStackValue::Constant(_) => todo!(),
            VStackValue::LValue(ty, _) => ty.clone(),
            VStackValue::Pointer(ptrty, _) => Type::Pointer(ptrty.clone()),
            VStackValue::OpaqueScalar(scalar, _) => Type::Scalar(*scalar),
            VStackValue::AggregatePieced(ty, _) => ty.clone(),
            VStackValue::OpaqueAggregate(ty, _) => ty.clone(),
            VStackValue::CompareResult(_, _) => Type::Scalar(ScalarType {
                header: ScalarTypeHeader {
                    bitsize: 32,
                    ..Default::default()
                },
                kind: ScalarTypeKind::Integer {
                    signed: false,
                    min: None,
                    max: None,
                },
            }),
            VStackValue::Trapped => Type::Void,
            VStackValue::ArrayRepeat(val, len) => Type::Array(Box::new(ArrayType {
                ty: val.value_type(),
                len: len.clone(),
            })),
        }
    }
}

/// Enum for Trap conditions, generated in certain places. May be no-ops in some cases
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Trap {
    /// Unreachable Code
    Unreachable,
    /// Breakpoint
    Breakpoint,
    /// Abort
    Abort,
    /// Overflow
    Overflow,
}

/// A ValLocation that cannot be instantiated - that is, no opaque values can be produced within this location
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NoOpaque {}

impl ValLocation for NoOpaque {
    fn addressible(&self) -> bool {
        match *self {}
    }

    fn unassigned(_: usize) -> Self {
        panic!("Unassigned location")
    }
}

impl VStackValue<NoOpaque> {
    /// Converts a known-transparent [`VStackValue`] into one that is suitable for some other [`ValLocation`]
    pub fn into_transparent_for<T: ValLocation>(self) -> VStackValue<T> {
        match self {
            VStackValue::Constant(v) => VStackValue::Constant(v),
            VStackValue::LValue(ty, lval) => VStackValue::LValue(ty, lval.into_transparent_for()),
            VStackValue::Pointer(pty, lval) => {
                VStackValue::Pointer(pty, lval.into_transparent_for())
            }
            VStackValue::OpaqueScalar(_, loc) => match loc {},
            VStackValue::AggregatePieced(ty, init) => VStackValue::AggregatePieced(
                ty,
                init.into_iter()
                    .map(|Pair(name, val)| Pair(name, val.into_transparent_for()))
                    .collect(),
            ),
            VStackValue::OpaqueAggregate(_, loc) => match loc {},
            VStackValue::CompareResult(left, right) => VStackValue::CompareResult(
                Box::new(Box::into_inner(left).into_transparent_for()),
                Box::new(Box::into_inner(right).into_transparent_for()),
            ),
            VStackValue::Trapped => VStackValue::Trapped,
            VStackValue::ArrayRepeat(val, count) => VStackValue::ArrayRepeat(
                Box::new(Box::into_inner(val).into_transparent_for()),
                count,
            ),
        }
    }
}

impl LValue<NoOpaque> {
    /// Converts a known-transparent [`LValue`] into one that is suitable for some other [`ValLocation`]
    pub fn into_transparent_for<T: ValLocation>(self) -> LValue<T> {
        match self {
            LValue::OpaquePointer(val) => match val {},
            LValue::Temporary(val) => {
                LValue::Temporary(Box::new(Box::into_inner(val).into_transparent_for()))
            }
            LValue::Local(n) => LValue::Local(n),
            LValue::GlobalAddress(path) => LValue::GlobalAddress(path),
            LValue::Label(n) => LValue::Label(n),
            LValue::Field(ty, base, field) => LValue::Field(
                ty,
                Box::new(Box::into_inner(base).into_transparent_for()),
                field,
            ),
            LValue::StringLiteral(enc, bytes) => LValue::StringLiteral(enc, bytes),
            LValue::Offset(base, bytes) => LValue::Offset(
                Box::new(Box::into_inner(base).into_transparent_for()),
                bytes,
            ),
            LValue::Null => LValue::Null,
            LValue::TransparentAddr(addr) => LValue::TransparentAddr(addr),
        }
    }
}
