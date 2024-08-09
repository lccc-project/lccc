use xlang::{
    abi::{collection::HashMap, pair::Pair},
    ir::{ArrayType, CompareOp, Path, PointerType, ScalarType, Type, Value},
};

use core::{fmt::Debug, hash::Hash};
use std::num::NonZeroU128;

/// Represents the location of opaque values both as locals and on the value stack
pub trait ValLocation: Eq + Debug + Clone {
    /// Checks if this location is addressable (is not a register)
    fn addressible(&self) -> bool;

    /// Returns the type of the value location
    fn val_type(&self) -> &Type;
}

/// The pointee of a pointer
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LValue<Loc: ValLocation> {
    /// An LValue from an Opaque pointer stored in `Loc`
    OpaquePointer(Loc),
    /// A pointer to a local variable
    Local(u32),
    /// A pointer to a global static/function
    GlobalAddress(Path),
    /// A pointer to a Label
    Label(u32),
    /// Aggregate Element Field
    Field(Type, Box<LValue<Loc>>, String),

    /// Offset (in bytes) to some other lvalue
    Offset(Box<LValue<Loc>>, i64),

    /// A Null pointer
    Null,

    /// A raw Address
    TransparentAddr(NonZeroU128),
}

impl<Loc: ValLocation> core::fmt::Display for LValue<Loc> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            LValue::OpaquePointer(loc) => f.write_fmt(format_args!("opaque({:?})", loc)),
            LValue::Local(n) => f.write_fmt(format_args!("_{}", n)),
            LValue::GlobalAddress(path) => f.write_fmt(format_args!("global_addr({})", path)),
            LValue::Label(n) => f.write_fmt(format_args!("&&@{}", n)),
            LValue::Field(ty, lval, name) => {
                f.write_fmt(format_args!("{}.({})::{}", lval, ty, name))
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
    CompareResult(CompareOp, ScalarType, Loc, Loc),

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
            VStackValue::CompareResult(op, ty, l, r) => {
                f.write_fmt(format_args!("{} result ({:?},{:?}): {}", op, l, r, ty))
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

    /// Creates an opaque value corresponding to the specified [`Type`]
    pub fn opaque_value(ty: Type, loc: Loc) -> Self {
        match ty {
            Type::Void | Type::FnType(_) | Type::Null => {
                panic!("opaque_value requires a complete value type")
            }
            Type::Scalar(sty) => Self::OpaqueScalar(sty, loc),
            Type::Aligned(_, ty) | Type::TaggedType(_, ty) => {
                Self::opaque_value(xlang::abi::boxed::Box::into_inner(ty), loc)
            }
            Type::Pointer(pty) => Self::Pointer(pty, LValue::OpaquePointer(loc)),
            ty => VStackValue::OpaqueAggregate(ty, loc),
        }
    }

    /// Creates an opaque [`VStackValue::LValue`] of the specified [`Type`]
    pub fn opaque_lvalue(ty: Type, loc: Loc) -> Self {
        Self::LValue(ty, LValue::OpaquePointer(loc))
    }

    /// Obtains the type of the value
    pub fn value_type(&self) -> Type {
        match self {
            VStackValue::Constant(val) => match val {
                Value::Invalid(ty) | Value::Uninitialized(ty) => ty.clone(),
                Value::GenericParameter(_) => panic!("Cannot handle generic params this late"),
                Value::Integer { ty, .. } => Type::Scalar(*ty),
                Value::GlobalAddress { ty, .. } => {
                    let mut pty = PointerType::default();
                    *pty.inner = ty.clone();
                    Type::Pointer(pty)
                }
                Value::ByteString { .. } => todo!("what type is byte string constant again?"),
                Value::String { ty, .. } => ty.clone(),
                Value::LabelAddress(_) => {
                    let mut pty = PointerType::default();
                    *pty.inner = Type::Void;
                    Type::Pointer(pty)
                }
                Value::Empty => Type::Null,
            },
            VStackValue::LValue(ty, _) => ty.clone(),
            VStackValue::Pointer(ptrty, _) => Type::Pointer(ptrty.clone()),
            VStackValue::OpaqueScalar(scalar, _) => Type::Scalar(*scalar),
            VStackValue::AggregatePieced(ty, _) => ty.clone(),
            VStackValue::OpaqueAggregate(ty, _) => ty.clone(),
            VStackValue::CompareResult(_, sty, _, _) => Type::Scalar(*sty),
            VStackValue::Trapped => Type::Null,
            VStackValue::ArrayRepeat(val, len) => {
                Type::Array(xlang::abi::boxed::Box::new(ArrayType {
                    ty: val.value_type(),
                    len: len.clone(),
                }))
            }
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

impl core::fmt::Display for Trap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trap::Unreachable => f.write_str("unreachable"),
            Trap::Breakpoint => f.write_str("breakpoint"),
            Trap::Abort => f.write_str("abort"),
            Trap::Overflow => f.write_str("overflow"),
        }
    }
}

/// A ValLocation that cannot be instantiated - that is, no opaque values can be produced within this location
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NoOpaque {}

impl ValLocation for NoOpaque {
    fn addressible(&self) -> bool {
        match *self {}
    }

    fn val_type(&self) -> &Type {
        match *self {}
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
            VStackValue::CompareResult(_, _, left, _) => match left {},
            VStackValue::Trapped => VStackValue::Trapped,
            VStackValue::ArrayRepeat(val, count) => {
                VStackValue::ArrayRepeat(Box::new((*val).into_transparent_for()), count)
            }
        }
    }
}

impl LValue<NoOpaque> {
    /// Converts a known-transparent [`LValue`] into one that is suitable for some other [`ValLocation`]
    pub fn into_transparent_for<T: ValLocation>(self) -> LValue<T> {
        match self {
            LValue::OpaquePointer(val) => match val {},
            LValue::Local(n) => LValue::Local(n),
            LValue::GlobalAddress(path) => LValue::GlobalAddress(path),
            LValue::Label(n) => LValue::Label(n),
            LValue::Field(ty, base, field) => {
                LValue::Field(ty, Box::new((*base).into_transparent_for()), field)
            }
            LValue::Offset(base, bytes) => {
                LValue::Offset(Box::new((*base).into_transparent_for()), bytes)
            }
            LValue::Null => LValue::Null,
            LValue::TransparentAddr(addr) => LValue::TransparentAddr(addr),
        }
    }
}
