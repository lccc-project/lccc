use xlang::{
    ir::{Path, PointerType, ScalarType, Type, Value},
    prelude::v1::*,
};

use core::{fmt::Debug, hash::Hash};

use crate::str::Encoding;

/// Represents the location of opaque values both as locals and on the value stack
pub trait ValLocation: Eq + Hash + Debug + Clone {
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
    Local(Loc),
    /// A pointer to a global static/function
    GlobalAddress(Path),
    /// A pointer to a Label
    Label(u32),
    /// Aggregate Element Field
    Field(Type, Box<LValue<Loc>>, String),
    /// A pointer to a string literal
    StringLiteral(Encoding, Vec<u8>),
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

    /// Placeholder for a value that's already caused a `ud2`
    Trapped,
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
