/// The kind of a scalar type that can be given to an asm block
#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AsmScalarKind {
    /// Integer type (includes pointer, fixed point, and character types)
    Integer,
    /// Floating-point type (includes long double)
    Float,
    /// Vector type reguardless of component type
    Vector,
    /// Vector type (integer only)
    VectorInt,
    /// Vector type (floating-point only)
    VectorFloat,
    /// Clobbers only (Cannot be used in as an input or output constraint)
    ClobberOnly,
}

/// A Pair of an [`AsmScalarKind`] and a size (in bits) for the scalar type
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct AsmScalar(pub AsmScalarKind, pub u32);
