use xlang_abi::span::Span;

#[doc(hidden)]
pub use xlang_abi::span as __span;

/// A type that is usable in an architeacture builtin function
#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinType<'a> {
    /// The void type `void()`
    Void,
    /// An integer type (signed or unsigned, also includes fixed-point and character types) of a given width
    Int(u16),
    /// A vector type (notwithstanding the element type or width) of a given size (in bytes)
    Vec(i16),
    /// A vector type of integer elements with the given element width and number of elements
    VecInt {
        /// The width of the scalar element type
        scalar_width: u16,
        /// The number of elements in the vector
        elements: u16,
    },
    /// A floating-point type of a given width
    Float(u16),
    /// A vector type of floating-point elements with the given element width and number of elements
    VecFloat {
        /// The width of the scalar element type
        scalar_width: u16,
        /// The number of elements in the vector
        elements: u16,
    },
    /// float(long)
    LongFloat,
    /// A pointer to some other builtin type
    Pointer(&'a BuiltinType<'a>),
}

/// A signature for an architecture builtin function
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct BuiltinSignature<'a> {
    /// The parameters in the signature
    pub params: Span<'a, BuiltinType<'a>>,
    /// The return type in the signature
    pub retty: BuiltinType<'a>,
}

/// Parses a builtin type from a subset of XIR type syntax.
/// It also accepts `vec(n)` as a vector `n` bytes long
#[macro_export]
macro_rules! builtin_type{
    (void $(())?) => {$crate::properties::builtins::BuiltinType::Void};
    (int ($scalar_width:literal)) => {
        $crate::properties::builtins::BuiltinType::Int($scalar_width)
    };
    (int vectorsize($elements:literal) ($scalar_width:literal)) => {
        $crate::properties::builtins::BuiltinType::VecInt{scalar_wdith: $scalar_width, elements: $elements}
    };
    (vec($vec_bytes:literal)) => {
        $crate::properties::builtins::BuiltinType::Vec($vec_bytes)
    };
    (float ($scalar_width:literal)) => {
        $crate::properties::builtins::BuiltinType::Float($scalar_width)
    };
    (float(long)) => {
        $crate::properties::builtins::BuiltinType::LongFloat
    };
    (float vectorsize($elements:literal) ($scalar_width:literal)) => {
        $crate::properties::builtins::BuiltinType::VecFloat{scalar_wdith: $scalar_width, elements: $elements}
    };
    (*$($tt:tt)+) => {
        $crate::properties::builtins::BuiltinType::Pointer(&$crate::builtin_type!($($tt)+))
    }
}
/// Generates a valid [`BuiltinSignature`] from a signature like (params...)->ret
#[macro_export]
macro_rules! builtin_signature{
    (($($(* $(@@$ignore:ident@@)?)* $primary_name:ident $($secondary_name:ident ($($secondary_group:tt)*))? $(($($primary_group:tt)*))?),* $(,)?)->$($ret:tt)+) => {
        $crate::properties::builtins::BuiltinSignature{
            params: $crate::properties::builtins::__span![$($crate::builtin_type!($(* $(@@$ignore@@)?)* $primary_name $($secondary_name ($($secondary_group)*))? $(($($primary_group)+))?)),*],
            retty: $crate::builtin_type!($($ret)+)
        }
    };
    (($($(* $(@@$ignore:ident@@)?)* $primary_name:ident $($secondary_name:ident ($($secondary_group:tt)*))? $(($($primary_group:tt)*))?),* $(,)?)) => {
        $crate::properties::builtins::BuiltinSignature{
            params: $crate::properties::builtins::__span![$($crate::builtin_type!($(* $(@@$ignore@@)?)* $primary_name $($secondary_name ($($secondary_group)*))? $(($($primary_group)+))?)),*],
            retty: $crate::properties::builtins::BuiltinType::Void
        }
    }
}
