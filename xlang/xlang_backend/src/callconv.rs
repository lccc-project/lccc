use xlang::ir::{FnType, Type};

use crate::expr::ValLocation;

/// Represents a calling convention, which can
pub trait CallingConvention {
    /// The type for locations that the Calling Convention stores values
    type Loc: ValLocation;

    /// Given a function type, obtains the list of parameter value locations according to the signature of `fnty`.
    /// `fnty` is the type of the callable, and `params` is the list of parameter types the function is actually called with (note that these may differ, e.g. where varargs is used)
    ///
    fn param_locations(&self, fnty: &FnType, params: &[Type]) -> Vec<Self::Loc>;

    /// If a given type is passed in a return place, then obtain the location to store that place.
    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc>;
}
