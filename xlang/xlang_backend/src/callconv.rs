use xlang::ir::{FnType, Type};

use crate::expr::ValLocation;

/// Represents a calling convention, which can
pub trait CallingConvention {
    /// The type for locations that the Calling Convention stores values
    type Loc: ValLocation;

    /// If a given type is passed in a return place, then obtain the location to store that place.
    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc>;

    /// Find the `n`th parameter inside or outside of the context of the function
    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc;

    /// Finds the return value location
    fn find_return_val(&self, fnty: &FnType) -> Self::Loc;
}
