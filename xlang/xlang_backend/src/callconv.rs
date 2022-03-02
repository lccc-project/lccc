use xlang::ir::{FnType, Type};

use crate::expr::ValLocation;

/// Represents a calling convention, which can
pub trait CallingConvention {
    /// The type for locations that the Calling Convention stores values
    type Loc: ValLocation;

    /// If a given type is passed in a return place, then obtain the location to store that place.
    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc>;

    /// Find the `n`th parameter
    fn find_param(&self, fnty: &FnType, param: u32, infn: bool) -> Self::Loc;
}
