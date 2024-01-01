use std::{rc::Rc, sync::Arc};

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

    /// Determines whether tailcall is possible

    fn can_tail(&self, fnty: &FnType, base_ty: &FnType) -> bool {
        self.pass_return_place(&fnty.ret).is_some()
            || !self.pass_return_place(&base_ty.ret).is_some()
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for &C {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for &mut C {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for Box<C> {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for xlang::abi::boxed::Box<C> {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for Rc<C> {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}

impl<C: CallingConvention + ?Sized> CallingConvention for Arc<C> {
    type Loc = C::Loc;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        C::pass_return_place(self, ty)
    }

    fn find_param(&self, fnty: &FnType, real: &FnType, param: u32, infn: bool) -> Self::Loc {
        C::find_param(self, fnty, real, param, infn)
    }

    fn find_return_val(&self, fnty: &FnType) -> Self::Loc {
        C::find_return_val(self, fnty)
    }
}
