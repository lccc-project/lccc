#[doc(hidden)]
pub use rust_mir_macro::{mir_impl as __mir_impl, mir_type_impl as __mir_type_impl};

#[macro_export]
macro_rules! mir{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_impl!([$crate] $($tt)*)
    }
}

pub use mir;

#[macro_export]
macro_rules! mir_type{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_type_impl!([$crate] $($tt)*)
    }
}

pub use mir_type;
