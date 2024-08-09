#[doc(hidden)]
pub use rust_mir_macro::{
    mir_basic_block_impl as __mir_basic_block_impl, mir_expr_impl as __mir_expr_impl,
    mir_fnty_impl as __mir_fnty_impl, mir_impl as __mir_impl, mir_stmt_impl as __mir_stmt_impl,
    mir_term_impl as __mir_term_impl, mir_type_impl as __mir_type_impl,
};

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

#[macro_export]
macro_rules! mir_expr{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_expr_impl!([$crate] $($tt)*)
    }
}

pub use mir_expr;

#[macro_export]
macro_rules! mir_fnty{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_fnty_impl!([$crate] $($tt)*)
    }
}

pub use mir_fnty;

#[macro_export]
macro_rules! mir_term{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_term_impl!([$crate] $($tt)*)
    }
}

pub use mir_term;

#[macro_export]
macro_rules! mir_stmt{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_stmt_impl!([$crate] $($tt)*)
    }
}

pub use mir_stmt;

#[macro_export]
macro_rules! mir_basic_block{
    ($($tt:tt)*) => {
        $crate::sema::mir::mir_macro::__mir_basic_block_impl!([$crate] $($tt)*)
    }
}

pub use mir_basic_block;
