#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
#![allow(
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::missing_safety_doc
)] // FIXME: Remove this allow
pub mod option;
pub mod traits;

pub mod alloc;
pub mod boxed;
pub mod collection;
pub mod hash;
mod internal;
pub mod io;
pub mod pair;
pub mod prelude;
pub mod ptr;
pub mod result;
pub mod span;
pub mod string;
pub mod vec;

pub use xlang_abi_macro::xlang_trait;

#[macro_export]
#[cfg(not(rustcall_is_fastcall))]
macro_rules! rustcall{
    (extern "rustcall" fn $($tt:tt)*) => {
        extern "C" fn $($tt)*
    };
    (unsafe extern "rustcall" fn $($tt:tt)*) => {
        unsafe extern "C" fn $($tt)*
    };
    ($vis:vis extern "rustcall" fn $($tt:tt)*) => {
        $vis extern "C" fn $($tt)*
    };
    ($vis:vis unsafe extern "rustcall" fn $($tt:tt)*) => {
        $vis unsafe extern "C" fn $($tt)*
    };
    (extern "rustcall" { $($item:item)*}) => {
        extern "C" {
            $($item)*
        }
    }
}

#[macro_export]
#[cfg(rustcall_is_fastcall)]
macro_rules! rustcall{
    (extern "rustcall" fn $($tt:tt)*) => {
        extern "fastcall" fn $($tt)*
    };
    (unsafe extern "rustcall" fn $($tt:tt)*) => {
        unsafe extern "fastcall" fn $($tt)*
    };
    ($(vis:vis)? extern "rustcall" fn $($tt:tt)*) => {
        $($vis)? extern "fastcall" fn $($tt)*
    };
    ($(vis:vis)? unsafe extern "rustcall" fn $($tt:tt)*) => {
        $($vis)? unsafe extern "fastcall" fn $($tt)*
    };
    (extern "rustcall" { $($item:item)*}) => {
        extern "fastcall" {
            $($item:item)*
        }
    }
}

#[cfg(test)]
mod test {
    #![allow(dead_code)]
    rustcall! {
        pub extern "rustcall" fn foo(){}
    }

    type Foo = rustcall!(extern "rustcall" fn());
    type Bar = rustcall!(unsafe extern "rustcall" fn());
    rustcall! {
        extern "rustcall" {
            fn bar();
        }
    }

    const FOO: Foo = foo;

    const BAR: Bar = bar;
}
