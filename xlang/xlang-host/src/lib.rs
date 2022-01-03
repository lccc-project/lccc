#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery, missing_docs)]
//!
//! The [`xlang_host`] crate provides access to the host target's apis, depending on the interfaces available from the OS
//! The definition is such that unknown operating systems that provide the required interfaces will function properly on xlang.

///
/// Interfaces to load and access dynamic libraries on the host, used for finding xlang plugins
pub mod dso;
/// Primitive types defined by the OS (such as the size type and the pointer-sized type).
pub mod primitives;

///
/// A macro which allows definining functions, function pointer types, and extern blocks using the `"rustcall"` abi.
/// This abi matches the ABI used by lcrust v0 for `extern "Rust"` functions, without `#[track_caller]` support.
/// See [The LCRust v0 ABI](https://github.com/LightningCreations/lccc/lcrust/docs/abi/v0.md#Function-ABI) for details on the interface.
///
/// The exact expansion of this macro is unspecified and unstable. You cannot rely on the type of functions or function pointers produced by this interface, only the ABI.
///
/// Due to [a defect](https://github.com/LightningCreations/lccc/issues/6), it is undefined behaviour to panic out of functions defined with this ABI.
/// This is intended to be fixed in the future on as many compilers as possible
#[allow(clippy::doc_markdown)] // No, clippy, LCRust is not an item, it's the name of the ABI
#[macro_export]
#[cfg(not(rustcall_is_fastcall))]
macro_rules! rustcall{
    (extern "rustcall" fn $($tt:tt)*) => {
        extern "C" fn $($tt)*
    };
    (unsafe extern "rustcall" fn $($tt:tt)*) => {
        unsafe extern "C" fn $($tt)*
    };
    ($(#[$meta:meta])* $vis:vis extern "rustcall" fn $($tt:tt)*) => {
        $(#[$meta])* $vis extern "C" fn $($tt)*
    };
    ($(#[$meta:meta])*  $vis:vis unsafe extern "rustcall" fn $($tt:tt)*) => {
        $(#[$meta])* $vis unsafe extern "C" fn $($tt)*
    };
    (extern "rustcall" { $($item:item)*}) => {
        extern "C" {
            $($item)*
        }
    }
}

///
/// A macro which allows definining functions, function pointer types, and extern blocks using the `"rustcall"` abi.
/// This abi matches the ABI used by lcrust v0 for `extern "Rust"` functions, without `#[track_caller]` support.
/// See [The LCRust v0 ABI](https://github.com/LightningCreations/lccc/lcrust/docs/abi/v0.md#Function-ABI) for details on the interface.
///
/// The exact expansion of this macro is unspecified and unstable. You cannot rely on the type of functions or function pointers produced by this interface, only the ABI.
///
/// Due to [a defect](https://github.com/LightningCreations/lccc/issues/6), it is undefined behaviour to panic out of functions defined with this ABI.
/// This is intended to be fixed in the future on as many compilers as possible
#[macro_export]
#[cfg(rustcall_is_fastcall)]
macro_rules! rustcall{
    (extern "rustcall" fn $($tt:tt)*) => {
        extern "fastcall" fn $($tt)*
    };
    (unsafe extern "rustcall" fn $($tt:tt)*) => {
        unsafe extern "fastcall" fn $($tt)*
    };
    ($(#[$meta:meta])* $vis:vis extern "rustcall" fn $($tt:tt)*) => {
        $(#[$meta])* $($vis)? extern "fastcall" fn $($tt)*
    };
    ($(#[$meta:meta])*  $(vis:vis)? unsafe extern "rustcall" fn $($tt:tt)*) => {
        $(#[$meta])* $($vis)? unsafe extern "fastcall" fn $($tt)*
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
