use cfg_match::cfg_match;


cfg_match! {
    all(any(has_feature_c_unwind="stable",has_feature_c_unwind="feature"),rustcall_is_fastcall) => {
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
        macro_rules! rustcall{
            (extern "rustcall" fn $($tt:tt)*) => {
                extern "fastcall-unwind" fn $($tt)*
            };
            (unsafe extern "rustcall" fn $($tt:tt)*) => {
                unsafe extern "fastcall-unwind" fn $($tt)*
            };
            ($(#[$meta:meta])* $vis:vis extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis extern "fastcall-unwind" fn $($tt)*
            };
            ($(#[$meta:meta])*  $vis:vis unsafe extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis unsafe extern "fastcall-unwind" fn $($tt)*
            };
            (extern "rustcall" { $($item:item)*}) => {
                extern "fastcall-unwind" {
                    $($item)*
                }
            }
        }
    }
    any(has_feature_c_unwind="stable",has_feature_c_unwind="feature") => {
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
        macro_rules! rustcall{
            
            (extern "rustcall" fn $($tt:tt)*) => {
                extern "C-unwind" fn $($tt)*
            };
            (unsafe extern "rustcall" fn $($tt:tt)*) => {
                unsafe extern "C-unwind" fn $($tt)*
            };
            ($(#[$meta:meta])* $vis:vis extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis extern "C-unwind" fn $($tt)*
            };
            ($(#[$meta:meta])*  $vis:vis unsafe extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis unsafe extern "C-unwind" fn $($tt)*
            };
            (extern "rustcall" { $($item:item)*}) => {
                extern "C-unwind" {
                    $($item)*
                }
            }
        }
    }
    rustcall_is_fastcall => {
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
        macro_rules! rustcall{
            (extern "rustcall" fn $($tt:tt)*) => {
                extern "fastcall" fn $($tt)*
            };
            (unsafe extern "rustcall" fn $($tt:tt)*) => {
                unsafe extern "fastcall" fn $($tt)*
            };
            ($(#[$meta:meta])* $vis:vis extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis extern "fastcall" fn $($tt)*
            };
            ($(#[$meta:meta])*  $vis:vis unsafe extern "rustcall" fn $($tt:tt)*) => {
                $(#[$meta])* $vis unsafe extern "fastcall" fn $($tt)*
            };
            (extern "rustcall" { $($item:item)*}) => {
                extern "fastcall" {
                    $($item)*
                }
            }
        } 
    }
    _ => {
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
    }
}
