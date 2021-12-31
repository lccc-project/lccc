//! Defines traits used by xlang plugins, frontends, and backends.
//! xlang plugins provide an interface to the lccc program and other drivers, defined by the traits here. This interface is stable in both API and ABI (either changes may be made only in a Semver major version)
//! xlang plugins use the system suffix for dynamic libraries (.so on unix, .dll on windows, and .dylib on macos) and are built and loaded as such. On dll platforms, an import library is not necessary and is unused.
//!
//! xlang plugins should be installed to the xlang plugin directory
//!

use xlang_abi::{
    io::{self, Read, Write},
    prelude::v1::*,
    result::Result,
    string::StringView,
};
use xlang_struct::File;
use xlang_targets::Target;

#[repr(u8)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Error {
    Diagnostic(String),
    InternalError(String),
}

#[allow(clippy::module_name_repetitions)]
pub trait XLangPlugin {
    fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error>;
    fn set_target(&mut self, targ: Target);
}

pub trait XLangFrontend: XLangPlugin {
    fn file_matches(&self, x: StringView) -> bool;
    fn set_file_path(&mut self, x: StringView);
    fn read_source(&mut self, x: DynMut<dyn Read>) -> io::Result<()>;
}

pub trait XLangCodegen: XLangPlugin {
    fn target_matches(&self, x: &xlang_targets::Target) -> bool;
    fn write_output(&mut self, x: DynMut<dyn Write>) -> io::Result<()>;
}

mod abi {
    use xlang_abi::{
        result::Result,
        string::StringView,
        traits::{AbiSafeTrait, AbiSafeUnsize, DynMut, DynPtrSafe},
    };
    use xlang_struct::File;
    use xlang_targets::Target;

    use super::{Error, XLangCodegen, XLangFrontend, XLangPlugin};

    unsafe impl AbiSafeTrait for dyn XLangPlugin {
        type VTable = vtable::XLangPlugin;
    }

    unsafe impl AbiSafeTrait for dyn XLangFrontend {
        type VTable = vtable::XLangFrontend;
    }

    mod vtable {
        use xlang_abi::{
            result::Result,
            string::StringView,
            traits::{AbiSafeVTable, DynMut},
        };
        use xlang_struct::File;
        use xlang_targets::Target;

        use super::super::Error;

        #[repr(C)]
        pub struct XLangPlugin {
            pub size: usize,
            pub align: usize,
            pub dtor: Option<unsafe extern "C" fn(*mut ())>,
            pub reserved: Option<unsafe extern "C" fn(*mut ())>,
            pub accept_ir: unsafe extern "C" fn(*mut (), ir: &mut File) -> Result<(), Error>,
            pub set_target: unsafe extern "C" fn(*mut (), Target),
        }

        unsafe impl AbiSafeVTable<dyn super::XLangPlugin> for XLangPlugin {}

        #[repr(C)]
        pub struct XLangFrontend {
            pub size: usize,
            pub align: usize,
            pub dtor: Option<unsafe extern "C" fn(*mut ())>,
            pub reserved: Option<unsafe extern "C" fn(*mut ())>,
            pub accept_ir: unsafe extern "C" fn(*mut (), &mut File) -> Result<(), Error>,
            pub set_target: unsafe extern "C" fn(*mut (), Target),
            pub file_matches: unsafe extern "C" fn(*const (), StringView) -> bool,
            pub set_file_path: unsafe extern "C" fn(*mut (), StringView),
            pub read_source: unsafe extern "C" fn(
                *mut (),
                DynMut<dyn xlang_abi::io::Read>,
            ) -> xlang_abi::io::Result<()>,
        }

        unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend> for XLangFrontend {}

        #[repr(C)]
        pub struct XLangCodegen {
            pub size: usize,
            pub align: usize,
            pub dtor: Option<unsafe extern "C" fn(*mut ())>,
            pub reserved: Option<unsafe extern "C" fn(*mut ())>,
            pub accept_ir: unsafe extern "C" fn(*mut (), &mut File) -> Result<(), Error>,
            pub set_target: unsafe extern "C" fn(*mut (), Target),
            pub target_matches: unsafe extern "C" fn(*const (), &Target) -> bool,
            pub write_output: unsafe extern "C" fn(
                *mut (),
                DynMut<dyn xlang_abi::io::Write>,
            ) -> xlang_abi::io::Result<()>,
        }

        unsafe impl AbiSafeVTable<dyn super::super::XLangCodegen> for XLangCodegen {}
    }

    unsafe extern "C" fn __dtor<T>(x: *mut ()) {
        core::ptr::drop_in_place(x.cast::<T>());
    }

    unsafe extern "C" fn __accept_ir<T: XLangPlugin>(
        x: *mut (),
        ir: &mut File,
    ) -> Result<(), Error> {
        (&mut *(x.cast::<T>())).accept_ir(ir)
    }

    unsafe extern "C" fn __file_matches<T: XLangFrontend>(
        ptr: *const (),
        file: StringView,
    ) -> bool {
        (&*(ptr.cast::<T>())).file_matches(file)
    }

    unsafe extern "C" fn __set_file_path<T: XLangFrontend>(ptr: *mut (), file: StringView) {
        (&mut *(ptr.cast::<T>())).set_file_path(file);
    }

    unsafe extern "C" fn __set_target<T: XLangPlugin>(ptr: *mut (), target: Target) {
        (&mut *(ptr.cast::<T>())).set_target(target);
    }

    unsafe extern "C" fn __read_source<T: XLangFrontend>(
        ptr: *mut (),
        input: DynMut<dyn xlang_abi::io::Read>,
    ) -> xlang_abi::io::Result<()> {
        (&mut *(ptr.cast::<T>())).read_source(input)
    }

    unsafe extern "C" fn __write_output<T: XLangCodegen>(
        ptr: *mut (),
        output: DynMut<dyn xlang_abi::io::Write>,
    ) -> xlang_abi::io::Result<()> {
        (&mut *(ptr.cast::<T>())).write_output(output)
    }

    unsafe extern "C" fn __target_matches<T: XLangCodegen>(
        ptr: *const (),
        target: &xlang_targets::Target,
    ) -> bool {
        (&*(ptr.cast::<T>())).target_matches(target)
    }

    unsafe impl<T: XLangPlugin + 'static> AbiSafeUnsize<T> for dyn XLangPlugin {
        fn construct_vtable_for() -> &'static Self::VTable {
            &vtable::XLangPlugin {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                dtor: std::option::Option::Some(__dtor::<T>),
                reserved: std::option::Option::None,
                accept_ir: __accept_ir::<T>,
                set_target: __set_target::<T>,
            }
        }
    }

    unsafe impl<T: XLangFrontend + 'static> AbiSafeUnsize<T> for dyn XLangFrontend {
        fn construct_vtable_for() -> &'static Self::VTable {
            &vtable::XLangFrontend {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                dtor: std::option::Option::Some(__dtor::<T>),
                reserved: std::option::Option::None,
                accept_ir: __accept_ir::<T>,
                set_target: __set_target::<T>,
                file_matches: __file_matches::<T>,
                set_file_path: __set_file_path::<T>,
                read_source: __read_source::<T>,
            }
        }
    }

    impl XLangPlugin for dyn DynPtrSafe<dyn XLangPlugin> {
        fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
            unsafe { (self.vtable().accept_ir)(self.as_raw_mut(), ir) }
        }

        fn set_target(&mut self, targ: Target) {
            unsafe { (self.vtable().set_target)(self.as_raw_mut(), targ) }
        }
    }

    impl XLangPlugin for dyn DynPtrSafe<dyn XLangFrontend> {
        fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
            unsafe { (self.vtable().accept_ir)(self.as_raw_mut(), ir) }
        }

        fn set_target(&mut self, targ: Target) {
            unsafe { (self.vtable().set_target)(self.as_raw_mut(), targ) }
        }
    }

    impl XLangFrontend for dyn DynPtrSafe<dyn XLangFrontend> {
        fn file_matches(&self, x: StringView) -> bool {
            unsafe { (self.vtable().file_matches)(self.as_raw(), x) }
        }

        fn set_file_path(&mut self, x: StringView) {
            unsafe { (self.vtable().set_file_path)(self.as_raw_mut(), x) }
        }

        fn read_source(
            &mut self,
            x: xlang_abi::traits::DynMut<dyn xlang_abi::io::Read>,
        ) -> xlang_abi::io::Result<()> {
            unsafe { (self.vtable().read_source)(self.as_raw_mut(), x) }
        }
    }

    unsafe impl AbiSafeTrait for dyn XLangCodegen {
        type VTable = vtable::XLangCodegen;
    }

    unsafe impl<T: XLangCodegen + 'static> AbiSafeUnsize<T> for dyn XLangCodegen {
        fn construct_vtable_for() -> &'static Self::VTable {
            &Self::VTable {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                dtor: Some(__dtor::<T>),
                reserved: None,
                accept_ir: __accept_ir::<T>,
                set_target: __set_target::<T>,
                target_matches: __target_matches::<T>,
                write_output: __write_output::<T>,
            }
        }
    }
    impl XLangPlugin for dyn DynPtrSafe<dyn XLangCodegen> {
        fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
            unsafe { (self.vtable().accept_ir)(self.as_raw_mut(), ir) }
        }
        fn set_target(&mut self, targ: Target) {
            unsafe { (self.vtable().set_target)(self.as_raw_mut(), targ) }
        }
    }
    impl XLangCodegen for dyn DynPtrSafe<dyn XLangCodegen> {
        fn target_matches(&self, x: &xlang_targets::Target) -> bool {
            unsafe { (self.vtable().target_matches)(self.as_raw(), x) }
        }

        fn write_output(
            &mut self,
            x: DynMut<dyn xlang_abi::io::Write>,
        ) -> xlang_abi::io::Result<()> {
            unsafe { (self.vtable().write_output)(self.as_raw_mut(), x) }
        }
    }
}
