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
    span::Span,
    string::StringView,
};
use xlang_struct::File;
use xlang_targets::{properties::MachineProperties, Target};

fake_enum::fake_enum! {
    #[repr(u32)]
    pub enum struct OutputMode{
        Obj = 0,
        Asm = 1,
    }
}

fake_enum::fake_enum! {
    #[repr(u32)]
    pub enum struct LibraryType{
        Unspecified = 0,
        Static = 1,
        Shared = 2,
        Framework = 3,
    }
}

#[repr(u32)]
pub enum LinkOpt {
    InputFile(String),
    Library(LibraryType, String),
    Libdir(String),
    RawOption(String),
}

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
    fn set_machine(&mut self, _mach: &MachineProperties) {}
    fn required_plugins(&self) -> Vec<String> {
        Vec::new()
    }
}

pub trait XLangCodegen: XLangPlugin {
    fn target_matches(&self, x: &xlang_targets::Target) -> bool;
    fn write_output(&mut self, x: DynMut<dyn Write>, mode: OutputMode) -> io::Result<()>;
    fn set_features(&mut self, _features: Span<StringView>) {}
}

mod abi {
    use xlang_abi::{
        result::Result,
        span::Span,
        string::{String, StringView},
        traits::{AbiSafeTrait, AbiSafeUnsize, DynMut, DynPtrSafe},
        vec::Vec,
    };
    use xlang_struct::File;
    use xlang_targets::{properties::MachineProperties, Target};

    use super::{Error, OutputMode, XLangCodegen, XLangFrontend, XLangPlugin};

    unsafe impl AbiSafeTrait for dyn XLangPlugin {
        type VTable = vtable::XLangPlugin;
    }

    unsafe impl AbiSafeTrait for dyn XLangFrontend {
        type VTable = vtable::XLangFrontend;
    }

    mod vtable {
        use xlang_abi::{
            result::Result,
            span::Span,
            string::{String, StringView},
            traits::{AbiSafeVTable, DynMut},
            vec::Vec,
        };
        use xlang_struct::File;
        use xlang_targets::{properties::MachineProperties, Target};

        use crate::plugin::OutputMode;

        use super::super::Error;

        #[repr(C)]
        pub struct XLangPlugin {
            pub size: usize,
            pub align: usize,
            pub dtor: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
            pub reserved: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
            pub accept_ir: xlang_host::rustcall!(
                unsafe extern "rustcall" fn(*mut (), ir: &mut File) -> Result<(), Error>
            ),
            pub set_target: xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut (), Target)),
        }

        unsafe impl AbiSafeVTable<dyn super::XLangPlugin> for XLangPlugin {}

        #[repr(C)]
        pub struct XLangFrontend {
            pub plugin_vtable: XLangPlugin,
            pub file_matches:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*const (), StringView) -> bool),
            pub set_file_path:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut (), StringView)),
            pub read_source: xlang_host::rustcall!(
                unsafe extern "rustcall" fn(
                    *mut (),
                    DynMut<dyn xlang_abi::io::Read>,
                ) -> xlang_abi::io::Result<()>
            ),

            pub set_machine:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut (), &MachineProperties)),
            pub required_plugins:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*const ()) -> Vec<String>),
        }

        unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend> for XLangFrontend {}

        #[repr(C)]
        pub struct XLangCodegen {
            pub plugin_vtable: XLangPlugin,
            pub target_matches:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*const (), &Target) -> bool),
            pub write_output: xlang_host::rustcall!(
                unsafe extern "rustcall" fn(
                    *mut (),
                    DynMut<dyn xlang_abi::io::Write>,
                    OutputMode,
                ) -> xlang_abi::io::Result<()>
            ),
            pub set_features:
                xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut (), Span<StringView>)),
        }

        unsafe impl AbiSafeVTable<dyn super::super::XLangCodegen> for XLangCodegen {}
    }

    xlang_host::rustcall! {unsafe extern "rustcall" fn __dtor<T>(x: *mut ()) {
        core::ptr::drop_in_place(x.cast::<T>());
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __accept_ir<T: XLangPlugin>(
        x: *mut (),
        ir: &mut File,
    ) -> Result<(), Error> {
        (&mut *(x.cast::<T>())).accept_ir(ir)
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __file_matches<T: XLangFrontend>(
        ptr: *const (),
        file: StringView,
    ) -> bool {
        (&*(ptr.cast::<T>())).file_matches(file)
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __set_file_path<T: XLangFrontend>(ptr: *mut (), file: StringView) {
        (&mut *(ptr.cast::<T>())).set_file_path(file);
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __set_target<T: XLangPlugin>(ptr: *mut (), target: Target) {
        (&mut *(ptr.cast::<T>())).set_target(target);
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __read_source<T: XLangFrontend>(
        ptr: *mut (),
        input: DynMut<dyn xlang_abi::io::Read>,
    ) -> xlang_abi::io::Result<()> {
        (&mut *(ptr.cast::<T>())).read_source(input)
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __write_output<T: XLangCodegen>(
        ptr: *mut (),
        output: DynMut<dyn xlang_abi::io::Write>,
        mode: OutputMode
    ) -> xlang_abi::io::Result<()> {
        (&mut *(ptr.cast::<T>())).write_output(output,mode)
    }}

    xlang_host::rustcall! {unsafe extern "rustcall" fn __target_matches<T: XLangCodegen>(
        ptr: *const (),
        target: &xlang_targets::Target,
    ) -> bool {
        (&*(ptr.cast::<T>())).target_matches(target)
    }}

    xlang_host::rustcall! {
        unsafe extern "rustcall" fn __set_machine<T: XLangFrontend>(ptr: *mut (), machine: &MachineProperties){
            (&mut *(ptr.cast::<T>())).set_machine(machine)
        }
    }

    xlang_host::rustcall! {
        unsafe extern "rustcall" fn __required_plugins<T: XLangFrontend>(ptr: *const ()) -> Vec<String>{
            (&*(ptr.cast::<T>())).required_plugins()
        }
    }

    xlang_host::rustcall! {
        unsafe extern "rustcall" fn __set_features<T: XLangCodegen>(ptr: *mut (),features: Span<StringView>){
            (&mut *(ptr.cast::<T>())).set_features(features)
        }
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
                plugin_vtable: vtable::XLangPlugin {
                    size: core::mem::size_of::<T>(),
                    align: core::mem::align_of::<T>(),
                    dtor: Some(__dtor::<T>),
                    reserved: None,
                    accept_ir: __accept_ir::<T>,
                    set_target: __set_target::<T>,
                },
                file_matches: __file_matches::<T>,
                set_file_path: __set_file_path::<T>,
                read_source: __read_source::<T>,
                set_machine: __set_machine::<T>,
                required_plugins: __required_plugins::<T>,
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
            unsafe { (self.vtable().plugin_vtable.accept_ir)(self.as_raw_mut(), ir) }
        }

        fn set_target(&mut self, targ: Target) {
            unsafe { (self.vtable().plugin_vtable.set_target)(self.as_raw_mut(), targ) }
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

        fn set_machine(&mut self, machine: &MachineProperties) {
            unsafe { (self.vtable().set_machine)(self.as_raw_mut(), machine) }
        }

        fn required_plugins(&self) -> Vec<String> {
            unsafe { (self.vtable().required_plugins)(self.as_raw()) }
        }
    }

    unsafe impl AbiSafeTrait for dyn XLangCodegen {
        type VTable = vtable::XLangCodegen;
    }

    unsafe impl<T: XLangCodegen + 'static> AbiSafeUnsize<T> for dyn XLangCodegen {
        fn construct_vtable_for() -> &'static Self::VTable {
            &Self::VTable {
                plugin_vtable: vtable::XLangPlugin {
                    size: core::mem::size_of::<T>(),
                    align: core::mem::align_of::<T>(),
                    dtor: Some(__dtor::<T>),
                    reserved: None,
                    accept_ir: __accept_ir::<T>,
                    set_target: __set_target::<T>,
                },
                target_matches: __target_matches::<T>,
                write_output: __write_output::<T>,
                set_features: __set_features::<T>,
            }
        }
    }
    impl XLangPlugin for dyn DynPtrSafe<dyn XLangCodegen> {
        fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
            unsafe { (self.vtable().plugin_vtable.accept_ir)(self.as_raw_mut(), ir) }
        }
        fn set_target(&mut self, targ: Target) {
            unsafe { (self.vtable().plugin_vtable.set_target)(self.as_raw_mut(), targ) }
        }
    }
    impl XLangCodegen for dyn DynPtrSafe<dyn XLangCodegen> {
        fn target_matches(&self, x: &xlang_targets::Target) -> bool {
            unsafe { (self.vtable().target_matches)(self.as_raw(), x) }
        }

        fn write_output(
            &mut self,
            x: DynMut<dyn xlang_abi::io::Write>,
            mode: OutputMode,
        ) -> xlang_abi::io::Result<()> {
            unsafe { (self.vtable().write_output)(self.as_raw_mut(), x, mode) }
        }

        fn set_features(&mut self, features: Span<StringView>) {
            unsafe { (self.vtable().set_features)(self.as_raw_mut(), features) }
        }
    }
}
