//! Defines traits used by xlang plugins, frontends, and backends.
//! xlang plugins provide an interface to the lccc program and other drivers, defined by the traits here. This interface is stable in both API and ABI (either changes may be made only in a Semver major version)
//! xlang plugins use the system suffix for dynamic libraries (.so on unix, .dll on windows, and .dylib on macos) and are built and loaded as such. On dll platforms, an import library is not necessary and is unused.
//!
//! xlang plugins should be installed to the xlang plugin directory
//!

#![allow(improper_ctypes_definitions)]

pub mod v1 {
    use xlang_abi::{
        io::{self, Read, ReadSeek, Write},
        prelude::v1::*,
        result::Result,
        span::Span,
        string::StringView,
    };
    use xlang_struct::File;
    use xlang_targets::{properties::MachineProperties, properties::TargetProperties};

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
        fn set_target(&mut self, targ: &'static TargetProperties<'static>);
    }

    pub trait DriverCallbacks {
        fn read_relative_file(&mut self, path: StringView) -> io::Result<DynBox<dyn ReadSeek>>;
    }

    pub trait XLangFrontend: XLangPlugin {
        fn file_matches(&self, x: StringView) -> bool;
        fn set_file_path(&mut self, x: StringView);
        fn read_source(&mut self, x: DynMut<dyn Read>) -> io::Result<()>;
        fn set_machine(&mut self, _mach: &MachineProperties) {}
        fn required_plugins(&self) -> Vec<String> {
            Vec::new()
        }
        fn set_callbacks(&mut self, _callbacks: DynBox<dyn DriverCallbacks>) {}
    }

    pub trait XLangCodegen: XLangPlugin {
        fn target_matches(&self, x: StringView) -> bool;
        fn write_output(&mut self, x: DynMut<dyn Write>, mode: OutputMode) -> io::Result<()>;
        fn set_features(&mut self, _features: Span<StringView>) {}
    }

    mod abi {
        use xlang_abi::{
            result::Result,
            span::Span,
            string::{String, StringView},
            traits::{AbiSafeTrait, AbiSafeUnsize, DynBox, DynMut, DynPtrSafe},
            vec::Vec,
        };
        use xlang_struct::File;
        use xlang_targets::properties::{MachineProperties, TargetProperties};

        use super::{DriverCallbacks, Error, OutputMode, XLangCodegen, XLangFrontend, XLangPlugin};

        unsafe impl AbiSafeTrait for dyn XLangPlugin {
            type VTable = vtable::XLangPlugin;
        }

        unsafe impl AbiSafeTrait for dyn XLangPlugin + Send {
            type VTable = vtable::XLangPlugin;
        }

        unsafe impl AbiSafeTrait for dyn XLangPlugin + Sync {
            type VTable = vtable::XLangPlugin;
        }

        unsafe impl AbiSafeTrait for dyn XLangPlugin + Send + Sync {
            type VTable = vtable::XLangPlugin;
        }

        unsafe impl AbiSafeTrait for dyn XLangFrontend {
            type VTable = vtable::XLangFrontend;
        }

        unsafe impl AbiSafeTrait for dyn XLangFrontend + Send {
            type VTable = vtable::XLangFrontend;
        }

        unsafe impl AbiSafeTrait for dyn XLangFrontend + Sync {
            type VTable = vtable::XLangFrontend;
        }

        unsafe impl AbiSafeTrait for dyn XLangFrontend + Send + Sync {
            type VTable = vtable::XLangFrontend;
        }

        unsafe impl AbiSafeTrait for dyn DriverCallbacks {
            type VTable = vtable::DriverCallbacks;
        }

        unsafe impl AbiSafeTrait for dyn DriverCallbacks + Send {
            type VTable = vtable::DriverCallbacks;
        }

        unsafe impl AbiSafeTrait for dyn DriverCallbacks + Sync {
            type VTable = vtable::DriverCallbacks;
        }

        unsafe impl AbiSafeTrait for dyn DriverCallbacks + Send + Sync {
            type VTable = vtable::DriverCallbacks;
        }

        mod vtable {
            use xlang_abi::{
                result::Result,
                span::Span,
                string::{String, StringView},
                traits::{AbiSafeVTable, DynBox, DynMut},
                vec::Vec,
            };
            use xlang_struct::File;
            use xlang_targets::properties::{MachineProperties, TargetProperties};

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
                pub set_target: xlang_host::rustcall!(
                    unsafe extern "rustcall" fn(*mut (), &'static TargetProperties<'static>)
                ),
            }

            unsafe impl AbiSafeVTable<dyn super::XLangPlugin> for XLangPlugin {}
            unsafe impl AbiSafeVTable<dyn super::XLangPlugin + Send> for XLangPlugin {}
            unsafe impl AbiSafeVTable<dyn super::XLangPlugin + Sync> for XLangPlugin {}
            unsafe impl AbiSafeVTable<dyn super::XLangPlugin + Send + Sync> for XLangPlugin {}

            #[repr(C)]
            pub struct XLangFrontend {
                pub plugin_vtable: XLangPlugin,
                pub file_matches: xlang_host::rustcall!(
                    unsafe extern "rustcall" fn(*const (), StringView) -> bool
                ),
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
                pub set_callbacks: xlang_host::rustcall!(
                    unsafe extern "rustcall" fn(*mut (), DynBox<dyn super::super::DriverCallbacks>)
                ),
            }

            unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend> for XLangFrontend {}
            unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend + Send> for XLangFrontend {}
            unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend + Sync> for XLangFrontend {}
            unsafe impl AbiSafeVTable<dyn super::super::XLangFrontend + Send + Sync> for XLangFrontend {}

            #[repr(C)]
            pub struct XLangCodegen {
                pub plugin_vtable: XLangPlugin,
                pub target_matches: xlang_host::rustcall!(
                    unsafe extern "rustcall" fn(*const (), StringView) -> bool
                ),
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
            unsafe impl AbiSafeVTable<dyn super::super::XLangCodegen + Send> for XLangCodegen {}
            unsafe impl AbiSafeVTable<dyn super::super::XLangCodegen + Sync> for XLangCodegen {}
            unsafe impl AbiSafeVTable<dyn super::super::XLangCodegen + Send + Sync> for XLangCodegen {}

            #[repr(C)]
            pub struct DriverCallbacks {
                pub size: usize,
                pub align: usize,
                pub dtor: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
                pub reserved: Option<xlang_host::rustcall!(unsafe extern "rustcall" fn(*mut ()))>,
                pub read_relative_file: xlang_host::rustcall!(
                    unsafe extern "rustcall" fn(
                        *mut (),
                        StringView,
                    ) -> xlang_abi::io::Result<
                        DynBox<dyn xlang_abi::io::ReadSeek>,
                    >
                ),
            }

            unsafe impl AbiSafeVTable<dyn super::super::DriverCallbacks> for DriverCallbacks {}
            unsafe impl AbiSafeVTable<dyn super::super::DriverCallbacks + Send> for DriverCallbacks {}
            unsafe impl AbiSafeVTable<dyn super::super::DriverCallbacks + Sync> for DriverCallbacks {}
            unsafe impl AbiSafeVTable<dyn super::super::DriverCallbacks + Send + Sync> for DriverCallbacks {}
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

        xlang_host::rustcall! {unsafe extern "rustcall" fn __set_target<T: XLangPlugin>(ptr: *mut (), target: &'static TargetProperties<'static>) {
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
            target: StringView,
        ) -> bool {
            (&*(ptr.cast::<T>())).target_matches(target)
        }}

        xlang_host::rustcall! {
            unsafe extern "rustcall" fn __set_machine<T: XLangFrontend>(ptr: *mut (), machine: &MachineProperties){
                (&mut *(ptr.cast::<T>())).set_machine(machine);
            }
        }

        xlang_host::rustcall! {
            unsafe extern "rustcall" fn __required_plugins<T: XLangFrontend>(ptr: *const ()) -> Vec<String>{
                (&*(ptr.cast::<T>())).required_plugins()
            }
        }

        xlang_host::rustcall! {
            unsafe extern "rustcall" fn __set_features<T: XLangCodegen>(ptr: *mut (),features: Span<StringView>){
                (&mut *(ptr.cast::<T>())).set_features(features);
            }
        }

        xlang_host::rustcall! {
            unsafe extern "rustcall" fn __read_relative_file<T: DriverCallbacks>(ptr: *mut (), path: StringView) -> xlang_abi::io::Result<DynBox<dyn xlang_abi::io::ReadSeek>>{
                unsafe{&mut *(ptr.cast::<T>())}.read_relative_file(path)
            }
        }

        xlang_host::rustcall! {
            unsafe extern "rustcall" fn __set_callbacks<T: XLangFrontend>(ptr: *mut (),features: DynBox<dyn DriverCallbacks>){
                (&mut *(ptr.cast::<T>())).set_callbacks(features);
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
                    set_callbacks: __set_callbacks::<T>,
                }
            }
        }

        impl XLangPlugin for dyn DynPtrSafe<dyn XLangPlugin> {
            fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
                unsafe { (self.vtable().accept_ir)(self.as_raw_mut(), ir) }
            }

            fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
                unsafe { (self.vtable().set_target)(self.as_raw_mut(), targ) }
            }
        }

        impl XLangPlugin for dyn DynPtrSafe<dyn XLangFrontend> {
            fn accept_ir(&mut self, ir: &mut File) -> Result<(), Error> {
                unsafe { (self.vtable().plugin_vtable.accept_ir)(self.as_raw_mut(), ir) }
            }

            fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
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

            fn set_callbacks(&mut self, callbacks: DynBox<dyn DriverCallbacks>) {
                unsafe { (self.vtable().set_callbacks)(self.as_raw_mut(), callbacks) }
            }
        }

        unsafe impl AbiSafeTrait for dyn XLangCodegen {
            type VTable = vtable::XLangCodegen;
        }

        unsafe impl AbiSafeTrait for dyn XLangCodegen + Send {
            type VTable = vtable::XLangCodegen;
        }

        unsafe impl AbiSafeTrait for dyn XLangCodegen + Sync {
            type VTable = vtable::XLangCodegen;
        }

        unsafe impl AbiSafeTrait for dyn XLangCodegen + Send + Sync {
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
            fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
                unsafe { (self.vtable().plugin_vtable.set_target)(self.as_raw_mut(), targ) }
            }
        }
        impl XLangCodegen for dyn DynPtrSafe<dyn XLangCodegen> {
            fn target_matches(&self, x: StringView) -> bool {
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

        unsafe impl<T: DriverCallbacks + 'static> AbiSafeUnsize<T> for dyn DriverCallbacks {
            fn construct_vtable_for() -> &'static Self::VTable {
                &Self::VTable {
                    size: core::mem::size_of::<T>(),
                    align: core::mem::align_of::<T>(),
                    dtor: Some(__dtor::<T>),
                    reserved: None,
                    read_relative_file: __read_relative_file::<T>,
                }
            }
        }

        impl DriverCallbacks for dyn DynPtrSafe<dyn DriverCallbacks> {
            fn read_relative_file(
                &mut self,
                path: StringView,
            ) -> xlang_abi::io::Result<DynBox<dyn xlang_abi::io::ReadSeek>> {
                unsafe { (self.vtable().read_relative_file)(self.as_raw_mut(), path) }
            }
        }
    }
}

pub use v1::*;
