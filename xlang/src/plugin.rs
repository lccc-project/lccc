use xlang_abi::{prelude::v1::*, result::Result};
use xlang_struct::File;

#[repr(u8)]
pub enum PluginError {
    Diagnostic(String),
    InternalError(String),
}

pub trait XLangPlugin {
    fn accept_ir(&mut self, ir: &mut File) -> Result<(), PluginError>;
}

mod abi {
    use xlang_abi::{
        result::Result,
        traits::{AbiSafeTrait, AbiSafeUnsize, DynPtrSafe},
    };
    use xlang_struct::File;

    use super::{PluginError, XLangPlugin};

    unsafe impl AbiSafeTrait for dyn XLangPlugin {
        type VTable = vtable::XLangPlugin;
    }

    mod vtable {
        use xlang_abi::{result::Result, traits::AbiSafeVTable};
        use xlang_struct::File;

        use super::super::PluginError;

        #[repr(C)]
        pub struct XLangPlugin {
            pub size: usize,
            pub align: usize,
            pub dtor: Option<unsafe extern "C" fn(*mut ())>,
            pub reserved: Option<unsafe extern "C" fn(*mut ())>,
            pub accept_ir: unsafe extern "C" fn(*mut (), ir: &mut File) -> Result<(), PluginError>,
        }

        unsafe impl AbiSafeVTable<dyn super::XLangPlugin> for XLangPlugin {}
    }

    unsafe extern "C" fn __dtor<T>(x: *mut ()) {
        core::ptr::drop_in_place(x as *mut T)
    }

    unsafe extern "C" fn __accept_ir<T: XLangPlugin>(
        x: *mut (),
        ir: &mut File,
    ) -> Result<(), PluginError> {
        (&mut *(x as *mut T)).accept_ir(ir)
    }

    unsafe impl<T: XLangPlugin + 'static> AbiSafeUnsize<T> for dyn XLangPlugin {
        fn construct_vtable_for() -> &'static Self::VTable {
            &vtable::XLangPlugin {
                size: core::mem::size_of::<T>(),
                align: core::mem::align_of::<T>(),
                dtor: std::option::Option::Some(__dtor::<T>),
                reserved: std::option::Option::None,
                accept_ir: __accept_ir::<T>,
            }
        }
    }

    impl XLangPlugin for dyn DynPtrSafe<dyn XLangPlugin> {
        fn accept_ir(&mut self, ir: &mut File) -> Result<(), PluginError> {
            unsafe { (self.vtable().accept_ir)(self.as_raw_mut(), ir) }
        }
    }
}
