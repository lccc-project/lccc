#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

pub mod mc;

use xlang::{
    plugin::XLangCodegen,
    prelude::v1::{Box, DynBox},
};

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(Box::new(xlang_backend::mc::MCBackend::new(mc::new_writer())))
}}
