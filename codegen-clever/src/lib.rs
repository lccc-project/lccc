use xlang::plugin::XLangCodegen;
use xlang::prelude::v1::*;

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    todo!()
}}

xlang::plugin_abi_version!("0.1");
