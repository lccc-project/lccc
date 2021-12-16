use xlang_abi::prelude::v1::*;
use xlang_struct::File;

#[repr(u8)]
pub enum PluginError {
    Diagnostic(String),
    InternalError(String),
}

pub trait XLangPlugin {
    fn accept_ir(&mut self, ir: &mut File) -> Result<(), PluginError>;
}
