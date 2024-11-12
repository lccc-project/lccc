#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(dead_code, unused_variables)] // For now
use xlang::abi::io::{self, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;

mod analyze;
mod lex;
mod mode;
mod parse;
mod pplex;

use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang::targets::properties::TargetProperties;

#[allow(clippy::missing_const_for_fn)] // b/c 1.54.0 doesn't support panic in const fns
fn diagnostic() -> ! {
    panic!("Hello everyone, and welcome to yet another placeholder function for generating a diagnostic!")
}

struct CFrontend {
    filename: Option<String>,
}

impl CFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self { filename: None }
    }
}

impl XLangFrontend for CFrontend {
    fn file_matches(&self, name: StringView) -> bool {
        name.ends_with(".c") || name.ends_with(".h")
    }

    fn set_file_path(&mut self, name: StringView) {
        self.filename = Some(String::from(&*name));
    }

    fn read_source(&mut self, file: DynMut<dyn Read>) -> io::Result<()> {
        todo!()
    }
}

impl XLangPlugin for CFrontend {
    #[allow(clippy::too_many_lines, unreachable_code)] // ray can fix passing properties.
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        todo!()
    }

    fn set_target(&mut self, _targ: &'static TargetProperties<'static>) {}
}

xlang::host::rustcall! {
    #[allow(clippy::missing_const_for_fn)]
    #[no_mangle]
    #[allow(improper_ctypes_definitions)]
    pub extern "rustcall" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
        DynBox::unsize_box(Box::new(CFrontend::new()))
    }
}
