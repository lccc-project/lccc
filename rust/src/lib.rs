#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

mod irgen;
mod lex;
mod parse;
mod sema;

use irgen::irgen;
use lex::lex;
use parse::parse_crate;
use sema::{convert, typeck_program, Program};

use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang::targets::Target;

struct RustFrontend {
    filename: Option<String>,
    program: Option<Program>,
}

impl RustFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            filename: None,
            program: None,
        }
    }
}

impl XLangFrontend for RustFrontend {
    fn file_matches(&self, name: StringView) -> bool {
        name.ends_with(".rs")
    }

    fn set_file_path(&mut self, name: StringView) {
        self.filename = Some(String::from(&*name));
    }

    fn read_source(&mut self, file: DynMut<dyn Read>) -> io::Result<()> {
        let mut file = file.into_chars();
        let lexed = lex(&mut file);
        let items = parse_crate(lexed.into_iter());
        let mut converted = convert(&items);
        typeck_program(&mut converted);
        self.program = Some(converted);
        io::Result::Ok(())
    }
}

impl XLangPlugin for RustFrontend {
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        irgen(self.program.as_ref().unwrap(), file);
        Result::Ok(())
    }

    fn set_target(&mut self, _: Target) {}
}

xlang::host::rustcall! {
    #[allow(clippy::missing_const_for_fn)]
    #[no_mangle]
    pub extern "rustcall" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
        DynBox::unsize_box(Box::new(RustFrontend::new()))
    }
}
