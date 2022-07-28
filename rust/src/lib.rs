// FIXME: Fix the problems, then switch this back to deny
#![allow(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

mod irgen;
mod lex;
mod macro_parse;
mod parse;
mod sema;
mod session;

use lex::lex;
use parse::parse_mod;
use sema::{convert, Program};

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
        let items = parse_mod(lexed.into_iter(), std::vec::Vec::new());
        let mut converted = convert(&items);
        println!("{}", converted);
        self.program = Some(converted);
        io::Result::Ok(())
    }
}

impl XLangPlugin for RustFrontend {
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        todo!()
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
