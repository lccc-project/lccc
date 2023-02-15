// FIXME: Fix the problems, then switch this back to deny
#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod interning;
mod irgen;
mod lex;
mod macro_parse;
mod parse;
mod sema;
mod session;
mod span;

use lex::lex;

use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang::targets::Target;

struct RustFrontend {
    filename: Option<String>,
}

impl RustFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self { filename: None }
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
        let lexed = lex(&mut file).unwrap();
        println!("{:#?}", lexed);
        io::Result::Ok(())
    }
}

impl XLangPlugin for RustFrontend {
    fn accept_ir(&mut self, _file: &mut ir::File) -> Result<(), Error> {
        // println!("{:#?}", file);
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
