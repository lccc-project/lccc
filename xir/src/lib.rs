#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use ir::File;
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};

mod lexer;
mod parser;
mod validate;

use lexer::lex;
use xlang::targets::Target;

use crate::parser::parse_file;

struct XirFrontend {
    filename: Option<String>,
    target: Option<Target>,
    file: Option<File>,
}

impl XirFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            filename: None,
            target: None,
            file: None,
        }
    }
}

impl XLangFrontend for XirFrontend {
    fn file_matches(&self, name: StringView) -> bool {
        name.ends_with(".xir")
    }

    fn set_file_path(&mut self, name: StringView) {
        self.filename = Some(String::from(&*name));
    }

    fn read_source(&mut self, file: DynMut<dyn Read>) -> io::Result<()> {
        let lexed = lex(file.into_chars()).collect::<Vec<_>>();
        self.file = Some(parse_file(lexed.into_iter(), self.target.clone().unwrap()));
        Result::Ok(())
    }
}

impl XLangPlugin for XirFrontend {
    #[allow(clippy::too_many_lines)]
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        *file = self.file.take().unwrap();
        Result::Ok(())
    }

    fn set_target(&mut self, targ: xlang::targets::Target) {
        self.target = Some(targ);
    }
}

xlang::host::rustcall! {
    #[allow(clippy::missing_const_for_fn)]
    #[no_mangle]
    pub extern "rustcall" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
        DynBox::unsize_box(Box::new(XirFrontend::new()))
    }
}
