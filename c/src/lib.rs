#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
mod lex;

use lex::lex;
use xlang::abi::string::StringView;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang_struct::File;

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
        let mut file = file.into_chars();
        let lexed = lex(&mut file);
        println!("{:?}", lexed);
        io::Result::Ok(())
    }
}

impl XLangPlugin for CFrontend {
    fn accept_ir(&mut self, _: &mut File) -> Result<(), Error> {
        todo!()
    }
}

#[allow(clippy::missing_const_for_fn)]
#[no_mangle]
pub extern "C" fn xlang_plugin_main() -> DynBox<dyn XLangFrontend> {
    DynBox::unsize_box(Box::new(CFrontend::new()))
}
