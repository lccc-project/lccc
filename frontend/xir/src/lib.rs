#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

use xlang::abi::io::{self, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang::targets::properties::TargetProperties;

mod lexer;
mod parser;

struct XirFrontend {
    filename: Option<String>,
    target: Option<&'static TargetProperties<'static>>,
}

impl XirFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            filename: None,
            target: None,
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

    #[allow(clippy::cast_lossless)]
    fn read_source(&mut self, it: DynMut<dyn Read>) -> io::Result<()> {
        use xlang::abi::io::IntoChars;
        let filename = self
            .filename
            .take()
            .unwrap_or_else(|| String::from("<unset file name>"));

        let chars = it.into_chars();

        let lexer = lexer::XirLexer;

        let toks = lexer::lex_file(&lexer, chars, filename).unwrap();
        println!("{:?}", toks);

        xlang::abi::result::Ok(())
    }
}

impl XLangPlugin for XirFrontend {
    #[allow(clippy::too_many_lines)]
    fn accept_ir(&mut self, _: &mut ir::File) -> Result<(), Error> {
        todo!()
    }

    fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
        self.target = Some(targ);
    }
}

xlang::host::rustcall! {
    #[allow(clippy::missing_const_for_fn)]
    #[allow(improper_ctypes_definitions)]
    #[no_mangle]
    pub extern "rustcall" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
        DynBox::unsize_box(Box::new(XirFrontend::new()))
    }
}
