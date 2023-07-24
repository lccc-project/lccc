// FIXME: Fix the problems, then switch this back to deny
#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod ast;
mod feature;
mod helpers;
mod interning;
mod irgen;
mod lang;
mod lex;
mod macro_parse;
mod parse;
mod sema;
mod session;
mod span;

use lex::lex;

use peekmore::PeekMore;
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang::targets::properties::{get_properties, TargetProperties};
use xlang::targets::Target;

use crate::{
    irgen::irgen,
    lex::filter_comments,
    parse::do_mod,
    sema::{convert_crate, Definitions},
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CrateType {
    Bin,
    RLib,
    Dylib,
    CDylib,
    Staticlib,
    ProcMacro,
}

struct RustFrontend {
    filename: Option<String>,
    defs: Option<Definitions>,
    target: Option<Target>,
    props: Option<&'static TargetProperties<'static>>,
}

impl RustFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            filename: None,
            defs: None,
            target: None,
            props: None,
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
        let filename = self
            .filename
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_default();

        let crate_name = filename.rsplit('/').next().unwrap();
        let crate_name = crate_name.rsplit('\\').next().unwrap();
        let crate_name = crate_name.split('.').next().unwrap();

        let mut lexed = lex(&mut file, &*filename).unwrap();
        filter_comments(&mut lexed);
        let parsed = do_mod(&mut lexed.into_iter().peekmore()).unwrap();
        let mut defs = Definitions::new();
        convert_crate(&mut defs, &parsed, CrateType::Bin).unwrap();

        eprintln!("{}", defs);
        defs.set_current_crate_name(crate_name);

        self.defs = Some(defs);
        io::Result::Ok(())
    }
}

impl XLangPlugin for RustFrontend {
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        file.target = self.target.clone().unwrap();
        irgen(self.defs.as_mut().unwrap(), file);
        println!("{:#?}", file);
        Result::Ok(())
    }

    fn set_target(&mut self, target: Target) {
        self.props = Some(get_properties(&target).unwrap());
        self.target = Some(target);
    }
}

xlang::host::rustcall! {
    #[allow(clippy::missing_const_for_fn)]
    #[no_mangle]
    pub extern "rustcall" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
        DynBox::unsize_box(Box::new(RustFrontend::new()))
    }
}
