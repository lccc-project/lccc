#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use ir::File;
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
use xlang::abi::span::SpanMut;
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};

mod binary_reader;
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

    #[allow(clippy::cast_lossless)]
    fn read_source(&mut self, mut file: DynMut<dyn Read>) -> io::Result<()> {
        let mut b = 0u8;

        xlang::abi::try_!(file.read_exact(SpanMut::from_mut(&mut b)));

        if b == 0xFF {
            let mut mag = [0u8; 3];
            xlang::abi::try_!(file.read_exact(SpanMut::new(&mut mag)));

            if mag != *b"XIR" {
                return xlang::abi::result::Err(io::Error::Message(
                    "Invalid Binary XIR file".into(),
                ));
            }
        } else {
            let mut stream = file.into_chars();
            let c = match b {
                0x00..=0x7f => b as char,
                0xC0..=0xDF => {
                    let c2 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c2) {
                        panic!("Invalid UTF-8");
                    }
                    char::from_u32(((b as u32) << 5) | (c2 as u32)).expect("Invalid UTF-8")
                }
                0xE0..=0xEF => {
                    let c2 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c2) {
                        panic!("Invalid UTF-8");
                    }
                    let c3 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c3) {
                        panic!("Invalid UTF-8");
                    }
                    char::from_u32(((b as u32) << 10) | ((c2 as u32) << 5) | (c3 as u32))
                        .expect("Invalid UTF-8")
                }
                0xF0..=0xF7 => {
                    let c2 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c2) {
                        panic!("Invalid UTF-8");
                    }
                    let c3 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c3) {
                        panic!("Invalid UTF-8");
                    }
                    let c4 = stream.next_byte().expect("Invalid UTF-8");
                    if !(0x80..=0xBF).contains(&c4) {
                        panic!("Invalid UTF-8");
                    }
                    char::from_u32(
                        ((b as u32) << 15) | ((c2 as u32) << 10) | ((c3 as u32) << 5) | (c4 as u32),
                    )
                    .expect("Invalid UTF-8")
                }
                _ => panic!("Invalid UTF-8"),
            };
            let lexed = lex(core::iter::once(c).chain(stream)).collect::<Vec<_>>();
            self.file = Some(parse_file(lexed.into_iter(), self.target.clone().unwrap()));
        }

        Result::Ok(())
    }
}

impl XLangPlugin for XirFrontend {
    #[allow(clippy::too_many_lines)]
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        *file = self.file.take().unwrap();
        validate::tycheck(file);
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
