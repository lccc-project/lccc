#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
mod lex;
mod parse;

use lex::lex;
use parse::parse;
use xlang::abi::string::StringView;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};
use xlang_struct::{
    Abi, AnnotatedElement, File, FnType, FunctionDeclaration, MemberDeclaration, Path,
    PathComponent, ScalarType, ScalarTypeHeader, ScalarTypeKind, ScalarValidity, ScopeMember, Type,
    Visibility,
};

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
        let parsed = parse(&lexed);
        println!("{:?}", parsed);
        io::Result::Ok(())
    }
}

impl XLangPlugin for CFrontend {
    fn accept_ir(&mut self, file: &mut File) -> Result<(), Error> {
        let main_path = Path {
            components: xlang::abi::vec![PathComponent::Text("main".into())],
        };
        let function = FunctionDeclaration {
            ty: FnType {
                ret: Type::Scalar(ScalarType {
                    header: ScalarTypeHeader {
                        bitsize: 32,
                        vectorsize: 0,
                        validity: ScalarValidity::empty(),
                    },
                    kind: ScalarTypeKind::Integer {
                        signed: true,
                        min: i128::MIN,
                        max: i128::MAX,
                    },
                }),
                params: Vec::new(),
                tag: Abi::C,
            },
            body: None,
        };
        file.root.members.insert(
            main_path,
            ScopeMember {
                annotations: AnnotatedElement {
                    annotations: Vec::new(),
                },
                vis: Visibility::Public,
                member_decl: MemberDeclaration::Function(function),
            },
        );
        Result::Ok(())
    }
}

#[allow(clippy::missing_const_for_fn)]
#[no_mangle]
pub extern "C" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
    DynBox::unsize_box(Box::new(CFrontend::new()))
}
