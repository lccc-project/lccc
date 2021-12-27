#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use xlang::abi::io::{self, IntoChars, Read};
use xlang::abi::prelude::v1::*;
use xlang::abi::result::Result;
mod lex;
mod parse;

use lex::lex;
use parse::{
    parse, BaseType, Declaration, Expression, Initializer, Pointer, PrimitiveType, Statement, Type,
};
use xlang::abi::string::StringView;
use xlang::ir;
use xlang::plugin::{Error, XLangFrontend, XLangPlugin};

fn diagnostic() -> ! {
    panic!("Hello everyone, and welcome to yet another placeholder function for generating a diagnostic!")
}

struct CFrontend {
    filename: Option<String>,
    parsed: std::vec::Vec<Declaration>,
}

impl CFrontend {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            filename: None,
            parsed: std::vec::Vec::new(),
        }
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
        self.parsed = parse(&lexed);
        io::Result::Ok(())
    }
}

impl XLangPlugin for CFrontend {
    fn accept_ir(&mut self, file: &mut ir::File) -> Result<(), Error> {
        let char_type = ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: 8,
                vectorsize: 0,
                validity: ir::ScalarValidity::empty(),
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: false,
                min: i128::from(u8::MIN),
                max: i128::from(u8::MAX),
            },
        };
        let int_type = ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: 32,
                vectorsize: 0,
                validity: ir::ScalarValidity::empty(),
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: true,
                min: i128::from(i32::MIN),
                max: i128::from(i32::MAX),
            },
        };

        fn into_xir_type_real(
            char_type: &ir::ScalarType,
            int_type: &ir::ScalarType,
            ty: &Type,
            pointer: Option<&Pointer>,
        ) -> ir::Type {
            // const is not currently supported by xir
            let inner = match ty.base {
                BaseType::Function { .. } => todo!(),
                BaseType::Primitive(PrimitiveType::Char) => ir::Type::Scalar(*char_type),
                BaseType::Primitive(PrimitiveType::Int) => ir::Type::Scalar(*int_type),
            };
            if let Some(pointer) = pointer {
                if pointer.sub_ptr.is_some() {
                    todo!()
                } else {
                    ir::Type::Pointer(ir::PointerType {
                        alias: ir::PointerAliasingRule::None,
                        valid_range: xlang::abi::pair::Pair(ir::ValidRangeType::None, 0),
                        inner: Box::new(inner),
                    })
                }
            } else {
                inner
            }
        }

        let into_xir_type = |ty: &Type, pointer: Option<&Pointer>| {
            into_xir_type_real(&char_type, &int_type, ty, pointer)
        };

        fn codegen_expr_real(
            char_type: &ir::ScalarType,
            int_type: &ir::ScalarType,
            expr: &Expression,
            block: &mut Vec<ir::BlockItem>,
        ) {
            match expr {
                Expression::FunctionCall { callee, args } => {
                    codegen_expr_real(char_type, int_type, callee, block);
                    for arg in args {
                        codegen_expr_real(char_type, int_type, arg, block);
                    }
                    block.push(ir::BlockItem::Expr(ir::Expr::CallFunction(ir::FnType {
                        ret: ir::Type::Scalar(*int_type),
                        params: xlang::vec![into_xir_type_real(
                            char_type,
                            int_type,
                            &Type {
                                base: BaseType::Primitive(PrimitiveType::Char),
                                constant: false
                            },
                            Some(&Pointer {
                                constant: true,
                                restrict: false,
                                sub_ptr: std::option::Option::None
                            })
                        )],
                        tag: ir::Abi::C,
                    })));
                }
                _ => todo!("{:?}", expr),
            }
        }

        let codegen_expr = |expr: &Expression, block: &mut Vec<ir::BlockItem>| {
            codegen_expr_real(&char_type, &int_type, expr, block)
        };

        for decl in &self.parsed {
            let member = if let Type {
                base: BaseType::Function { ret, params },
                ..
            } = &decl.ty
            {
                let mut param_types = Vec::new();
                for param in params {
                    param_types.push(into_xir_type(
                        &param.0.inner,
                        param.0.pointer.as_ref().into(),
                    ));
                }
                let body = decl.initializer.as_ref().map(|init| {
                    if let Initializer::Function(statements) = init {
                        let mut block = Vec::new();
                        for statement in statements {
                            let Statement::Expression(expr) = statement;
                            codegen_expr(expr, &mut block);
                        }
                        if decl.name == "main" {
                            block.push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Integer {
                                ty: int_type,
                                val: 0,
                            })));
                            block.push(ir::BlockItem::Expr(ir::Expr::ExitBlock {
                                blk: 0,
                                values: 1,
                            }));
                        }
                        ir::Block { items: block }
                    } else {
                        diagnostic()
                    }
                });
                let function = ir::FunctionDeclaration {
                    ty: ir::FnType {
                        ret: into_xir_type(&**ret, None),
                        params: param_types,
                        tag: ir::Abi::C,
                    },
                    body: body.into(),
                };
                ir::ScopeMember {
                    annotations: ir::AnnotatedElement {
                        annotations: Vec::new(),
                    },
                    vis: ir::Visibility::Public,
                    member_decl: ir::MemberDeclaration::Function(function),
                }
            } else {
                todo!()
            };
            file.root.members.insert(
                ir::Path {
                    components: xlang::vec![ir::PathComponent::Text(String::from(&decl.name))],
                },
                member,
            );
        }
        Result::Ok(())
    }
}

#[allow(clippy::missing_const_for_fn)]
#[no_mangle]
pub extern "C" fn xlang_frontend_main() -> DynBox<dyn XLangFrontend> {
    DynBox::unsize_box(Box::new(CFrontend::new()))
}
