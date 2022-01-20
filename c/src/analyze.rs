use crate::parse::{
    BaseType, Declaration, Expression, FullType, Initializer, Pointer, PrimitiveType, Statement,
    Type,
};
use std::collections::HashMap;

#[allow(clippy::missing_const_for_fn)] // b/c 1.54.0 doesn't support panic in const fns
fn diagnostic() -> ! {
    panic!("¯\\_(ツ)_/¯ Sema error");
}

fn recurse_expr(expr: &mut Expression, ids_to_types: &HashMap<String, FullType>) {
    match expr {
        Expression::FunctionCall { callee, args, ty } => {
            recurse_expr(callee, ids_to_types);
            for arg in args {
                recurse_expr(arg, ids_to_types);
            }
            *ty = Some(match callee.get_type() {
                Some(FullType {
                    inner:
                        Type {
                            base: BaseType::Function { ret, .. },
                            ..
                        },
                    ..
                }) => FullType {
                    inner: (**ret).clone(),
                    pointer: None,
                }, // TODO: Support pointers
                Some(_) => diagnostic(),
                None => unreachable!(),
            });
        }
        Expression::Identifier { id, ty } => {
            *ty = Some(ids_to_types[id].clone());
        }
        Expression::String { ty, .. } => {
            *ty = Some(FullType {
                inner: Type {
                    base: BaseType::Primitive(PrimitiveType::Char),
                    constant: true,
                },
                pointer: Some(Pointer {
                    constant: false,
                    restrict: false,
                    sub_ptr: None,
                }),
            });
        }
    }
}

pub fn analyze(file: &mut Vec<Declaration>) {
    let mut ids_to_types = HashMap::new();
    for decl in file {
        if ids_to_types.contains_key(&decl.name) {
            if ids_to_types[&decl.name]
                != (FullType {
                    inner: decl.ty.clone(),
                    pointer: decl.pointer.clone(),
                })
            {
                diagnostic();
            }
        } else {
            ids_to_types.insert(
                decl.name.clone(),
                FullType {
                    inner: decl.ty.clone(),
                    pointer: decl.pointer.clone(),
                },
            );
        }
        if let Some(init) = &mut decl.initializer {
            match init {
                Initializer::Expression(expr) => recurse_expr(expr, &ids_to_types),
                Initializer::Function(func) => {
                    for stmt in func {
                        let Statement::Expression(expr) = stmt;
                        recurse_expr(expr, &ids_to_types);
                    }
                }
            }
        }
    }
}
