use xlang::prelude::v1::{Box, HashMap, None as XLangNone, Pair, Some as XLangSome};
use xlang_struct::{
    AggregateDefinition, BinaryOp, Block, BlockItem, File, FunctionDeclaration, OverflowBehaviour,
    Path, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, StackItem, StackValueKind,
    StaticDefinition, Type,
};

struct TypeState {
    tys: HashMap<Path, Type>,
    aggregate: HashMap<Path, Option<AggregateDefinition>>,
}

impl TypeState {
    pub fn get_field_type<'a>(&'a self, ty: &'a Type, name: &str) -> Option<&'a Type> {
        match ty {
            Type::TaggedType(_, ty) => self.get_field_type(ty, name),
            Type::Product(tys) => {
                let v = name.parse::<usize>().unwrap();
                Some(&tys[v])
            }
            Type::Aligned(_, ty) => self.get_field_type(ty, name),
            Type::Aggregate(n) => Some(
                n.fields
                    .iter()
                    .filter(|Pair(s, _)| s == name)
                    .map(|Pair(_, ty)| ty)
                    .next()
                    .unwrap(),
            ),
            Type::Named(p) => {
                if let Some(ag) = self.aggregate.get(p).map(Option::as_ref) {
                    Some(
                        ag.unwrap()
                            .fields
                            .iter()
                            .filter(|Pair(s, _)| s == name)
                            .map(|Pair(_, ty)| ty)
                            .next()
                            .unwrap(),
                    )
                } else {
                    self.get_field_type(&self.tys[p], name)
                }
            }
            _ => None,
        }
    }
}

fn tycheck_function(x: &mut FunctionDeclaration, tys: &TypeState) {
    match x {
        FunctionDeclaration {
            ty,
            body: XLangSome(body),
        } => {
            let local_tys = ty
                .params
                .iter()
                .cloned()
                .chain(body.locals.iter().cloned())
                .collect::<Vec<_>>();
            let ret = tycheck_block(&mut body.block, tys, &local_tys, 0);
            if ty.ret == Type::Void {
                assert_eq!(ret.len(), 0);
            } else {
                assert_eq!(ret.len(), 1);
                check_unify(&ret[0].ty, &ty.ret, tys)
            }
        }
        _ => {}
    }
}

fn check_unify(ty1: &Type, ty2: &Type, _tys: &TypeState) {
    assert_eq!(ty1, ty2, "Could not unify types {:?},{:?}", ty1, ty2);
}

fn check_unify_stack(stack: &[StackItem], target: &[StackItem], tys: &TypeState) {
    let begin = stack.len() - target.len();
    let stack = &stack[begin..];

    for (a, b) in stack.iter().zip(target) {
        assert!(
            a.kind == b.kind,
            "Could not unify stack items {:?} and {:?}",
            a,
            b
        );
        check_unify(&a.ty, &b.ty, tys);
    }
}

fn tycheck_block(block: &mut Block, tys: &TypeState, locals: &[Type], n: u32) -> Vec<StackItem> {
    let mut vstack = Vec::new();

    let mut targets = HashMap::<_, _>::new();
    let mut diverged = false;

    let mut exit = None;

    for item in &block.items {
        if let BlockItem::Target { num, stack } = item {
            if let Some(_) = targets.insert(*num, stack.clone()) {
                panic!("Target @{} redeclared", num);
            }
        }
    }

    for item in &mut block.items {
        match item {
            BlockItem::Expr(expr) => match expr {
                xlang_struct::Expr::Null => {}
                xlang_struct::Expr::Const(v) => match v {
                    xlang_struct::Value::Invalid(ty) => vstack.push(StackItem {
                        ty: ty.clone(),
                        kind: StackValueKind::RValue,
                    }),
                    xlang_struct::Value::Uninitialized(ty) => vstack.push(StackItem {
                        ty: ty.clone(),
                        kind: StackValueKind::RValue,
                    }),
                    xlang_struct::Value::GenericParameter(_) => todo!(),
                    xlang_struct::Value::Integer { ty, .. } => vstack.push(StackItem {
                        ty: Type::Scalar(*ty),
                        kind: StackValueKind::RValue,
                    }),
                    xlang_struct::Value::GlobalAddress { ty, item } => {
                        match ty {
                            Type::Null => {
                                if let Some(t) = tys.tys.get(item) {
                                    *ty = t.clone();
                                }
                            }
                            _ => {}
                        }

                        vstack.push(StackItem {
                            ty: Type::Pointer(PointerType {
                                inner: Box::new(ty.clone()),
                                ..Default::default()
                            }),
                            kind: StackValueKind::RValue,
                        })
                    }
                    xlang_struct::Value::ByteString { .. } => vstack.push(StackItem {
                        ty: Type::Pointer(PointerType {
                            inner: Box::new(Type::Scalar(ScalarType {
                                header: ScalarTypeHeader {
                                    bitsize: 8,
                                    ..Default::default()
                                },
                                kind: ScalarTypeKind::Integer {
                                    signed: false,
                                    min: XLangNone,
                                    max: XLangNone,
                                },
                            })),
                            ..Default::default()
                        }),
                        kind: StackValueKind::RValue,
                    }),
                    xlang_struct::Value::String { ty, .. } => vstack.push(StackItem {
                        ty: ty.clone(),
                        kind: StackValueKind::RValue,
                    }),
                    xlang_struct::Value::LabelAddress(n) => {
                        assert_eq!(targets[n].len(), 0);
                        vstack.push(StackItem {
                            ty: Type::Pointer(PointerType {
                                inner: Box::new(Type::Void),
                                ..Default::default()
                            }),
                            kind: StackValueKind::RValue,
                        });
                    }
                },
                xlang_struct::Expr::ExitBlock { blk, values } => {
                    if *blk == n {
                        let pos = vstack.len().checked_sub(*values as usize).unwrap();
                        let stack = vstack.split_off(pos);
                        match &exit {
                            None => exit = Some(stack),
                            Some(items) => {
                                assert!(items.len() == (*values).into());
                                check_unify_stack(&stack, &items, tys);
                            }
                        }
                    }
                }
                xlang_struct::Expr::BinaryOp(op, v) => {
                    let val1 = vstack.pop().unwrap();
                    let val2 = vstack.pop().unwrap();

                    assert_eq!(val1.kind, StackValueKind::RValue);
                    assert_eq!(val2.kind, StackValueKind::RValue);
                    match &val1.ty {
                        Type::Scalar(_) => match *op {
                            BinaryOp::CmpInt | BinaryOp::Cmp => vstack.push(StackItem {
                                ty: Type::Scalar(ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 32,
                                        ..Default::default()
                                    },
                                    kind: ScalarTypeKind::Integer {
                                        signed: true,
                                        min: XLangNone,
                                        max: XLangNone,
                                    },
                                }),
                                kind: StackValueKind::RValue,
                            }),
                            BinaryOp::CmpLt
                            | BinaryOp::CmpLe
                            | BinaryOp::CmpGt
                            | BinaryOp::CmpGe
                            | BinaryOp::CmpNe
                            | BinaryOp::CmpEq => {
                                vstack.push(StackItem {
                                    ty: Type::Scalar(ScalarType {
                                        header: ScalarTypeHeader {
                                            bitsize: 1,
                                            ..Default::default()
                                        },
                                        kind: ScalarTypeKind::Integer {
                                            signed: false,
                                            min: XLangNone,
                                            max: XLangNone,
                                        },
                                    }),
                                    kind: StackValueKind::RValue,
                                });
                            }
                            _ => match *v {
                                OverflowBehaviour::Checked => {
                                    vstack.push(val1);
                                    vstack.push(StackItem {
                                        ty: Type::Scalar(ScalarType {
                                            header: ScalarTypeHeader {
                                                bitsize: 1,
                                                ..Default::default()
                                            },
                                            kind: ScalarTypeKind::Integer {
                                                signed: false,
                                                min: XLangNone,
                                                max: XLangNone,
                                            },
                                        }),
                                        kind: StackValueKind::RValue,
                                    });
                                }
                                _ => vstack.push(val1),
                            },
                        },
                        x => todo!("{} {:?}", op, x),
                    }
                }
                xlang_struct::Expr::UnaryOp(_, _) => todo!(),
                xlang_struct::Expr::CallFunction(_) | xlang_struct::Expr::Tailcall(_) => todo!(),
                xlang_struct::Expr::Branch { .. } => todo!(),
                xlang_struct::Expr::BranchIndirect => todo!(),
                xlang_struct::Expr::Convert(_, _) => todo!(),
                xlang_struct::Expr::Derive(_, _) => todo!(),
                xlang_struct::Expr::Local(n) => vstack.push(StackItem {
                    ty: locals[(*n) as usize].clone(),
                    kind: StackValueKind::LValue,
                }),
                xlang_struct::Expr::Pop(n) => {
                    let back = vstack.len().checked_sub((*n) as usize).unwrap();
                    vstack.drain(back..).for_each(core::mem::drop);
                }
                xlang_struct::Expr::Dup(n) => {
                    let back = vstack.len().checked_sub((*n) as usize).unwrap();
                    let stack = vstack.split_off(back);
                    vstack.extend(stack);
                }
                xlang_struct::Expr::Pivot(n, m) => {
                    let back1 = vstack.len().checked_sub((*m) as usize).unwrap();
                    let back2 = back1.checked_sub((*n) as usize).unwrap();
                    let stack1 = vstack.split_off(back1);
                    let stack2 = vstack.split_off(back2);
                    vstack.extend(stack1);
                    vstack.extend(stack2);
                }
                xlang_struct::Expr::Aggregate(ctor) => {
                    let ty = &ctor.ty;
                    let back = vstack.len().checked_sub(ctor.fields.len()).unwrap();
                    let values = vstack.split_off(back);
                    for (field, item) in ctor.fields.iter().zip(values) {
                        assert_eq!(item.kind, StackValueKind::RValue);
                        check_unify(tys.get_field_type(ty, field).unwrap(), &item.ty, tys);
                    }

                    vstack.push(StackItem {
                        ty: ty.clone(),
                        kind: StackValueKind::RValue,
                    });
                }
                xlang_struct::Expr::Member(name) => {
                    let val = vstack.pop().unwrap();

                    assert_eq!(val.kind, StackValueKind::LValue);

                    let ty = tys.get_field_type(&val.ty, name).unwrap();

                    vstack.push(StackItem {
                        ty: ty.clone(),
                        kind: StackValueKind::LValue,
                    });
                }
                xlang_struct::Expr::MemberIndirect(name) => {
                    let mut val = vstack.pop().unwrap();

                    assert_eq!(val.kind, StackValueKind::RValue);

                    match &mut val.ty {
                        Type::Pointer(ptr) => {
                            let ty = tys.get_field_type(&ptr.inner, name).unwrap().clone();

                            *ptr = PointerType {
                                inner: Box::new(ty),
                                ..Default::default()
                            };
                        }
                        ty => panic!("Cannot use member indirect {} on {:?}", name, ty),
                    }

                    vstack.push(val);
                }
                xlang_struct::Expr::Block { n, block } => {
                    let res = tycheck_block(block, tys, locals, *n);

                    vstack.extend(res);
                }
                xlang_struct::Expr::Assign(_) => {
                    let rvalue = vstack.pop().unwrap();
                    let lvalue = vstack.pop().unwrap();

                    assert_eq!(rvalue.kind, StackValueKind::RValue);
                    assert_eq!(lvalue.kind, StackValueKind::LValue);

                    check_unify(&rvalue.ty, &lvalue.ty, tys);
                }
                xlang_struct::Expr::AsRValue(_) => {
                    let val = vstack.pop().unwrap();
                    assert_eq!(val.kind, StackValueKind::LValue);

                    vstack.push(StackItem {
                        ty: val.ty,
                        kind: StackValueKind::RValue,
                    });
                }
                xlang_struct::Expr::CompoundAssign(_, _, _) => todo!(),
                xlang_struct::Expr::LValueOp(_, _, _) => todo!(),
                xlang_struct::Expr::Indirect => todo!(),
                xlang_struct::Expr::AddrOf => todo!(),
                xlang_struct::Expr::Sequence(_) | xlang_struct::Expr::Fence(_) => {}
                xlang_struct::Expr::Switch(_) => todo!(),
            },
            BlockItem::Target { stack, .. } => {
                if !diverged {
                    check_unify_stack(&vstack, stack, tys);
                }
                diverged = false;
                vstack.clear();
                vstack.extend(stack.iter().cloned());
            }
        }
    }

    exit.unwrap_or_else(Vec::new)
}

pub fn tycheck(x: &mut File) {
    let mut typestate = TypeState {
        tys: HashMap::new(),
        aggregate: HashMap::new(),
    };

    // pass one, gather global address types
    for Pair(path, member) in &x.root.members {
        match &member.member_decl {
            xlang_struct::MemberDeclaration::Function(FunctionDeclaration { ty, .. }) => {
                typestate
                    .tys
                    .insert(path.clone(), Type::FnType(Box::new(ty.clone())));
            }
            xlang_struct::MemberDeclaration::Static(StaticDefinition { ty, .. }) => {
                typestate.tys.insert(path.clone(), ty.clone());
            }
            xlang_struct::MemberDeclaration::AggregateDefinition(defn) => {
                typestate.aggregate.insert(path.clone(), Some(defn.clone()));
            }
            xlang_struct::MemberDeclaration::OpaqueAggregate(_) => {
                typestate.aggregate.insert(path.clone(), None);
            }
            _ => {}
        }
    }

    for Pair(_, member) in &mut x.root.members {
        match &mut member.member_decl {
            xlang_struct::MemberDeclaration::Function(
                decl @ FunctionDeclaration {
                    body: XLangSome(_), ..
                },
            ) => {
                tycheck_function(decl, &typestate);
            }
            xlang_struct::MemberDeclaration::Static(_) => todo!(),
            _ => {}
        }
    }
}
