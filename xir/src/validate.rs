use std::cell::RefCell;

use xlang::{
    abi::{collection::HashSet, string::StringView},
    prelude::v1::{Box, HashMap, None as XLangNone, Pair, Some as XLangSome, Vec},
    targets::properties::{asm::AsmScalar, asm::AsmScalarKind, TargetProperties},
};
use xlang_struct::{
    AggregateDefinition, AsmConstraint, BinaryOp, Block, BlockItem, BranchCondition, Expr, File,
    FunctionDeclaration, LValueOp, OverflowBehaviour, Path, PointerKind, PointerType, ScalarType,
    ScalarTypeHeader, ScalarTypeKind, StackItem, StackValueKind, StaticDefinition, Type,
    UnaryLValueOp, UnaryOp, Value,
};

struct TypeState {
    tys: HashMap<Path, Type>,
    aggregate: HashMap<Path, Option<AggregateDefinition>>,
    target_properties: &'static TargetProperties<'static>,
    constraint_name_cache: RefCell<HashMap<String, HashSet<AsmScalar>>>,
    register_aliases: RefCell<HashMap<String, Vec<StringView<'static>>>>,
}

impl TypeState {
    pub fn constraint_allowed_for_type(&self, ty: &ScalarType, constraint: &str) -> bool {
        let asm_ty_kind = match ty.kind {
            ScalarTypeKind::Integer { .. }
            | ScalarTypeKind::Char { .. }
            | ScalarTypeKind::Fixed { .. }
                if ty.header.vectorsize.is_some() =>
            {
                AsmScalarKind::VectorInt
            }
            ScalarTypeKind::Integer { .. }
            | ScalarTypeKind::Char { .. }
            | ScalarTypeKind::Fixed { .. } => AsmScalarKind::Integer,
            ScalarTypeKind::Float { .. } | ScalarTypeKind::LongFloat
                if ty.header.vectorsize.is_some() =>
            {
                AsmScalarKind::VectorFloat
            }
            ScalarTypeKind::Float { .. } | ScalarTypeKind::LongFloat => AsmScalarKind::Float,
            ScalarTypeKind::Empty => panic!("Empty scalar type"),
        };

        let total_size = if let XLangSome(vectorsize) = ty.header.vectorsize {
            u32::from(ty.header.bitsize) * u32::from(vectorsize)
        } else {
            u32::from(ty.header.bitsize)
        };

        let asm_ty = AsmScalar(asm_ty_kind, total_size);

        let constraint_name_cache = self.constraint_name_cache.borrow();

        if let Some(validtys) = constraint_name_cache.get(constraint) {
            validtys.contains(&asm_ty)
        } else {
            drop(constraint_name_cache);
            let register_aliases = self.register_aliases.borrow();

            if let Some(classes) = register_aliases.get(constraint) {
                for rconstraint in classes {
                    if self.constraint_allowed_for_type(ty, rconstraint) {
                        return true;
                    }
                }
                false
            } else {
                drop(register_aliases);
                let mut found = false;
                let mut valid_tys = HashSet::<AsmScalar>::new();
                for Pair(name, sty) in self.target_properties.arch.asm_propreties.constraints {
                    if name == constraint {
                        found = true;
                        if AsmScalarKind::Vector == sty.0 {
                            let bitsize = sty.1;
                            let _ =
                                valid_tys.insert(AsmScalar(AsmScalarKind::VectorFloat, bitsize));
                            let _ = valid_tys.insert(AsmScalar(AsmScalarKind::VectorInt, bitsize));
                        } else {
                            let _ = valid_tys.insert(*sty);
                        }
                    }
                }

                if found {
                    let mut constraint_name_cache = self.constraint_name_cache.borrow_mut();
                    let valid = valid_tys.contains(&asm_ty);
                    constraint_name_cache.insert(constraint.into(), valid_tys);
                    valid
                } else {
                    let mut class_list = Vec::new();
                    for Pair(class, regnames) in
                        self.target_properties.arch.asm_propreties.register_groups
                    {
                        for reg in regnames {
                            if reg == constraint {
                                class_list.push(*class);
                                break;
                            }
                        }
                    }

                    let mut valid = false;
                    for rconstraint in &class_list {
                        if self.constraint_allowed_for_type(ty, rconstraint) {
                            valid = true;
                            break;
                        }
                    }
                    let mut register_aliases = self.register_aliases.borrow_mut();

                    register_aliases.insert(constraint.into(), class_list);
                    valid
                }
            }
        }
    }
    pub fn get_field_type<'a>(&'a self, ty: &'a Type, name: &str) -> Option<&'a Type> {
        match ty {
            Type::TaggedType(_, ty) | Type::Aligned(_, ty) => self.get_field_type(ty, name),
            Type::Product(tys) => {
                let v = name.parse::<usize>().unwrap();
                Some(&tys[v])
            }
            Type::Aggregate(n) => Some(
                n.fields
                    .iter()
                    .filter(|Pair(s, _)| s == name)
                    .map(|Pair(_, ty)| ty)
                    .next()
                    .unwrap(),
            ),
            Type::Named(p) => self.aggregate.get(p).map(Option::as_ref).map_or_else(
                || self.get_field_type(&self.tys[p], name),
                |ag| {
                    Some(
                        ag.unwrap()
                            .fields
                            .iter()
                            .filter(|Pair(s, _)| s == name)
                            .map(|Pair(_, ty)| ty)
                            .next()
                            .unwrap(),
                    )
                },
            ),
            _ => None,
        }
    }

    pub fn refiy_type<'a>(&'a self, ty: &'a Type) -> &'a Type {
        match ty {
            Type::Named(p) => self.tys.get(p).unwrap_or(ty),
            Type::TaggedType(_, ty) | Type::Aligned(_, ty) => ty,
            ty => ty,
        }
    }
}

fn tycheck_function(x: &mut FunctionDeclaration, tys: &TypeState) {
    if let FunctionDeclaration {
        ty,
        body: XLangSome(body),
    } = x
    {
        let local_tys = ty
            .params
            .iter()
            .cloned()
            .chain(body.locals.iter().cloned())
            .collect::<Vec<_>>();
        let mut ret = None;
        tycheck_block(&mut body.block, tys, &local_tys, &mut ret);
        let ret = ret.unwrap_or_default();
        if ty.ret == Type::Void {
            assert_eq!(ret.len(), 0);
        } else {
            assert_eq!(ret.len(), 1);
            check_unify(&ret[0].ty, &ty.ret, tys);
        }
    }
}

#[allow(clippy::too_many_lines, clippy::similar_names)]
#[track_caller]
fn check_unify(ty1: &Type, ty2: &Type, type_state: &TypeState) {
    match (type_state.refiy_type(ty1), type_state.refiy_type(ty2)) {
        (Type::Null, _) | (_, Type::Null) | (Type::Void, Type::Void) => {}
        (Type::Scalar(sty1), Type::Scalar(sty2)) => {
            assert_eq!(
                sty1.header.bitsize, sty2.header.bitsize,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );
            assert_eq!(
                sty1.header.vectorsize, sty2.header.vectorsize,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );

            match (sty1.kind, sty2.kind) {
                (
                    ScalarTypeKind::Integer {
                        signed: signed1, ..
                    },
                    ScalarTypeKind::Integer {
                        signed: signed2, ..
                    },
                ) => assert_eq!(
                    signed1, signed2,
                    "cannot unify types {:?} and {:?}",
                    ty1, ty2
                ),
                (
                    ScalarTypeKind::Fixed {
                        fractbits: fractbits1,
                    },
                    ScalarTypeKind::Fixed {
                        fractbits: fractbits2,
                    },
                ) => assert_eq!(
                    fractbits1, fractbits2,
                    "cannot unify types {:?} and {:?}",
                    ty1, ty2
                ),
                (
                    ScalarTypeKind::Char { flags: flags1 },
                    ScalarTypeKind::Char { flags: flags2 },
                ) => assert_eq!(flags1, flags2, "cannot unify types {:?} and {:?}", ty1, ty2),
                (
                    ScalarTypeKind::Float { decimal: dec1 },
                    ScalarTypeKind::Float { decimal: dec2 },
                ) => assert_eq!(dec1, dec2, "cannot unify types {:?} and {:?}", ty1, ty2),
                (ScalarTypeKind::LongFloat, ScalarTypeKind::LongFloat) => {}
                (_, _) => panic!("cannot unify types {:?} and {:?}", ty1, ty2),
            }
        }
        (Type::FnType(fnty1), Type::FnType(fnty2)) => {
            assert_eq!(
                fnty1.tag, fnty2.tag,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );

            assert_eq!(
                fnty1.variadic, fnty2.variadic,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );

            assert_eq!(
                fnty1.params.len(),
                fnty2.params.len(),
                "cannot unify types {:?} and {:?}",
                ty1,
                ty2
            );

            fnty1
                .params
                .iter()
                .zip(&fnty2.params)
                .for_each(|(ty1, ty2)| check_unify(ty1, ty2, type_state));
            check_unify(&fnty1.ret, &fnty2.ret, type_state);
        }
        (Type::Pointer(pty1), Type::Pointer(pty2)) => {
            check_unify(&pty1.inner, &pty2.inner, type_state);
        }
        (Type::Product(tys1), Type::Product(tys2)) => tys1
            .iter()
            .zip(tys2)
            .for_each(|(ty1, ty2)| check_unify(ty1, ty2, type_state)),
        (Type::Aggregate(defn1), Type::Aggregate(defn2)) => {
            assert_eq!(
                defn1.kind, defn2.kind,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );

            assert_eq!(
                defn1.annotations, defn2.annotations,
                "cannot unify types {:?} and {:?}",
                ty1, ty2
            );

            defn1.fields.iter().zip(&defn2.fields).for_each(
                |(Pair(name1, ty1), Pair(name2, ty2))| {
                    check_unify(ty1, ty2, type_state);
                    assert_eq!(name1, name2, "cannot unify types {:?} and {:?}", ty1, ty2);
                },
            );
        }
        (Type::Array(arr1), Type::Array(arr2)) => match (&arr1.len, &arr2.len) {
            (
                Value::Integer {
                    ty: sty1,
                    val: val1,
                },
                Value::Integer {
                    ty: sty2,
                    val: val2,
                },
            ) => {
                check_unify(&Type::Scalar(*sty1), &Type::Scalar(*sty2), type_state);
                assert_eq!(val1, val2, "Cannot unify types {:?} and {:?}", ty1, ty2);
            }
            (_, _) => panic!("Cannot unify types {:?} and {:?}", ty1, ty2),
        },
        (Type::Named(n1), Type::Named(n2)) => {
            assert_eq!(n1, n2, "Cannot unify types {:?} and {:?}", ty1, ty2);
        }
        (ty1, ty2) => panic!("Cannot unify types {:?} and {:?}", ty1, ty2),
    }
}

#[track_caller]
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

#[allow(
    clippy::too_many_lines,
    clippy::cognitive_complexity,
    clippy::similar_names
)] // What does clippy want, for me to change the value kinds in xir
fn tycheck_expr(
    expr: &mut Expr,
    locals: &[Type],
    exit: &mut Option<Vec<StackItem>>,
    vstack: &mut Vec<StackItem>,
    targets: &HashMap<u32, Vec<StackItem>>,
    tys: &TypeState,
) -> bool {
    eprint!("Typechecking expr {} against stack [", expr);
    let mut sep = "";
    for item in &*vstack {
        eprint!("{}{}", sep, item);
        sep = ", ";
    }
    eprintln!("]");
    match expr {
        xlang_struct::Expr::Const(v) => match v {
            xlang_struct::Value::Invalid(ty)
            | xlang_struct::Value::String { ty, .. }
            | xlang_struct::Value::Uninitialized(ty) => vstack.push(StackItem {
                ty: ty.clone(),
                kind: StackValueKind::RValue,
            }),
            xlang_struct::Value::GenericParameter(_) => todo!(),
            xlang_struct::Value::Integer { ty, .. } => vstack.push(StackItem {
                ty: Type::Scalar(*ty),
                kind: StackValueKind::RValue,
            }),
            xlang_struct::Value::GlobalAddress { ty, item } => {
                if let Type::Null = ty {
                    if let Some(t) = tys.tys.get(item) {
                        *ty = t.clone();
                    }
                }

                vstack.push(StackItem {
                    ty: Type::Pointer(PointerType {
                        inner: Box::new(ty.clone()),
                        ..PointerType::default()
                    }),
                    kind: StackValueKind::RValue,
                });
            }
            xlang_struct::Value::ByteString { .. } => vstack.push(StackItem {
                ty: Type::Pointer(PointerType {
                    inner: Box::new(Type::Scalar(ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 8,
                            ..ScalarTypeHeader::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: XLangNone,
                            max: XLangNone,
                        },
                    })),
                    ..PointerType::default()
                }),
                kind: StackValueKind::RValue,
            }),
            xlang_struct::Value::LabelAddress(n) => {
                assert_eq!(targets[n].len(), 0);
                vstack.push(StackItem {
                    ty: Type::Pointer(PointerType {
                        inner: Box::new(Type::Void),
                        ..PointerType::default()
                    }),
                    kind: StackValueKind::RValue,
                });
            }
        },
        xlang_struct::Expr::Exit { values } => {
            let pos = vstack.len().checked_sub(*values as usize).unwrap();
            let stack = vstack.split_off(pos);
            match exit {
                None => *exit = Some(stack),
                Some(items) => {
                    assert!(items.len() == (*values).into());
                    check_unify_stack(&stack, items, tys);
                }
            }
            return true;
        }
        xlang_struct::Expr::BinaryOp(op, v) => {
            let val1 = vstack.pop().unwrap();
            let val2 = vstack.pop().unwrap();

            assert_eq!(val1.kind, StackValueKind::RValue);
            assert_eq!(val2.kind, StackValueKind::RValue);
            match tys.refiy_type(&val1.ty) {
                Type::Scalar(_) => match *op {
                    BinaryOp::CmpInt | BinaryOp::Cmp => vstack.push(StackItem {
                        ty: Type::Scalar(ScalarType {
                            header: ScalarTypeHeader {
                                bitsize: 32,
                                ..ScalarTypeHeader::default()
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
                                    ..ScalarTypeHeader::default()
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
                                        ..ScalarTypeHeader::default()
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
        xlang_struct::Expr::UnaryOp(op, v) => {
            let val = vstack.pop().unwrap();

            assert_eq!(
                val.kind,
                StackValueKind::RValue,
                "Cannot apply {:?} to {:?}",
                op,
                val
            );

            match &val.ty {
                Type::Scalar(_) => match *op {
                    UnaryOp::Minus => {
                        vstack.push(val);
                        if *v == OverflowBehaviour::Checked {
                            vstack.push(StackItem {
                                ty: Type::Scalar(ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 1,
                                        ..ScalarTypeHeader::default()
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
                    }
                    UnaryOp::LogicNot => {
                        vstack.push(StackItem {
                            ty: Type::Scalar(ScalarType {
                                header: ScalarTypeHeader {
                                    bitsize: 1,
                                    ..ScalarTypeHeader::default()
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
                    UnaryOp::BitNot => {
                        vstack.push(val);
                    }
                    op => panic!("Cannot apply {:?}", op),
                },
                ty => panic!("Cannot apply {:?} to {:?}", op, ty),
            }
        }
        xlang_struct::Expr::Tailcall(_) => todo!("tailcall"),
        xlang_struct::Expr::CallFunction(fnty) => {
            let params = vstack.split_off_back(fnty.params.len());
            let dest = vstack.pop().unwrap();

            assert!(!fnty.variadic, "Cannot call with variadic signature");
            assert_eq!(dest.kind, StackValueKind::RValue, "Cannot call {:?}", dest);

            let destty = dest.ty;

            match tys.refiy_type(&destty) {
                Type::Pointer(ty) => match tys.refiy_type(&ty.inner) {
                    Type::FnType(ty) => {
                        if ty.variadic {
                            assert!(ty.params.len() <= fnty.params.len());
                        } else {
                            assert!(fnty.params.len() == fnty.params.len());
                        }

                        ty.params
                            .iter()
                            .zip(&fnty.params)
                            .for_each(|(ty1, ty2)| check_unify(ty1, ty2, tys));

                        check_unify(&ty.ret, &fnty.ret, tys);

                        params.iter().zip(&fnty.params).for_each(|(ty1, ty2)| {
                            assert_eq!(
                                ty1.kind,
                                StackValueKind::RValue,
                                "Cannot pass {:?} to {:?}",
                                ty1,
                                fnty
                            );
                            check_unify(&ty1.ty, ty2, tys);
                        });
                    }
                    ty => panic!("Cannot call {:?}", ty),
                },
                ty => panic!("Cannot call {:?}", ty),
            }

            vstack.push(StackItem {
                kind: StackValueKind::RValue,
                ty: fnty.ret.clone(),
            });
        }
        xlang_struct::Expr::Branch { cond, target } => {
            match *cond {
                BranchCondition::Always | BranchCondition::Never => {}
                _ => {
                    let ctrl = vstack.pop().unwrap();
                    assert_eq!(
                        ctrl.kind,
                        StackValueKind::RValue,
                        "Cannot branch on {:?}",
                        ctrl
                    );

                    match tys.refiy_type(&ctrl.ty) {
                        Type::Scalar(ScalarType {
                            kind: ScalarTypeKind::Integer { .. },
                            ..
                        }) => {}
                        ty => panic!("Cannot branch on {:?}", ty),
                    }
                }
            }

            let tstack = &targets[target];
            check_unify_stack(vstack, tstack, tys);

            if *cond == BranchCondition::Always {
                vstack.clear();
                return true;
            }
        }
        xlang_struct::Expr::BranchIndirect => {
            let target = vstack.pop().unwrap();
            assert_eq!(
                target.kind,
                StackValueKind::RValue,
                "Cannot branch to {:?}",
                target
            );

            check_unify(
                &target.ty,
                &Type::Pointer(PointerType {
                    inner: Box::new(Type::Void),
                    ..PointerType::default()
                }),
                tys,
            );
        }
        xlang_struct::Expr::Convert(str, ty) => {
            let val = vstack.pop().unwrap();
            assert_eq!(val.kind, StackValueKind::RValue, "Cannot convert {:?}", val);
            match str {
                xlang_struct::ConversionStrength::Strong => todo!("strong convert"),
                xlang_struct::ConversionStrength::Weak => todo!("weak convert"),
                xlang_struct::ConversionStrength::Reinterpret => match (&*ty, &val.ty) {
                    (
                        Type::Pointer(_)
                        | Type::Scalar(ScalarType {
                            kind: ScalarTypeKind::Integer { .. },
                            ..
                        }),
                        Type::Pointer(_)
                        | Type::Scalar(ScalarType {
                            kind: ScalarTypeKind::Integer { .. },
                            ..
                        }),
                    ) => {}
                    (ty1, ty2) => panic!("Cannot convert between {} and {}", ty1, ty2),
                },
            }
            vstack.push(StackItem {
                ty: ty.clone(),
                kind: StackValueKind::RValue,
            });
        }
        xlang_struct::Expr::Derive(pty, expr) => {
            tycheck_expr(expr, locals, exit, vstack, targets, tys);
            let mut ptr = vstack.pop().unwrap();
            assert_eq!(ptr.kind, StackValueKind::RValue);
            match &mut ptr.ty {
                Type::Pointer(ptrty) => {
                    check_unify(&pty.inner, &ptrty.inner, tys);
                    ptrty.alias = pty.alias;
                    ptrty.valid_range = pty.valid_range;
                    ptrty.decl = pty.decl;
                }
                ty => panic!("Cannot apply derive to {:?}", ty),
            }
        }
        xlang_struct::Expr::Local(n) => vstack.push(StackItem {
            ty: locals[(*n) as usize].clone(),
            kind: StackValueKind::LValue,
        }),
        xlang_struct::Expr::Pop(n) => {
            let back = vstack.len().checked_sub((*n) as usize).unwrap();
            vstack.shrink(back);
        }
        xlang_struct::Expr::Dup(n) => {
            let back = vstack.len().checked_sub((*n) as usize).unwrap();
            let stack = vstack.split_off(back);
            vstack.extend(stack.clone());
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
                        ..PointerType::default()
                    };
                }
                ty => panic!("Cannot use member indirect {} on {:?}", name, ty),
            }

            vstack.push(val);
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
        xlang_struct::Expr::CompoundAssign(op, v, _) => {
            let rvalue = vstack.pop().unwrap();
            let lvalue = vstack.pop().unwrap();

            assert_eq!(rvalue.kind, StackValueKind::RValue);
            assert_eq!(lvalue.kind, StackValueKind::LValue);

            match *op {
                BinaryOp::Cmp
                | BinaryOp::CmpEq
                | BinaryOp::CmpLt
                | BinaryOp::CmpLe
                | BinaryOp::CmpGt
                | BinaryOp::CmpGe
                | BinaryOp::CmpNe => panic!("invalid op for compound_assign {}", op),
                _ => {}
            }

            match tys.refiy_type(&lvalue.ty) {
                Type::Scalar(_) => {
                    check_unify(&lvalue.ty, &rvalue.ty, tys);
                    if *v == OverflowBehaviour::Checked {
                        vstack.push(StackItem {
                            ty: Type::Scalar(ScalarType {
                                header: ScalarTypeHeader {
                                    bitsize: 1,
                                    ..ScalarTypeHeader::default()
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
                }
                Type::Pointer(_) => {
                    match *v {
                        OverflowBehaviour::Checked
                        | OverflowBehaviour::Saturate
                        | OverflowBehaviour::Trap => {
                            panic!("Cannot use overflow behaviour {} with a pointer type", v);
                        }
                        _ => {}
                    }
                    match *op {
                        BinaryOp::Add => match tys.refiy_type(&rvalue.ty) {
                            Type::Scalar(ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            }) => {}
                            ty => panic!("Cannot add {} and {}", lvalue.ty, ty),
                        },
                        BinaryOp::Sub => match tys.refiy_type(&rvalue.ty) {
                            Type::Scalar(ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            }) => {}
                            rty @ Type::Pointer(_) => check_unify(&lvalue.ty, rty, tys),
                            ty => panic!("Cannot subtract {} and {}", lvalue.ty, ty),
                        },
                        op => panic!("Cannot apply {} to {}", op, lvalue.ty),
                    }
                }
                ty => todo!("compound_assign {}: {:?}", op, ty),
            }
        }
        xlang_struct::Expr::FetchAssign(op, v, _) => {
            let rvalue = vstack.pop().unwrap();
            let lvalue = vstack.pop().unwrap();

            assert_eq!(rvalue.kind, StackValueKind::RValue);
            assert_eq!(lvalue.kind, StackValueKind::LValue);

            match *op {
                BinaryOp::Cmp
                | BinaryOp::CmpEq
                | BinaryOp::CmpLt
                | BinaryOp::CmpLe
                | BinaryOp::CmpGt
                | BinaryOp::CmpGe
                | BinaryOp::CmpNe => panic!("invalid op for compound_assign {}", op),
                _ => {}
            }

            vstack.push(StackItem {
                ty: rvalue.ty.clone(),
                kind: StackValueKind::LValue,
            });

            match tys.refiy_type(&lvalue.ty) {
                Type::Scalar(_) => {
                    check_unify(&lvalue.ty, &rvalue.ty, tys);
                    if *v == OverflowBehaviour::Checked {
                        vstack.push(StackItem {
                            ty: Type::Scalar(ScalarType {
                                header: ScalarTypeHeader {
                                    bitsize: 1,
                                    ..ScalarTypeHeader::default()
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
                }
                Type::Pointer(_) => {
                    match *v {
                        OverflowBehaviour::Checked
                        | OverflowBehaviour::Saturate
                        | OverflowBehaviour::Trap => {
                            panic!("Cannot use overflow behaviour {} with a pointer type", v);
                        }
                        _ => {}
                    }
                    match *op {
                        BinaryOp::Add => match tys.refiy_type(&rvalue.ty) {
                            Type::Scalar(ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            }) => {}
                            ty => panic!("Cannot add {} and {}", lvalue.ty, ty),
                        },
                        BinaryOp::Sub => match tys.refiy_type(&rvalue.ty) {
                            Type::Scalar(ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            }) => {}
                            rty @ Type::Pointer(_) => check_unify(&lvalue.ty, rty, tys),
                            ty => panic!("Cannot subtract {} and {}", lvalue.ty, ty),
                        },
                        op => panic!("Cannot apply {} to {}", op, lvalue.ty),
                    }
                }
                ty => todo!("compound_assign {}: {:?}", op, ty),
            }
        }
        xlang_struct::Expr::UnaryLValue(op, v, _) => match *op {
            UnaryLValueOp::PreDec | UnaryLValueOp::PreInc => {
                let lvalue = vstack.pop().unwrap();

                assert_eq!(lvalue.kind, StackValueKind::LValue);

                match tys.refiy_type(&lvalue.ty) {
                    Type::Scalar(_) => {
                        vstack.push(lvalue);
                        if *v == OverflowBehaviour::Checked {
                            vstack.push(StackItem {
                                ty: Type::Scalar(ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 1,
                                        ..ScalarTypeHeader::default()
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
                    }
                    ty => panic!("Cannot use {:?} on {:?}", op, ty),
                }
            }
            UnaryLValueOp::PostDec | UnaryLValueOp::PostInc => {
                let mut lvalue = vstack.pop().unwrap();

                assert_eq!(lvalue.kind, StackValueKind::LValue);

                match tys.refiy_type(&lvalue.ty) {
                    Type::Scalar(_) => {
                        if *v == OverflowBehaviour::Checked {
                            lvalue.kind = StackValueKind::RValue;
                            vstack.push(lvalue);
                            vstack.push(StackItem {
                                ty: Type::Scalar(ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 1,
                                        ..ScalarTypeHeader::default()
                                    },
                                    kind: ScalarTypeKind::Integer {
                                        signed: false,
                                        min: XLangNone,
                                        max: XLangNone,
                                    },
                                }),
                                kind: StackValueKind::RValue,
                            });
                        } else {
                            vstack.push(lvalue);
                        }
                    }
                    ty => panic!("Cannot use {:?} on {:?}", op, ty),
                }
            }
            op => panic!("Invalid lvalue operator {}", op),
        },
        xlang_struct::Expr::LValueOp(op, _) => match *op {
            LValueOp::Xchg => {
                let val1 = vstack.pop().unwrap();
                let val2 = vstack.pop().unwrap();

                assert_eq!(val1.kind, StackValueKind::LValue);
                assert_eq!(val2.kind, StackValueKind::LValue);

                check_unify(&val1.ty, &val2.ty, tys);
            }
            LValueOp::Cmpxchg | LValueOp::Wcmpxchg => {
                let control = vstack.pop().unwrap();
                let swap = vstack.pop().unwrap();
                let dest = vstack.pop().unwrap();

                assert_eq!(dest.kind, StackValueKind::LValue);
                assert_eq!(swap.kind, StackValueKind::LValue);
                assert_eq!(control.kind, StackValueKind::RValue);

                check_unify(&dest.ty, &control.ty, tys);
                check_unify(&dest.ty, &swap.ty, tys);

                vstack.push(StackItem {
                    ty: Type::Scalar(ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 1,
                            ..ScalarTypeHeader::default()
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
            op => todo!("{:?}", op),
        },
        xlang_struct::Expr::Indirect => {
            let mut val = vstack.pop().unwrap();

            assert_eq!(val.kind, StackValueKind::RValue);

            match val.ty {
                Type::Pointer(ptr) => {
                    val.ty = Box::into_inner(ptr.inner);
                    val.kind = StackValueKind::LValue;
                }
                ty => panic!("Cannot use indirect on {:?}", ty),
            }

            vstack.push(val);
        }
        xlang_struct::Expr::AddrOf => {
            let mut val = vstack.pop().unwrap();

            assert_eq!(val.kind, StackValueKind::LValue);

            val.ty = Type::Pointer(PointerType {
                inner: Box::new(val.ty),
                ..PointerType::default()
            });

            vstack.push(val);
        }
        xlang_struct::Expr::Sequence(_) | xlang_struct::Expr::Fence(_) => {}
        xlang_struct::Expr::Switch(switch) => {
            let ctrl = vstack.pop().unwrap();

            assert_eq!(
                ctrl.kind,
                StackValueKind::RValue,
                "Cannot switch on {:?}",
                ctrl
            );

            match tys.refiy_type(&ctrl.ty) {
                Type::Scalar(ScalarType {
                    kind: ScalarTypeKind::Integer { .. },
                    ..
                }) => {}
                ty => panic!("Cannot switch on {:?}", ty),
            }

            let mut ustack = None::<&Vec<StackItem>>;

            match switch {
                xlang_struct::Switch::Hash(h) => {
                    for Pair(val, targ) in &h.cases {
                        match val {
                            Value::Integer { ty, val: _ } => {
                                check_unify(&ctrl.ty, &Type::Scalar(*ty), tys);
                            }
                            val => panic!("Cannot switch on case {:?}", val),
                        }

                        let tstack = &targets[targ];

                        #[allow(clippy::option_if_let_else)]
                        if let Some(stack) = ustack {
                            assert_eq!(
                                stack.len(),
                                tstack.len(),
                                "Cannot unify stack for target @{} ({:?}) with switch stack {:?}",
                                targ,
                                tstack,
                                stack
                            );
                            stack.iter().zip(tstack).for_each(|(ty1,ty2)| {
                                assert_eq!(ty1.kind,ty2.kind,"Cannot unify stack for target @{} ({:?}) with switch stack {:?}",targ,tstack,stack);
                                check_unify(&ty1.ty,&ty2.ty,tys);
                            });
                        } else {
                            check_unify_stack(vstack, tstack, tys);
                            ustack = Some(tstack);
                        }
                    }
                }
                xlang_struct::Switch::Linear(_) => todo!(),
            }
        }
        Expr::Asm(asm) => {
            let syntax = &mut asm.syntax;

            if syntax.is_empty() {
                *syntax = tys.target_properties.arch.asm_propreties.syntax_names[0].into();
            } else {
                let mut valid_syntax_name = false;
                for syn in tys.target_properties.arch.asm_propreties.syntax_names {
                    if syn == syntax {
                        valid_syntax_name = true;
                        break;
                    }
                }
                assert!(
                    valid_syntax_name,
                    "Invalid assembly syntax name for target {}",
                    syntax
                );
            }

            for target in &asm.targets {
                let stack = &targets[target];

                assert!(
                    stack.is_empty(),
                    "Cannot list @{} as destination for `asm goto`",
                    target
                );
            }

            let inputs = &asm.inputs;

            let input_stack = vstack.split_off_back(inputs.len());

            for (constraint, val) in inputs.iter().zip(input_stack) {
                let name = match constraint {
                    AsmConstraint::Memory | AsmConstraint::CC => panic!(
                        "Invalid constraint {} (only available for clobbers list)",
                        constraint
                    ),
                    AsmConstraint::ArchConstraint(name) => name,
                };

                assert_eq!(val.kind, StackValueKind::RValue, "Invalid value {}", val);

                match &val.ty {
                    Type::Scalar(sty) => assert!(
                        tys.constraint_allowed_for_type(sty, name),
                        "Invalid value {}",
                        sty
                    ),
                    Type::Pointer(pty) => {
                        let bits = match (pty.kind, &*pty.inner) {
                            (PointerKind::Near, _) => tys.target_properties.primitives.nearptrbits,
                            (PointerKind::Far, _) => tys.target_properties.primitives.farptrbits,
                            (_, Type::FnType(_)) => tys.target_properties.primitives.fnptrbits,
                            _ => tys.target_properties.primitives.ptrbits,
                        };

                        let sty = ScalarType {
                            header: ScalarTypeHeader {
                                bitsize: bits,
                                ..Default::default()
                            },
                            kind: ScalarTypeKind::Integer {
                                signed: false,
                                min: XLangNone,
                                max: XLangNone,
                            },
                        };
                        assert!(
                            tys.constraint_allowed_for_type(&sty, name),
                            "Invalid value {}",
                            pty
                        );
                    }
                    ty => panic!("Invalid value {}", ty),
                }
            }

            for output in &asm.outputs {
                let constraint = &output.constraint;
                let ty = &output.ty;

                let name = match constraint {
                    AsmConstraint::Memory | AsmConstraint::CC => panic!(
                        "Invalid constraint {} (only available for clobbers list)",
                        constraint
                    ),
                    AsmConstraint::ArchConstraint(name) => name,
                };

                match ty {
                    Type::Scalar(sty) => assert!(
                        tys.constraint_allowed_for_type(sty, name),
                        "Invalid value {} for constraint {}",
                        sty,
                        name
                    ),
                    Type::Pointer(pty) => {
                        let bits = match (pty.kind, &*pty.inner) {
                            (PointerKind::Near, _) => tys.target_properties.primitives.nearptrbits,
                            (PointerKind::Far, _) => tys.target_properties.primitives.farptrbits,
                            (_, Type::FnType(_)) => tys.target_properties.primitives.fnptrbits,
                            _ => tys.target_properties.primitives.ptrbits,
                        };

                        let sty = ScalarType {
                            header: ScalarTypeHeader {
                                bitsize: bits,
                                ..Default::default()
                            },
                            kind: ScalarTypeKind::Integer {
                                signed: false,
                                min: XLangNone,
                                max: XLangNone,
                            },
                        };
                        assert!(
                            tys.constraint_allowed_for_type(&sty, name),
                            "Invalid value {} for constraint {}",
                            pty,
                            name
                        );
                    }
                    ty => panic!("Invalid value {} {}", ty, name),
                }

                vstack.push(StackItem {
                    ty: ty.clone(),
                    kind: StackValueKind::RValue,
                });
            }
        }
        Expr::BeginStorage(n) | Expr::EndStorage(n) => {
            assert!(locals.len() > (*n as usize));
        }
        Expr::Select(n) => todo!("select {}",n),
    }
    false
}

fn tycheck_block(
    block: &mut Block,
    tys: &TypeState,
    locals: &[Type],
    exit: &mut Option<Vec<StackItem>>,
) {
    let mut vstack = Vec::new();

    let mut targets = HashMap::<_, _>::new();
    let mut diverged = false;

    for item in &block.items {
        if let BlockItem::Target { num, stack } = item {
            assert!(
                targets.insert(*num, stack.clone()).is_none(),
                "Target @{} redeclared",
                num
            );
        }
    }

    for item in &mut block.items {
        match item {
            BlockItem::Expr(expr) => {
                if !diverged {
                    diverged = tycheck_expr(expr, locals, exit, &mut vstack, &targets, tys);
                }
            }
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
}

pub fn tycheck(x: &mut File) {
    let targ = xlang::targets::properties::get_properties(x.target.clone()).unwrap();
    let mut typestate = TypeState {
        tys: HashMap::new(),
        aggregate: HashMap::new(),
        target_properties: targ,
        constraint_name_cache: RefCell::new(HashMap::new()),
        register_aliases: RefCell::new(HashMap::new()),
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
                eprintln!("Encountered Aggregate {} ({})", path, defn);
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
