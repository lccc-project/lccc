use core::cmp::Eq;
use core::hash::Hash;

use xlang::{
    abi::{collection::HashMap, pair::Pair},
    ir::{ArrayType, FnType, ScalarType, Type},
};

use crate::ty::{AggregateLayout, FlattenFieldsOf, TypeInformation};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ParamPosition {
    First,
    Last,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CallConvLocation<R> {
    Register(R),
    Indirect(R),
    StackOffset(i32),
    Split(Vec<CallConvLocation<R>>),
    Null,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ReturnPointerBehaviour<R, C> {
    Dedicated(R),
    Param(ParamPosition, C),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum RegisterDisposition {
    Interleave,
    Consume,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StackedParamsOrder {
    Rtl,
    Ltr,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ClassifyAggregateDisposition<C> {
    Single(C),
    Recursive,
    SplitFlat(u64),
}

pub trait Tag {
    type Register: Clone + Eq;
    type TypeClass;
    fn tag_name(&self) -> &'static str;

    fn param_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register];

    fn return_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register];

    fn replace_param_with_pointer(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass>;

    fn combine_wide(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass>;

    fn replace_return_with_pointer(
        &self,
        cl: &[Self::TypeClass],
    ) -> Option<ReturnPointerBehaviour<Self::Register, Self::TypeClass>>;

    fn replace_class_as_varargs(&self, cl: &Self::TypeClass) -> Option<Self::TypeClass>;

    fn register_disposition(&self, cl: &Self::TypeClass) -> RegisterDisposition;

    fn stacked_params_order(&self) -> StackedParamsOrder;
}

pub trait CallConvInfo {
    type Tag: Tag<TypeClass = Self::TypeClass, Register = Self::Register>;
    type TypeClass: Eq + Hash + Clone;
    type Register: Eq + Clone;
    fn get_tag(&self, tag: &str) -> Self::Tag;

    fn no_class(&self) -> Self::TypeClass;
    fn classify_scalar(&self, sty: ScalarType) -> Vec<Self::TypeClass>;
    fn classify_pointer(&self) -> Self::TypeClass;
    fn classify_aggregate_disposition(&self) -> ClassifyAggregateDisposition<Self::TypeClass>;

    fn merge_class(&self, left: Self::TypeClass, right: Self::TypeClass) -> Self::TypeClass;
    fn adjust_classes_after_combine(&self, classes: &mut [Self::TypeClass]);
}

pub struct CallConv<R, T> {
    params: Vec<CallConvLocation<R>>,
    stacked_params_count: u32,
    ret_location: CallConvLocation<R>,
    tag: T,
    is_varargs: bool,
}

impl<R, T> CallConv<R, T> {
    pub fn tag(&self) -> &T {
        &self.tag
    }

    pub fn params(&self) -> &[CallConvLocation<R>] {
        &self.params
    }

    pub fn ret_location(&self) -> &CallConvLocation<R> {
        &self.ret_location
    }

    pub fn stacked_params_count(&self) -> u32 {
        self.stacked_params_count
    }

    pub fn is_varargs(&self) -> bool {
        self.is_varargs
    }
}

pub fn classify_type<I: CallConvInfo>(
    info: &I,
    ty: &Type,
    tys: &TypeInformation,
) -> Vec<I::TypeClass> {
    let mut classes = Vec::new();
    match ty {
        Type::Null | Type::Void => classes.push(info.no_class()),
        Type::Scalar(sty) => classes.extend(info.classify_scalar(*sty)),
        Type::FnType(_) => panic!("Cannot classify a non-value type (other than void)"),
        Type::Pointer(_) => classes.push(info.classify_pointer()),
        Type::Array(_) => todo!(),
        Type::TaggedType(_, ty) => classes = classify_type(info, ty, tys),
        Type::Product(_) => todo!(),
        Type::Aligned(_, _) => todo!(),
        Type::Aggregate(_) => todo!(),
        Type::Named(_) => todo!(),
    }

    info.adjust_classes_after_combine(&mut classes);

    classes
}

pub fn compute_call_conv<I: CallConvInfo>(
    info: &I,
    real_ty: &FnType,
    call_ty: &FnType,
    tys: &TypeInformation,
) -> CallConv<I::Register, I::Tag> {
    let tag = info.get_tag(&real_ty.tag);

    let is_varargs = real_ty.variadic;
    let fixed_arity_params_count = real_ty.params.len();

    let mut param_register_by_class = HashMap::<_, _>::new();

    let mut return_register_by_class = HashMap::<_, _>::new();

    let return_classes = classify_type(info, &real_ty.ret, tys);

    let mut return_loc = None;

    let mut total_params_consumed = 0;

    let mut param_locs = Vec::new();

    let no_class = info.no_class();

    if let Some(combined) = tag.combine_wide(&return_classes) {
        if combined == no_class {
            return_loc = Some(CallConvLocation::Null);
        } else {
            let regs = return_register_by_class
                .get_or_insert_with_mut(combined, |cl| tag.return_regs_for_class(cl));

            if let Some(val) = regs.first() {
                return_loc = Some(CallConvLocation::Register(val.clone()));
            } else {
                panic!("Wide register return is non-existant")
            }
        }
    } else if let Some(targ) = tag.replace_return_with_pointer(&return_classes) {
        match targ {
            ReturnPointerBehaviour::Dedicated(reg) => {
                return_loc = Some(CallConvLocation::Register(reg))
            }
            ReturnPointerBehaviour::Param(ParamPosition::First, cl) => {
                let regs = param_register_by_class
                    .get_or_insert_with_mut(cl.clone(), |cl| tag.param_regs_for_class(cl));
                let reg = match tag.register_disposition(&cl) {
                    RegisterDisposition::Interleave => {
                        if let Some((l, r)) = regs.split_first() {
                            let l = l.clone();
                            *regs = r;
                            Some(l)
                        } else {
                            None
                        }
                    }
                    RegisterDisposition::Consume => {
                        if let Some(val) = regs.get(total_params_consumed) {
                            total_params_consumed += 1;
                            Some(val.clone())
                        } else {
                            None
                        }
                    }
                };

                if let Some(reg) = reg {
                    return_loc = Some(CallConvLocation::Indirect(reg))
                } else {
                    todo!("return in memory on stack")
                }
            }
            ReturnPointerBehaviour::Param(_, cl) => todo!("non-start return pointer"),
        }
    } else {
        let mut split_locs = vec![];

        for cl in return_classes {
            if cl == no_class {
                split_locs.push(CallConvLocation::Null);
            } else {
                let regs = return_register_by_class
                    .get_or_insert_with_mut(cl, |cl| tag.return_regs_for_class(cl));

                if let Some((reg, rest)) = regs.split_first() {
                    split_locs.push(CallConvLocation::Register(reg.clone()));
                    *regs = rest;
                } else {
                    panic!("Register return is non-existant")
                }
            }
        }

        if split_locs.is_empty() {
            return_loc = Some(CallConvLocation::Null);
        } else if split_locs.len() == 1 {
            let val = split_locs.pop().unwrap();
            return_loc = Some(val);
        } else {
            return_loc = Some(CallConvLocation::Split(split_locs));
        }
    }

    for (pos, param) in call_ty.params.iter().enumerate() {
        let class = classify_type(info, param, tys);
        if let Some(combined) = tag.combine_wide(&class) {
            let class = if pos >= fixed_arity_params_count {
                if let Some(replace) = tag.replace_class_as_varargs(&combined) {
                    replace
                } else {
                    combined
                }
            } else {
                combined
            };
            let regs = param_register_by_class
                .get_or_insert_with_mut(class.clone(), |cl| tag.param_regs_for_class(cl));
            let reg = match tag.register_disposition(&class) {
                RegisterDisposition::Interleave => {
                    if let Some((l, r)) = regs.split_first() {
                        let l = l.clone();
                        *regs = r;
                        Some(l)
                    } else {
                        None
                    }
                }
                RegisterDisposition::Consume => {
                    if let Some(val) = regs.get(total_params_consumed) {
                        total_params_consumed += 1;
                        Some(val.clone())
                    } else {
                        None
                    }
                }
            };

            if let Some(reg) = reg {
                param_locs.push(CallConvLocation::Register(reg))
            } else {
                todo!("stack")
            }
        } else if let Some(ptr) = tag.replace_param_with_pointer(&class) {
            let class = if pos >= fixed_arity_params_count {
                if let Some(replace) = tag.replace_class_as_varargs(&ptr) {
                    replace
                } else {
                    ptr
                }
            } else {
                ptr
            };
            let regs = param_register_by_class
                .get_or_insert_with_mut(class.clone(), |cl| tag.param_regs_for_class(cl));
            let reg = match tag.register_disposition(&class) {
                RegisterDisposition::Interleave => {
                    if let Some((l, r)) = regs.split_first() {
                        let l = l.clone();
                        *regs = r;
                        Some(l)
                    } else {
                        None
                    }
                }
                RegisterDisposition::Consume => {
                    if let Some(val) = regs.get(total_params_consumed) {
                        total_params_consumed += 1;
                        Some(val.clone())
                    } else {
                        None
                    }
                }
            };

            if let Some(reg) = reg {
                param_locs.push(CallConvLocation::Indirect(reg))
            } else {
                todo!("stack")
            }
        } else {
            let mut split_locs = Vec::new();
            for class in class {
                if class == no_class {
                    split_locs.push(CallConvLocation::Null);
                } else {
                    let class = if pos >= fixed_arity_params_count {
                        if let Some(replace) = tag.replace_class_as_varargs(&class) {
                            replace
                        } else {
                            class
                        }
                    } else {
                        class
                    };
                    let regs = param_register_by_class
                        .get_or_insert_with_mut(class.clone(), |cl| tag.param_regs_for_class(cl));
                    let reg = match tag.register_disposition(&class) {
                        RegisterDisposition::Interleave => {
                            if let Some((l, r)) = regs.split_first() {
                                let l = l.clone();
                                *regs = r;
                                Some(l)
                            } else {
                                None
                            }
                        }
                        RegisterDisposition::Consume => {
                            if let Some(val) = regs.get(total_params_consumed) {
                                total_params_consumed += 1;
                                Some(val.clone())
                            } else {
                                None
                            }
                        }
                    };

                    if let Some(reg) = reg {
                        param_locs.push(CallConvLocation::Register(reg))
                    } else {
                        todo!("stack")
                    }
                }
            }

            if split_locs.is_empty() {
                param_locs.push(CallConvLocation::Null);
            } else if split_locs.len() == 1 {
                let val = split_locs.pop().unwrap();
                param_locs.push(val);
            } else {
                param_locs.push(CallConvLocation::Split(split_locs));
            }
        }
    }
    let return_loc = return_loc.expect("We have a return type, right");
    drop(return_register_by_class); // These are here for Borrowck: Now I'm missing the dropck_eyepatch on drop code for `HashMap`
    drop(param_register_by_class);
    CallConv {
        params: param_locs,
        stacked_params_count: 0,
        ret_location: return_loc,
        tag,
        is_varargs,
    }
}
