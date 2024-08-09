use core::cmp::Eq;
use core::hash::Hash;

use xlang::{
    abi::collection::HashMap,
    ir::{FnType, PointerKind, ScalarType, Type},
};

use crate::ty::TypeInformation;

/// The position in the parameters list to insert a location
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum ParamPosition {
    /// Inserts the location in the first parameter location
    First,
    /// Inserts the location in the last parameter location
    Last,
}

/// The location of a parameter or return value
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CallConvLocation<R> {
    /// Passed in a register (which may be a synthetic register)
    Register(R),
    /// Passed indirectly, with a pointer present in the given register
    Indirect(R),
    /// Passed on the stack, starting at the given displacement from the start of the parameter area.
    StackOffset(i32),
    /// Passed in multiple [`CallConvLocation`]s, typically registers.
    Split(Vec<CallConvLocation<R>>),
    /// An empty value passed in no location
    Null,
}

/// Describes the location of a return pointer passed to a function
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum ReturnPointerBehaviour<R, C> {
    /// Specifies that a dedicated register is used. The [`Tag`] must ensure that this does not conflict with any register used by parameters
    Dedicated(R),
    /// Specifies that the return pointer is passed as a parameter of the given class.
    Param(ParamPosition, C),
}

/// Describes the behaviour when passing multiple different classes of values
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum RegisterDisposition {
    /// Interleaves parameters of a given class with parameters of other classes
    ///
    /// With different classes mixing [`RegisterDisposition`]s, classes with [`RegisterDisposition::Interleave`] neither affect, nor are affected by parameter classes with [`RegisterDisposition::Consume`]
    Interleave,
    /// Consumes parameters positionally, jointly with other classes with [`RegisterDisposition::Consume`].
    ///
    /// With different classes mixing [`RegisterDisposition`]s, only paramaeter classes with [`RegisterDisposition::Consume`] affect the list of consumed registers shared by those classes.
    Consume,
}

/// Indicates the order that parameters passed on the stack are layed out (in the directionality of the stack)
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StackedParamsOrder {
    /// Indicates that the first slot for the parameter region is used by the rightmost parameter passed on the stack
    Rtl,
    /// Indicates that the first slot for the parameter region is used by the leftmost parameter passed on the stack
    Ltr,
}

/// The disposition of a [`Tag`] for classifying aggregate types
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum ClassifyAggregateDisposition<C> {
    /// Indicates that all aggregate types recieving the given class.
    Single(C),
    /// Classifies aggregate types by field, recursively, merging all fields to tag the whole aggregate one single class
    Recursive,
    /// Splits the aggregate type into chunks of the given size, and classifies each chunk according to contained scalar fields merging the classes in each chunk.
    SplitFlat(u64),
}

/// A trait for compiled calling convention [`Tag`]s.
/// [`Tag`]s decide how different classified values are ultimately passed or returned from functions, specifying which registers are used for which class, and when certain classes are replaced.
///
/// The [`Tag`] is expected to uphold certain invariants about type classes and dispositions it indicates via
///
pub trait Tag {
    /// The type of the registers used by the calling convention.
    type Register: Clone + Eq;
    /// The type of the classification (or partial classification) of individual values that can be passed or returned
    type TypeClass;

    /// The name of the tag, for display purposes
    fn tag_name(&self) -> &'static str;

    /// Returns the list of registers used for passing values (or parts thereof) of the specified `cl`, if any.
    ///
    /// It is a logic error if any two different type classes share the same (non-empty) lists of registers unless both are [`RegisterDisposition::Consume`],
    ///  or if any two different type classes use any of the same registers except as the same parameter position when both classes use [`RegisterDisposition::Consume`]
    fn param_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register];

    /// Returns the list of registers used for returning values (or parts thereof) of the specified `cl`, if any.
    fn return_regs_for_class(&self, cl: &Self::TypeClass) -> &[Self::Register];

    /// If the given list of classes should be passed indirectly, returns the type class of the pointer the parameter is replaced by.
    /// Otherwise, returns [`None`].
    ///
    /// It is a logic error of the implementation if this function returns a type class which, if given as the single input to this function, would return [`Some`].
    fn replace_param_with_pointer(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass>;

    /// If the given list of classes was generated by spliting a large value that can be passed as a single unit, returns the type class of the combined unit.
    /// Otherwise, returns [`None`].
    ///
    /// It is a logic error of the implementation if this function returns a type class which, if given as the single input to [`Tag::replace_param_with_pointer`] or [`Tag::replace_return_with_pointer`],
    ///  would return [`None`].
    fn combine_wide(&self, cl: &[Self::TypeClass]) -> Option<Self::TypeClass>;

    /// If the given list of type classes should be returned indirectly, returns the disposition for passing the return pointer to the function.
    /// Otherwise, returns [`None`].
    ///
    /// It is a logic error of the implementation if the function returns [`ReturnPointerBehaviour::Param`], and the class of the parameter specified,
    ///   if given as the single input to [`Tag::return_regs_for_class`] would yield a slice without 1 or more registers or, if passed to this function, would return [`Some`].
    ///
    /// It is further a logic error of the implementation if the function returns [`ReturnPointerBehaviour::Dedicated`], and the specified register collides with any register used for a parameter of any class.
    fn replace_return_with_pointer(
        &self,
        cl: &[Self::TypeClass],
    ) -> Option<ReturnPointerBehaviour<Self::Register, Self::TypeClass>>;

    /// If the given type class needs special handling when passed as a variadic parameter (parameter after the fixed airty parameter list),
    ///  returns the type class of the replaced segment.
    ///
    /// For parameters with multiple classes, each class is replaced individually.
    ///
    /// It is a logic error of the implementation if the function returns a type class which, when combined with the other classes for the full value and passed to [`Tag::replace_param_with_pointer`] would return [`Some`],
    ///  unless the function applied to the original set would return the same value.
    /// Note that checks for [`Tag::replace_param_with_pointer`] are performed before calling this function, so a [`Some`] value becoming [`None`] after this function is not necessarily respected (but it considered well-defined).
    fn replace_class_as_varargs(&self, cl: &Self::TypeClass) -> Option<Self::TypeClass>;

    /// Specifies how parameter parts of the given type class interact with other parameters passed in different type classes.
    ///
    /// Two options are provided:
    /// * [`RegisterDisposition::Interleave`] causes all such type classes to be disjoint - only parameters passed in registers of the same type class will affect the registers used for other parameters
    /// * [`RegisterDisposition::Consume`] causes a global (accross all parameters of the same call) list to be used, and parameters are assigned positionally.
    ///
    /// A [`Tag`] may specify a single register disposition for all type classes, or may specify mixed dispositions.
    /// The behaviour of mixed dispositions is described by the variants of [`RegisterDisposition`]
    fn register_disposition(&self, cl: &Self::TypeClass) -> RegisterDisposition;

    /// Specifies the order that parameters are placed on the stack after all registers are consumed
    fn stacked_params_order(&self) -> StackedParamsOrder;
}

/// A trait for information about calling conventions on a target.
///
/// As opposed to [`Tag`], [`CallConvInfo`] contains the information needed to perform initial classification of parameters/return values, as well as how to convert tag strings to [`Tag`] values.
pub trait CallConvInfo {
    /// The type of the [`Tag`] associated with the calling conventions.
    type Tag: Tag<TypeClass = Self::TypeClass, Register = Self::Register>;
    /// The type of parameter/return value classes used by the [`CallConvInfo::Tag`] and by value classification
    type TypeClass: Eq + Hash + Clone;
    /// The type of registers yielded by the [`CallConvInfo::Tag`]
    type Register: Eq + Clone;
    /// Maps the specified `tag` to the associated [`Tag`] value.
    ///
    /// ## Panics
    /// The implementation may panic (or otherwise return a nonsensical value) if the `tag` is invalid for the target.
    fn get_tag(&self, tag: &str) -> Self::Tag;

    /// Returns the [`CallConvInfo::TypeClass`] that an empty value (such as a portion of an aggregate that entirely consists of padding bytes) recieves.
    /// Anything classified as the return value will be ignored for parameter passing or return purposes (passed in [`CallConvLocation::Null`])
    fn no_class(&self) -> Self::TypeClass;

    /// Classifies the given scalar type, splitting it into sufficient individual pieces to allow passing it in registers.
    fn classify_scalar(&self, sty: ScalarType) -> Vec<Self::TypeClass>;
    /// Classifies pointer types of the given `width`.
    /// It is generally assumed that a pointer can allways be passed as a single value
    fn classify_pointer(&self, width: PointerKind) -> Self::TypeClass;
    /// Indicates how the [`classify_type`] function should classify aggregates.
    /// See documentation of [`ClassifyAggregateDisposition`] for details
    fn classify_aggregate_disposition(&self) -> ClassifyAggregateDisposition<Self::TypeClass>;

    /// Merges two type classes into a single class.
    ///
    /// It is a logic error of the implementation if any of the following properties do not hold:
    /// * Given the classes `a` and `b`, [`CallConvInfo::merge_class`]`(a,b)` is the same value as [`CallConvInfo::merge_class`]`(b,a)`
    /// * Given the classes `a` and `b`, if either `a` or `b` does not equal [`CallConvInfo::no_class`] , then [`CallConvInfo::merge_class`]`(a,b)` does not equal [`CallConvInfo::no_class`]`()`
    ///
    fn merge_class(&self, left: Self::TypeClass, right: Self::TypeClass) -> Self::TypeClass;

    /// Performs a final merge step after all pieces of value have been classified
    fn adjust_classes_after_combine(&self, classes: &mut [Self::TypeClass]);
}

/// A type that stores the result of computing the full calling convention of a function type.
pub struct CallConv<R, T> {
    params: Vec<CallConvLocation<R>>,
    stacked_params_count: u32,
    ret_location: CallConvLocation<R>,
    tag: T,
    is_varargs: bool,
}

impl<R, T> CallConv<R, T> {
    /// The computed [`Tag`] of the function type.
    pub fn tag(&self) -> &T {
        &self.tag
    }

    /// The full list of locations to place (or find) the parameters of the function.
    ///
    /// Each parameter in the "call type" (at the call site) or function type (at the def site) has exactly one element in order.
    pub fn params(&self) -> &[CallConvLocation<R>] {
        &self.params
    }

    /// Specifies the location to place (or find) the return value of the function.
    pub fn ret_location(&self) -> &CallConvLocation<R> {
        &self.ret_location
    }

    /// Specifies the number of different parameter classes that were passed on the stack.
    pub fn stacked_params_count(&self) -> u32 {
        self.stacked_params_count
    }

    /// Specifies whether or not the function was variadic.
    pub fn is_varargs(&self) -> bool {
        self.is_varargs
    }
}

/// Classifies the given [`Type`] according to `info`. Information about the types are derived from `tys`
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
        Type::Pointer(ptr) => classes.push(info.classify_pointer(ptr.kind)),
        Type::Array(_) => todo!(),
        Type::TaggedType(_, ty) => classes = classify_type(info, ty, tys),
        Type::Product(vals) => {
            if vals.len() != 0 {
                todo!("Aggregate types")
            }
        }
        Type::Aligned(_, _) => todo!(),
        Type::Aggregate(_) => todo!(),
        Type::Named(_) => todo!(),
    }

    info.adjust_classes_after_combine(&mut classes);

    classes
}

/// Computes the [`CallConv`] of a function call site (or def site) with the given [`CallConvInfo`] using `real_ty` and `call_ty`.
///
/// This function primarily computes calling convention for the call site of a function, with `real_ty` set to the type of the function being called,
///   and `call_ty` being the fn-type for the `call` or `tailcall` instruction when they differ (for example, for variadic parameters).
/// When computing the calling convention of a function inside that function, `real_ty` and `call_ty` should be set to the same value.
///
///
///
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

    #[allow(unused_assignments)] // ParamPosition::Last will be implemented later.
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
            ReturnPointerBehaviour::Param(_, _cl) => todo!("non-start return pointer"),
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
