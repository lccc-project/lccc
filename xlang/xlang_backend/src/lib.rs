#![deny(missing_docs, warnings)] // No clippy::nursery
//! A helper crate for implementing [`xlang::plugin::XLangCodegen`]s without duplicating code (also can be used to evaluate constant expressions)
//! the `xlang_backend` crate provides a general interface for writing expressions to an output.
use std::{cmp::Ordering, collections::VecDeque, fmt::Debug};

use self::str::Encoding;
use expr::{LValue, Trap, VStackValue, ValLocation};
use xlang::{
    abi::string::StringView,
    ir::{
        AccessClass, AggregateCtor, BinaryOp, Block, BranchCondition, Expr, FnType, FunctionBody,
        Path, PathComponent, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, StackItem,
        StackValueKind, Type, UnaryOp, Value,
    },
    prelude::v1::*,
    targets::properties::TargetProperties,
};

/// Module for handling and internalizing string literal values
pub mod str;

/// Module for handling the results of evaluating Expressions, and tracking the location of values
pub mod expr;

/// Module for handling types
pub mod ty;

/// Module for handling xlang/language intrinsics
pub mod intrinsic;

/// Module for handling calling convention, and calling functions
pub mod callconv;

/// Module for name mangling
pub mod mangle;

///
/// Basic Trait for creating the code generator
pub trait FunctionRawCodegen {
    /// The type for Locations
    type Loc: ValLocation;
    /// The type of the writer to pass to the [`FunctionRawCodegen::write_output`] function
    type Writer;
    /// The type of errors returned from the [`FunctionRawCodegen::write_output`] function
    type Error: Debug;

    /// Handles the `__lccc::xlang::deoptimize` intrinsic. Implemented as a no-op by default.
    /// Implementations that generate IR that is run through a separate optimizer should override the default impl
    fn write_deoptimize(&mut self, val: VStackValue<Self::Loc>) -> VStackValue<Self::Loc> {
        val
    }

    /// Writes an instruction corresponding to the given [`Trap`]
    fn write_trap(&mut self, trap: Trap);

    /// Writes a full thread fence for the given AccessClass
    fn write_barrier(&mut self, acc: AccessClass);

    /// Writes a given value into a given lvalue.
    fn store_val(&mut self, val: VStackValue<Self::Loc>, lvalue: LValue<Self::Loc>);

    /// Loads a value into the given val location
    fn load_val(&mut self, lvalue: LValue<Self::Loc>, loc: Self::Loc);

    /// Writes the exit routine for returning nothing
    fn return_void(&mut self);

    /// Writes the exit routine for returning a value, including moving the value into the return place
    fn return_value(&mut self, val: VStackValue<Self::Loc>);

    /// Preferred Vector size of the current codegen
    fn preferred_vec_size(&self) -> usize {
        0
    }

    /// Writes a call to a target intrinsic (such as `x86::_mm_addp_i8`)
    fn write_intrinsic(
        &mut self,
        name: StringView,
        params: Vec<VStackValue<Self::Loc>>,
    ) -> VStackValue<Self::Loc>;

    /// Writes a new target at the current location
    fn write_target(&mut self, target: u32);
    /// Performs a direct call to a named function
    fn call_direct(
        &mut self,
        value: StringView,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    ) -> Option<VStackValue<Self::Loc>>;
    /// Performs an indirect call to the pointer stored in `value`
    fn call_indirect(
        &mut self,
        value: Self::Loc,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    ) -> Option<VStackValue<Self::Loc>>;

    /// Performs a guaranteed tail call to the target
    /// Note: The signature is assumed to be compatible with the current function
    fn tailcall_direct(
        &mut self,
        value: StringView,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    );

    /// Performs a guaranteed tail call to the target
    /// Note: The signature is assumed to be compatible with the current function
    fn tailcall_indirect(
        &mut self,
        value: Self::Loc,
        ty: &FnType,
        params: Vec<VStackValue<Self::Loc>>,
    );

    /// Performs a conditional branch to `target` based on `condition` and `val`
    fn branch(&mut self, target: u32, condition: BranchCondition, val: VStackValue<Self::Loc>);
    /// Performs a conditional branch based on comparing `v1` and `v2` according to `condition`
    /// This is used for the sequence `cmp; branch <condition> @<target>`
    fn branch_compare(
        &mut self,
        target: u32,
        condition: BranchCondition,
        v1: VStackValue<Self::Loc>,
        v2: VStackValue<Self::Loc>,
    );
    /// Branches to the `target` unconditionally (IE. when the condition is always, or based on constant-folded values)
    fn branch_unconditional(&mut self, target: u32);

    /// Branches to the target given in `target`
    fn branch_indirect(&mut self, target: Self::Loc);
    /// Moves a value to `loc`. Doesn't free the incoming `val`
    fn move_value(&mut self, val: VStackValue<Self::Loc>, loc: Self::Loc);

    /// Computes the address of a global, and moves the pointer into `Self::Loc`
    fn compute_global_address(&mut self, path: &Path, loc: Self::Loc);

    /// Computes the address of a label, and moves the pointer into `Self::Loc`
    fn compute_label_address(&mut self, target: u32, loc: Self::Loc);

    /// Computes the address of a parameter and moves the pointer into `Self::Loc`
    fn compute_parameter_address(&mut self, param: u32, loc: Self::Loc);

    /// Computes the address of a local variable in `inloc` (used only if addressable), and moves the pointer into `Self::Loc`
    fn compute_local_address(&mut self, inloc: Self::Loc, loc: Self::Loc);

    /// Computes the address of a string literal
    fn compute_string_address(&mut self, enc: Encoding, bytes: Vec<u8>, loc: Self::Loc);

    /// Marks the given location as freed and allows other allocations to use the location without clobbering it
    fn free(&mut self, loc: Self::Loc);

    /// Clobbers the given location, saving the value and then freeing it.
    fn clobber(&mut self, loc: Self::Loc);

    /// Allocates space to store a local variable or stack value of type `Type`
    fn allocate(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc;

    /// Allocates space to store an lvalue
    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc;

    /// Writes the result of some binary operator applied to v1 and v2 to the location given by `out`
    /// Implementations should be prepared to handle constant values, as well as opaque values
    fn write_binary_op(
        &mut self,
        v1: VStackValue<Self::Loc>,
        v2: VStackValue<Self::Loc>,
        op: BinaryOp,
        out: Self::Loc,
    );

    /// Writes a branch point for the entry of block n
    fn write_block_entry_point(&mut self, n: u32);
    /// Writes a branch point for the exit of block n
    fn write_block_exit_point(&mut self, n: u32);
    /// Writes an branch to the exit of block n
    fn write_block_exit(&mut self, n: u32);

    /// Writes the result of some unary operator applied to v1 and v2 to the location given by `out`
    /// Implementations should be prepared to handle constant values, as well as opaque values
    fn write_unary_op(&mut self, v1: VStackValue<Self::Loc>, op: UnaryOp, out: Self::Loc);

    /// Writes the result of a scalar cast to some ScalarType to v1 to the location given by `out`
    /// Implementations should be prepared to handle constant values, as well as opaque values
    fn write_scalar_cast(&mut self, v1: VStackValue<Self::Loc>, ty: &ScalarType, out: Self::Loc);
}

/// A type for handling the generation of code for functions.
pub struct FunctionCodegen<F: FunctionRawCodegen> {
    inner: F,
    vstack: VecDeque<VStackValue<F::Loc>>,
    properties: &'static TargetProperties,
    targets: HashMap<u32, Vec<(F::Loc, StackItem)>>,
    diverged: bool,
    locals: Vec<(F::Loc, Type)>,
}

impl<F: FunctionRawCodegen> FunctionCodegen<F> {
    /// Constructs a new [`FunctionCodegen`] with a given [`FunctionRawCodegen`], the given function name and signature, and the target properties
    pub fn new(
        inner: F,
        _path: Path,
        _fnty: FnType,
        properties: &'static TargetProperties,
    ) -> Self {
        Self {
            inner,
            properties,
            vstack: VecDeque::new(),
            targets: HashMap::new(),
            diverged: false,
            locals: Vec::new(),
        }
    }

    /// Obtains the target properties.
    /// Convience Method for some code generators
    pub fn properties(&self) -> &'static TargetProperties {
        self.properties
    }

    /// Obtains a mutable reference to the inner `F`
    pub fn raw_inner(&mut self) -> &mut F {
        &mut self.inner
    }

    /// Obtains the inner `F` from self
    pub fn into_inner(self) -> F {
        self.inner
    }

    ///
    /// Pushes a single value onto the vstack
    pub fn push_value(&mut self, val: VStackValue<F::Loc>) {
        self.vstack.push_back(val)
    }

    ///
    /// Pops a single value from the vstack
    pub fn pop_value(&mut self) -> Option<VStackValue<F::Loc>> {
        self.vstack.pop_back().into()
    }

    ///
    /// Pops `n` values from the vstack
    pub fn pop_values(&mut self, n: usize) -> Option<Vec<VStackValue<F::Loc>>> {
        let len = self.vstack.len();
        if len < n {
            None
        } else {
            Some(self.vstack.drain((len - n)..).collect())
        }
    }

    /// Clears the expression stack
    pub fn clear_stack(&mut self) {
        self.vstack.clear()
    }

    /// Writes an expression in linear order into the codegen
    pub fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null => {}
            Expr::Const(v) => self.vstack.push_back(VStackValue::Constant(v.clone())),
            Expr::ExitBlock { blk, values } => {
                self.diverged = true;
                if *blk == 0 {
                    assert!(*values <= 1);
                    if *values == 0 {
                        self.inner.return_void();
                    } else {
                        let val = self.vstack.pop_back().unwrap();
                        if let VStackValue::Trapped = val {
                            self.vstack.push_back(VStackValue::Trapped)
                        } else {
                            self.inner.return_value(val);
                        }
                    }
                } else if *values == 0 {
                    self.inner.write_block_exit(*blk);
                } else {
                    todo!("exit block ${} {}", blk, values);
                }
            }
            Expr::BinaryOp(BinaryOp::Cmp) => {
                let [val1, val2] = [
                    self.vstack.pop_back().unwrap(),
                    self.vstack.pop_back().unwrap(),
                ];
                self.vstack
                    .push_back(VStackValue::CompareResult(Box::new(val1), Box::new(val2)))
            }
            Expr::BinaryOp(op) => {
                let val1 = self.vstack.pop_back().unwrap();
                let val2 = self.vstack.pop_back().unwrap();
                match (val1, val2) {
                    (VStackValue::Trapped, _) | (_, VStackValue::Trapped) => {
                        self.vstack.push_back(VStackValue::Trapped);
                    }
                    (VStackValue::Constant(Value::Invalid(_)), _)
                    | (_, VStackValue::Constant(Value::Invalid(_))) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.vstack.push_back(VStackValue::Trapped);
                    }
                    (VStackValue::Constant(Value::Uninitialized(ty)), _)
                    | (_, VStackValue::Constant(Value::Uninitialized(ty))) => match op {
                        BinaryOp::Cmp | BinaryOp::CmpInt => {
                            self.vstack
                                .push_back(VStackValue::Constant(Value::Uninitialized(
                                    Type::Scalar(ScalarType {
                                        header: ScalarTypeHeader {
                                            bitsize: 32,
                                            ..Default::default()
                                        },
                                        kind: ScalarTypeKind::Integer {
                                            signed: true,
                                            min: i32::MIN.into(),
                                            max: i32::MAX.into(),
                                        },
                                    }),
                                )));
                        }
                        BinaryOp::CmpLt
                        | BinaryOp::CmpGt
                        | BinaryOp::CmpEq
                        | BinaryOp::CmpNe
                        | BinaryOp::CmpGe
                        | BinaryOp::CmpLe => {
                            self.vstack
                                .push_back(VStackValue::Constant(Value::Uninitialized(
                                    Type::Scalar(ScalarType {
                                        header: ScalarTypeHeader {
                                            bitsize: 1,
                                            ..Default::default()
                                        },
                                        kind: ScalarTypeKind::Integer {
                                            signed: false,
                                            min: 0,
                                            max: 1,
                                        },
                                    }),
                                )));
                        }
                        _ => self
                            .vstack
                            .push_back(VStackValue::Constant(Value::Uninitialized(ty))),
                    },
                    (VStackValue::LValue(_, _), _) | (_, VStackValue::LValue(_, _)) => {
                        panic!("lvalues are not valid for {:?}", op)
                    }
                    (
                        VStackValue::AggregatePieced(_, _) | VStackValue::OpaqueAggregate(_, _),
                        _,
                    )
                    | (
                        _,
                        VStackValue::AggregatePieced(_, _) | VStackValue::OpaqueAggregate(_, _),
                    ) => {
                        panic!("aggregates are not valid for {:?}", op)
                    }
                    (VStackValue::Constant(_), VStackValue::Constant(_)) => todo!(),
                    (VStackValue::Constant(_), VStackValue::Pointer(_, _)) => todo!(),
                    (VStackValue::Constant(_), VStackValue::OpaqueScalar(_, _)) => todo!(),
                    (VStackValue::Constant(_), VStackValue::CompareResult(_, _)) => todo!(),

                    (VStackValue::Pointer(_, _), VStackValue::Constant(_)) => todo!(),
                    (VStackValue::Pointer(_, _), VStackValue::Pointer(_, _)) => todo!(),
                    (VStackValue::Pointer(_, _), VStackValue::OpaqueScalar(_, _)) => todo!(),
                    (VStackValue::Pointer(_, _), VStackValue::CompareResult(_, _)) => todo!(),
                    (VStackValue::OpaqueScalar(_, _), VStackValue::Constant(_)) => todo!(),

                    (VStackValue::OpaqueScalar(_, _), VStackValue::Pointer(_, _)) => todo!(),
                    (VStackValue::OpaqueScalar(_, _), VStackValue::OpaqueScalar(_, _)) => todo!(),
                    (VStackValue::OpaqueScalar(_, _), VStackValue::CompareResult(_, _)) => todo!(),
                    (VStackValue::CompareResult(_, _), VStackValue::Constant(_)) => todo!(),
                    (VStackValue::CompareResult(_, _), VStackValue::Pointer(_, _)) => todo!(),
                    (VStackValue::CompareResult(_, _), VStackValue::OpaqueScalar(_, _)) => todo!(),
                    (VStackValue::CompareResult(_, _), VStackValue::CompareResult(_, _)) => todo!(),
                }
            }
            Expr::UnaryOp(op) => todo!("unary op {:?}", op),
            Expr::CallFunction(ty) => {
                let start = self.vstack.len() - ty.params.len();
                let params = self.vstack.drain(start..).collect::<Vec<_>>();
                let f = self.vstack.pop_back().unwrap();
                match f {
                    VStackValue::Trapped => self.vstack.push_back(VStackValue::Trapped),
                    VStackValue::Constant(Value::GlobalAddress { item, .. }) => {
                        match &*item.components {
                            [PathComponent::Text(name)]
                            | [PathComponent::Root, PathComponent::Text(name)] => {
                                if let Some(ret) =
                                    self.inner.call_direct(StringView::new(name), ty, params)
                                {
                                    self.vstack.push_back(ret)
                                }
                            }
                            [PathComponent::Root, name @ ..] | [name @ ..] => match name.first() {
                                std::option::Option::Some(PathComponent::Text(n))
                                    if n == "__lccc" || n == "__lccc__" =>
                                {
                                    intrinsic::call_intrinsic(&item, self, ty)
                                }
                                std::option::Option::Some(_) => {
                                    let name = mangle::mangle_itanium(name);
                                    if let Some(ret) =
                                        self.inner.call_direct(StringView::new(&name), ty, params)
                                    {
                                        self.vstack.push_back(ret)
                                    }
                                }
                                std::option::Option::None => {
                                    panic!("unexpected path without components")
                                }
                            },
                        }
                    }
                    VStackValue::Constant(Value::Uninitialized(_))
                    | VStackValue::Constant(Value::Invalid(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.vstack.push_back(VStackValue::Trapped)
                    }
                    VStackValue::LValue(_, _) => todo!(),
                    VStackValue::Pointer(_, _) => todo!(),
                    v => panic!("invalid value {:?}", v),
                }
            }
            Expr::Branch { cond, target } => {
                match cond {
                    BranchCondition::Always => {
                        let locs = self.targets[target].clone();
                        let vals = self.pop_values(locs.len()).unwrap();
                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                            self.inner.move_value(val, loc); // This will break if the branch target uses any values rn.
                        }
                        self.inner.branch_unconditional(*target);
                        self.clear_stack();
                        self.diverged = true;
                    }
                    BranchCondition::Never => {}
                    cond => {
                        let val = self.vstack.pop_back().unwrap();
                        let locs = self.targets[target].clone();
                        let vals = self.pop_values(locs.len()).unwrap();

                        match val {
                            VStackValue::Constant(v) => match v {
                                Value::Invalid(_) | Value::Uninitialized(_) => {
                                    self.inner.write_trap(Trap::Unreachable);
                                    self.vstack.push_back(VStackValue::Trapped)
                                }
                                Value::GenericParameter(u) => todo!("%{}", u),
                                Value::Integer {
                                    ty:
                                        ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: true, .. },
                                            ..
                                        },
                                    val,
                                } => match ((val as i128).cmp(&0), cond) {
                                    (_, BranchCondition::Always | BranchCondition::Never) => {
                                        unreachable!()
                                    }
                                    (
                                        Ordering::Less,
                                        BranchCondition::Less
                                        | BranchCondition::LessEqual
                                        | BranchCondition::NotEqual,
                                    ) => {
                                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                            self.inner.move_value(val, loc);
                                            // This will break if the branch target uses any values rn.
                                        }
                                        self.inner.branch_unconditional(*target);
                                        self.clear_stack();
                                        self.diverged = true;
                                    }
                                    (
                                        Ordering::Equal,
                                        BranchCondition::Equal
                                        | BranchCondition::LessEqual
                                        | BranchCondition::GreaterEqual,
                                    ) => {
                                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                            self.inner.move_value(val, loc);
                                            // This will break if the branch target uses any values rn.
                                        }
                                        self.inner.branch_unconditional(*target);
                                        self.clear_stack();
                                        self.diverged = true;
                                    }
                                    (
                                        Ordering::Greater,
                                        BranchCondition::Greater
                                        | BranchCondition::GreaterEqual
                                        | BranchCondition::NotEqual,
                                    ) => {
                                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                            self.inner.move_value(val, loc);
                                            // This will break if the branch target uses any values rn.
                                        }
                                        self.inner.branch_unconditional(*target);
                                        self.clear_stack();
                                        self.diverged = true;
                                    }
                                    _ => {}
                                },
                                Value::Integer {
                                    ty:
                                        ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: false, .. },
                                            ..
                                        },
                                    val,
                                } => {
                                    match (val, cond) {
                                        (
                                            0,
                                            BranchCondition::Equal
                                            | BranchCondition::LessEqual
                                            | BranchCondition::GreaterEqual,
                                        ) => {
                                            for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                                self.inner.move_value(val, loc);
                                                // This will break if the branch target uses any values rn.
                                            }
                                            self.inner.branch_unconditional(*target);
                                            self.clear_stack();
                                            self.diverged = true;
                                        }
                                        (
                                            x,
                                            BranchCondition::Greater
                                            | BranchCondition::GreaterEqual
                                            | BranchCondition::NotEqual,
                                        ) if x != 0 => {
                                            for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                                self.inner.move_value(val, loc);
                                                // This will break if the branch target uses any values rn.
                                            }
                                            self.inner.branch_unconditional(*target);
                                            self.clear_stack();
                                            self.diverged = true;
                                        }
                                        _ => {}
                                    }
                                }
                                v => panic!("Cannot branch on {:?}", v),
                            },

                            VStackValue::OpaqueScalar(
                                sty @ ScalarType {
                                    kind: ScalarTypeKind::Integer { .. },
                                    ..
                                },
                                loc,
                            ) => self.inner.branch_compare(
                                *target,
                                *cond,
                                VStackValue::OpaqueScalar(sty, loc),
                                VStackValue::Constant(Value::Integer { ty: sty, val: 0 }),
                            ),

                            VStackValue::CompareResult(c1, c2) => {
                                for (val, (loc, _)) in vals.into_iter().zip(locs) {
                                    self.opaqueify(val, loc) // This will break if the branch target uses any values rn.
                                }
                                self.inner.branch_compare(
                                    *target,
                                    *cond,
                                    Box::into_inner(c1),
                                    Box::into_inner(c2),
                                );
                            }
                            VStackValue::Trapped => {}
                            v => panic!("Cannot branch on {:?}", v),
                        }
                    }
                }
            }
            Expr::Convert(_, ty) => {
                let val = self.vstack.pop_back().unwrap();
                match (val, ty) {
                    (VStackValue::Pointer(_, lvalue), Type::Pointer(pty)) => self
                        .vstack
                        .push_back(VStackValue::Pointer(pty.clone(), lvalue)),
                    (
                        VStackValue::Constant(Value::String {
                            encoding,
                            utf8,
                            ty: Type::Pointer(_),
                        }),
                        Type::Pointer(pty),
                    ) => self.vstack.push_back(VStackValue::Pointer(
                        pty.clone(),
                        LValue::StringLiteral(Encoding::XLang(encoding), utf8.into_bytes()),
                    )),
                    (VStackValue::LValue(_, _), ty) => {
                        panic!("convert {:?} cannot be applied to lvalues", ty)
                    }
                    (val, ty) => todo!("convert _ {:?}: {:?}", ty, val),
                }
            }
            Expr::Derive(_, expr) => self.write_expr(expr),
            Expr::Local(n) => {
                let (loc, ty) = self.locals[*n as usize].clone();
                self.vstack
                    .push_back(VStackValue::LValue(ty, LValue::Local(loc)))
            }
            Expr::Pop(n) => drop(self.pop_values(*n as usize).unwrap()),
            Expr::Dup(n) => {
                let stack = self.pop_values(*n as usize).unwrap();
                self.vstack.extend(stack.iter().cloned());
                self.vstack.extend(stack);
            }
            Expr::Pivot(n, m) => {
                let s1 = self.pop_values(*m as usize).unwrap();
                let s2 = self.pop_values(*n as usize).unwrap();
                self.vstack.extend(s1);
                self.vstack.extend(s2);
            }
            Expr::Aggregate(AggregateCtor { ty, fields }) => {
                let ctor_values = self.pop_values(fields.len()).unwrap();
                self.vstack.push_back(VStackValue::AggregatePieced(
                    ty.clone(),
                    fields.iter().cloned().zip(ctor_values).collect(),
                ))
            }
            Expr::Member(name) => {
                let value = self.vstack.pop_back().unwrap();
                match value {
                    VStackValue::LValue(ty, val) => {
                        let fty = ty::field_type(&ty, name).unwrap();
                        self.vstack.push_back(VStackValue::LValue(
                            fty,
                            LValue::Field(ty, Box::new(val), name.clone()),
                        ));
                    }
                    val => panic!("Cannot apply member {} to {:?}", name, val),
                }
            }
            Expr::MemberIndirect(name) => {
                let value = self.vstack.pop_back().unwrap();
                match value {
                    VStackValue::Pointer(ty, val) => {
                        let fty = ty::field_type(&ty.inner, name).unwrap();
                        let inner = ty.inner;
                        let pty = PointerType {
                            inner: Box::new(fty),
                            ..ty
                        };
                        self.vstack.push_back(VStackValue::Pointer(
                            pty,
                            LValue::Field(Box::into_inner(inner), Box::new(val), name.clone()),
                        ));
                    }
                    val => panic!("Cannot apply member {} to {:?}", name, val),
                }
            }
            Expr::Block { n, block } => {
                self.inner.write_block_entry_point(*n);
                self.write_block(block, *n);
                self.inner.write_block_exit_point(*n);
            }
            Expr::Assign(_) => {
                let value = self.vstack.pop_back().unwrap();
                let destination = self.vstack.pop_back().unwrap();
                match destination {
                    VStackValue::LValue(_, lval) => self.inner.store_val(value, lval),
                    val => panic!("Cannot assign to an rvalue {:?}", val),
                }
            }
            Expr::AsRValue(_) => {
                let destination = self.vstack.pop_back().unwrap();
                match destination {
                    VStackValue::LValue(ty, lval) => {
                        let loc = self.inner.allocate(&ty, false);
                        self.inner.load_val(lval, loc.clone());
                        self.push_opaque(&ty, loc);
                    }
                    val => panic!("Cannot assign to an rvalue {:?}", val),
                }
            }
        }
    }

    /// Pushes an opaque value of the given type
    pub fn push_opaque(&mut self, ty: &Type, loc: F::Loc) {
        match ty {
            Type::Scalar(st) => self.vstack.push_back(VStackValue::OpaqueScalar(*st, loc)),
            Type::Void | Type::Null | Type::FnType(_) => {
                self.vstack.push_back(VStackValue::Trapped)
            }
            Type::Pointer(pty) => self.vstack.push_back(VStackValue::Pointer(
                pty.clone(),
                LValue::OpaquePointer(loc),
            )),
            Type::Array(_) => todo!(),
            Type::TaggedType(_, ty) | Type::Aligned(_, ty) => self.push_opaque(ty, loc),
            Type::Product(_) | Type::Aggregate(_) => self
                .vstack
                .push_back(VStackValue::OpaqueAggregate(ty.clone(), loc)),
        }
    }

    ///
    /// Moves val into loc and pushes the opaque version
    pub fn opaqueify(&mut self, val: VStackValue<F::Loc>, loc: F::Loc) {
        match &val {
            VStackValue::Constant(v) => match v {
                Value::Invalid(_) => {
                    self.inner.write_trap(Trap::Unreachable);
                    self.vstack.push_back(VStackValue::Trapped)
                }
                Value::Uninitialized(ty) => self.push_opaque(ty, loc.clone()),
                Value::GenericParameter(n) => todo!("%{}", n),
                Value::Integer { ty, .. } => self
                    .vstack
                    .push_back(VStackValue::OpaqueScalar(*ty, loc.clone())),
                Value::GlobalAddress { ty, .. } => self.vstack.push_back(VStackValue::Pointer(
                    PointerType {
                        inner: Box::new(ty.clone()),
                        ..Default::default()
                    },
                    LValue::OpaquePointer(loc.clone()),
                )),
                Value::ByteString { .. } => todo!(),
                Value::String { ty, .. } => self.push_opaque(ty, loc.clone()),
            },
            VStackValue::LValue(ty, _) => self.vstack.push_back(VStackValue::LValue(
                ty.clone(),
                LValue::OpaquePointer(loc.clone()),
            )),
            VStackValue::Pointer(pty, _) => self.vstack.push_back(VStackValue::Pointer(
                pty.clone(),
                LValue::OpaquePointer(loc.clone()),
            )),
            VStackValue::OpaqueScalar(ty, _) => self
                .vstack
                .push_back(VStackValue::OpaqueScalar(*ty, loc.clone())),
            VStackValue::AggregatePieced(ty, _) | VStackValue::OpaqueAggregate(ty, _) => self
                .vstack
                .push_back(VStackValue::OpaqueAggregate(ty.clone(), loc.clone())),
            VStackValue::CompareResult(_, _) => self.vstack.push_back(VStackValue::OpaqueScalar(
                ScalarType {
                    header: ScalarTypeHeader {
                        bitsize: 32,
                        ..Default::default()
                    },
                    kind: ScalarTypeKind::Integer {
                        signed: true,
                        min: i32::MIN as i128,
                        max: i32::MAX as i128,
                    },
                },
                loc.clone(),
            )),
            VStackValue::Trapped => self.vstack.push_back(VStackValue::Trapped),
        }
        self.inner.move_value(val, loc);
    }

    /// Writes the body of a function to the codegen
    pub fn write_function_body(&mut self, body: &FunctionBody) {
        self.locals.reserve(body.locals.len());
        for ty in &body.locals {
            let loc = self.inner.allocate(ty, true);
            self.locals.push((loc, ty.clone()))
        }
        self.write_block(&body.block, 0);
        if !self.diverged {
            self.inner.return_void();
        }
    }

    /// Writes the elements of a block to the codegen, usually the top level block of a function
    pub fn write_block(&mut self, block: &Block, _: u32) {
        for item in &block.items {
            if let xlang::ir::BlockItem::Target { num, stack } = item {
                let values = stack
                    .iter()
                    .map(|item| match item {
                        StackItem {
                            kind: StackValueKind::LValue,
                            ..
                        } => (self.inner.allocate_lvalue(false), item.clone()),
                        StackItem {
                            kind: StackValueKind::RValue,
                            ty,
                        } => (self.inner.allocate(ty, false), item.clone()),
                    })
                    .collect();
                self.targets.insert(*num, values);
            }
        }

        for item in &block.items {
            match item {
                xlang::ir::BlockItem::Expr(expr) => self.write_expr(expr),
                xlang::ir::BlockItem::Target { num, .. } => {
                    if !self.diverged {
                        let locs = self.targets[num].clone();
                        let vals = self.pop_values(locs.len()).unwrap();
                        for (val, (loc, _)) in vals.into_iter().zip(locs) {
                            self.inner.move_value(val, loc); // This will break if the branch target uses any values rn.
                        }
                        self.clear_stack();
                    }
                    for (loc, item) in self.targets[num].clone() {
                        match item {
                            StackItem {
                                kind: StackValueKind::LValue,
                                ty,
                            } => self.vstack.push_back(VStackValue::LValue(
                                ty.clone(),
                                LValue::OpaquePointer(loc),
                            )),
                            StackItem {
                                kind: StackValueKind::RValue,
                                ty,
                            } => self.push_opaque(&ty, loc),
                        }
                    }
                    self.inner.write_target(*num);
                    self.diverged = false;
                }
            }
        }
    }
}
