#![deny(missing_docs, warnings)] // No clippy::nursery
//! A helper crate for implementing [`xlang::plugin::XLangCodegen`]s without duplicating code (also can be used to evaluate constant expressions)
//! the `xlang_backend` crate provides a general interface for writing expressions to an output.
use std::{collections::VecDeque, fmt::Debug};

use self::str::Encoding;
use expr::{LValue, Trap, VStackValue, ValLocation};
use xlang::{
    abi::string::StringView,
    ir::{
        AccessClass, BinaryOp, Block, BranchCondition, Expr, FnType, Path, PathComponent,
        ScalarType, Type, UnaryOp, Value,
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

    /// Assigns space in the prepared stack frame or a register to store a local variable of type `Type`
    fn assign_value(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc;
    /// Prepares a stack frame for use.
    /// This should be called before any other operations
    fn prepare_stack_frame(&mut self, to_assign: &[Type]) -> Option<usize>;
    /// Writes the result of some binary operator applied to v1 and v2 to the location given by `out`
    /// Implementations should be prepared to handle constant values, as well as opaque values
    fn write_binary_op(
        &mut self,
        v1: VStackValue<Self::Loc>,
        v2: VStackValue<Self::Loc>,
        op: BinaryOp,
        out: Self::Loc,
    );

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

    /// Writes an expression in linear order into the codegen
    pub fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null => {}
            Expr::Const(v) => self.vstack.push_back(VStackValue::Constant(v.clone())),
            Expr::ExitBlock { blk, values } => {
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
                } else {
                    todo!("exit block ${} {}", blk, values)
                }
            }
            Expr::BinaryOp(op) => todo!("binary op {:?}", op),
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
                            [..] => intrinsic::call_intrinsic(&item, self, ty),
                        }
                    }
                    VStackValue::Constant(Value::Uninitialized(_))
                    | VStackValue::Constant(Value::Invalid(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.vstack.push_back(VStackValue::Trapped)
                    }
                    VStackValue::LValue(_) => todo!(),
                    VStackValue::Pointer(_) => todo!(),
                    v => panic!("invalid value {:?}", v),
                }
            }
            Expr::Branch { cond, target } => todo!("branch {:?} @{}", cond, target),
        }
    }

    fn clear_stack(&mut self) {
        self.vstack.clear(); // TODO: Free values stored in registers/memory
    }

    /// Writes the elements of a block to the codegen, usually the top level block of a function
    pub fn write_block(&mut self, block: &Block) {
        for item in &block.items {
            match item {
                xlang::ir::BlockItem::Expr(expr) => self.write_expr(expr),
                xlang::ir::BlockItem::Target { num, stack } => {
                    self.inner.write_target(*num);
                    if stack.len() != 0 {
                        todo!("target @{} {:?}", num, stack)
                    } else {
                        self.clear_stack()
                    }
                }
            }
        }
    }
}
