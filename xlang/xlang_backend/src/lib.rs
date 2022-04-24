#![deny(missing_docs, warnings)] // No clippy::nursery
//! A helper crate for implementing [`xlang::plugin::XLangCodegen`]s without duplicating code (also can be used to evaluate constant expressions)
//! the `xlang_backend` crate provides a general interface for writing expressions to an output.
use std::{
    collections::{HashSet, VecDeque},
    convert::{TryFrom, TryInto},
    fmt::Debug,
    io::Write,
    mem::MaybeUninit,
    option::Option::Some as StdSome,
    rc::Rc,
};

use self::str::Encoding;
use callconv::CallingConvention;
use expr::{LValue, Trap, VStackValue, ValLocation};
use ty::TypeInformation;
use xlang::{
    abi::string::StringView,
    ir::{
        AccessClass, BinaryOp, Block, BranchCondition, CharFlags, Expr, FnType, FunctionBody,
        HashSwitch, LinearSwitch, OverflowBehaviour, Path, PointerType, ScalarType,
        ScalarTypeHeader, ScalarTypeKind, ScalarValidity, StackItem, StackValueKind, Switch, Type,
        Value,
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

    /// The type of calling conventions used by this backend
    type CallConv: CallingConvention<Loc = Self::Loc> + ?Sized;

    /// Handles the `__lccc::xlang::deoptimize` intrinsic. Implemented as a no-op by default.
    /// Implementations that generate IR that is run through a separate optimizer should override the default impl
    fn write_deoptimize(&mut self, val: Self::Loc) -> Self::Loc {
        val
    }

    /// Writes an instruction corresponding to the given [`Trap`]
    fn write_trap(&mut self, trap: Trap);

    /// Writes a full thread fence for the given AccessClass
    fn write_barrier(&mut self, acc: AccessClass);

    /// Performs a binary operatoration on a val location and a constant
    fn write_int_binary_imm(&mut self, a: Self::Loc, b: u128, ty: &Type, op: BinaryOp);

    /// Moves a value between two [`ValLocation`]s
    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc);

    /// Stores an immediate value into the given location
    fn move_imm(&mut self, src: u128, dest: Self::Loc, ty: &Type);

    /// Stores an immediate value into the pointer in `ptr`
    fn store_indirect_imm(&mut self, src: Value, ptr: Self::Loc);

    /// Loads a value into the pointer in the given value location
    fn load_val(&mut self, lvalue: Self::Loc, loc: Self::Loc);

    /// Stores a value into the given value location
    fn store_indirect(&mut self, lvalue: Self::Loc, loc: Self::Loc, ty: &Type);

    /// Obtains the calling convention for the current function
    fn get_callconv(&self) -> &Self::CallConv;

    /// The maximum integer size (in bits) supported natively (without emulation)
    fn native_int_size(&self) -> u16;
    /// The maximum floating point size (in bits) supported natively, or None if no floating-point support exists
    fn native_float_size(&self) -> Option<u16>;

    /// The maximum Vector size supported natively, in bytes
    fn native_vec_size(&self) -> Option<u64> {
        None
    }

    /// Preferred Vector size of the current codegen, in total bytes
    /// This need not be the same as the [`FunctionRawCodegen::native_vec_size`], for example, if some vector types incur a significant runtime performance penalty
    /// (such as AVX-512)
    fn preferred_vec_size(&self) -> Option<u64> {
        None
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
    fn call_direct(&mut self, path: &Path);
    /// Performs an indirect call to the pointer stored in `value`
    fn call_indirect(&mut self, value: Self::Loc);

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

    /// Performs the exit sequence of a function
    fn leave_function(&mut self);

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

    /// Writes a branch point for the entry of block n
    fn write_block_entry_point(&mut self, n: u32);
    /// Writes a branch point for the exit of block n
    fn write_block_exit_point(&mut self, n: u32);
    /// Writes an branch to the exit of block n
    fn write_block_exit(&mut self, n: u32);

    /// Prepares the stack frame (as necessary) for a call to a function with the given `callty` and `realty`
    fn prepare_call_frame(&mut self, callty: &FnType, realty: &FnType);
}

#[derive(Default, Debug)]
struct BranchToInfo {
    fallthrough_from: u32,
    branch_from: HashSet<u32>,
}

/// A type for handling the generation of code for functions.
pub struct FunctionCodegen<F: FunctionRawCodegen> {
    inner: F,
    vstack: VecDeque<VStackValue<F::Loc>>,
    properties: &'static TargetProperties,
    targets: HashMap<u32, Vec<VStackValue<F::Loc>>>,
    diverged: bool,
    locals: Vec<(VStackValue<F::Loc>, Type)>,
    fnty: FnType,
    locals_opaque: bool,
    _tys: Rc<TypeInformation>,
    ctarg: u32,
    cfg: HashMap<u32, BranchToInfo>,
}

impl<F: FunctionRawCodegen> FunctionCodegen<F> {
    /// Constructs a new [`FunctionCodegen`] with a given [`FunctionRawCodegen`], the given function name and signature, and the target properties
    pub fn new(
        inner: F,
        _path: Path,
        fnty: FnType,
        properties: &'static TargetProperties,
        tys: Rc<TypeInformation>,
    ) -> Self {
        Self {
            inner,
            properties,
            vstack: VecDeque::new(),
            targets: HashMap::new(),
            diverged: false,
            locals: Vec::new(),
            fnty,
            locals_opaque: false,
            _tys: tys,
            ctarg: !0,
            cfg: HashMap::new(),
        }
    }

    fn print_vstack(&self) {
        let mut iter = self.vstack.iter();
        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();
        core::write!(stdout, "[").unwrap();
        if let StdSome(val) = iter.next() {
            core::write!(stdout, "{}", val).unwrap();
        }

        for val in iter {
            core::write!(stdout, ", {}", val).unwrap();
        }

        core::writeln!(stdout, "]").unwrap();
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

    /// Moves a given value into the given value location
    pub fn move_val(&mut self, val: VStackValue<F::Loc>, loc: F::Loc) {
        match val {
            VStackValue::Constant(Value::Invalid(_)) => {
                self.inner.write_trap(Trap::Unreachable);
                self.vstack.push_back(VStackValue::Trapped);
            }
            VStackValue::Constant(Value::Uninitialized(_)) | VStackValue::Trapped => {}
            VStackValue::Constant(Value::GlobalAddress { item, .. }) => {
                self.inner.compute_global_address(&item, loc)
            }
            VStackValue::Constant(Value::LabelAddress(n)) => {
                self.inner.compute_label_address(n, loc)
            }
            VStackValue::Constant(Value::String {
                encoding,
                utf8,
                ty: Type::Pointer(_),
            }) => {
                self.inner
                    .compute_string_address(Encoding::XLang(encoding), utf8.into_bytes(), loc)
            }
            VStackValue::Constant(Value::String { ty, .. }) => todo!("string {:?}", ty),
            VStackValue::Constant(Value::ByteString { content }) => self
                .inner
                .compute_string_address(Encoding::Byte, content, loc),
            VStackValue::Constant(Value::Integer { val, ty }) => {
                self.inner.move_imm(val, loc, &Type::Scalar(ty))
            }
            VStackValue::Constant(Value::GenericParameter(n)) => todo!("%{}", n),
            VStackValue::Pointer(pty, lvalue) => match lvalue {
                LValue::OpaquePointer(loc2) => self.inner.move_val(loc2, loc),
                LValue::Temporary(_) => todo!("temporary address"),
                LValue::Local(n) => todo!("local {:?}", n),
                LValue::GlobalAddress(item) => self.inner.compute_global_address(&item, loc),
                LValue::Label(n) => self.inner.compute_label_address(n, loc),
                LValue::Field(_, _, _) => todo!("field"),
                LValue::StringLiteral(enc, bytes) => {
                    self.inner.compute_string_address(enc, bytes, loc)
                }
                LValue::Offset(_, _) => todo!("offset"),
                LValue::Null => self.inner.move_imm(0, loc, &Type::Pointer(pty)),
            },
            VStackValue::LValue(ty, lvalue) => {
                let pty = PointerType {
                    inner: Box::new(ty),
                    ..Default::default()
                };
                match lvalue {
                    LValue::OpaquePointer(loc2) => self.inner.move_val(loc2, loc),
                    LValue::Temporary(_) => todo!("temporary address"),
                    LValue::Local(n) => todo!("local {:?}", n),
                    LValue::GlobalAddress(item) => self.inner.compute_global_address(&item, loc),
                    LValue::Label(n) => self.inner.compute_label_address(n, loc),
                    LValue::Field(_, _, _) => todo!("field"),
                    LValue::StringLiteral(enc, bytes) => {
                        self.inner.compute_string_address(enc, bytes, loc)
                    }
                    LValue::Offset(_, _) => todo!("offset"),
                    LValue::Null => self.inner.move_imm(0, loc, &Type::Pointer(pty)),
                }
            }
            VStackValue::OpaqueScalar(_, loc2) => self.inner.move_val(loc2, loc),
            VStackValue::AggregatePieced(ty, fields) => {
                if self._tys.type_size(&ty) != StdSome(0) {
                    todo!("aggregate pieced {:?}", fields)
                }
            }
            VStackValue::OpaqueAggregate(_, loc2) => self.inner.move_val(loc2, loc),
            VStackValue::CompareResult(_, _) => todo!("compare result"),
            VStackValue::ArrayRepeat(_, _) => todo!("array repeat"),
        }
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

    /// Pops `N` values from the stack, and returns them in a statically-sized array, or otherwise returns `None`.
    pub fn pop_values_static<const N: usize>(&mut self) -> Option<[VStackValue<F::Loc>; N]> {
        let len = self.vstack.len();
        if len < N {
            None
        } else {
            let mut array = MaybeUninit::<[VStackValue<F::Loc>; N]>::uninit();
            let ptr = array.as_mut_ptr().cast::<VStackValue<F::Loc>>();

            let vals = self.vstack.drain((len - N)..);

            for (i, val) in vals.enumerate() {
                // SAFETY:
                // i is less than the length of the array
                unsafe {
                    ptr.add(i).write(val);
                }
            }

            // SAFETY:
            // The loop above has initialized array
            Some(unsafe { array.assume_init() })
        }
    }

    ///
    /// Pushes all of the incoming values to the stack, in order
    pub fn push_values<I: IntoIterator<Item = VStackValue<F::Loc>>>(&mut self, vals: I) {
        self.vstack.extend(vals);
    }

    /// Obtains an opaque value of the given type that is placed in `loc`
    pub fn opaque_value(&mut self, ty: &Type, loc: F::Loc) -> VStackValue<F::Loc> {
        match ty {
            Type::Null | Type::Void | Type::FnType(_) => panic!("Invalid type"),
            Type::Scalar(sty) => VStackValue::OpaqueScalar(*sty, loc),
            Type::Pointer(pty) => VStackValue::Pointer(pty.clone(), LValue::OpaquePointer(loc)),
            Type::Array(_) => todo!("array"),
            Type::TaggedType(_, ty) => self.opaque_value(ty, loc),
            Type::Product(_) | Type::Aggregate(_) => VStackValue::OpaqueAggregate(ty.clone(), loc),
            Type::Aligned(_, ty) => self.opaque_value(ty, loc),
            Type::Named(path) => todo!("named type {:?}", path),
        }
    }

    /// Makes the given value opaque, if it is not alreayd.
    pub fn make_opaque(&mut self, val: VStackValue<F::Loc>) -> VStackValue<F::Loc> {
        match val {
            VStackValue::Constant(v) => match v {
                Value::Invalid(ty) => {
                    self.inner.write_trap(Trap::Unreachable);
                    let loc = self.inner.allocate(&ty, false);
                    self.opaque_value(&ty, loc)
                }
                Value::Uninitialized(ty) => {
                    let loc = self.inner.allocate(&ty, false);
                    self.opaque_value(&ty, loc)
                }
                Value::GenericParameter(_) => todo!("generic parameters"),
                Value::Integer { ty, val } => {
                    let loc = self.inner.allocate(&Type::Scalar(ty), false);
                    self.move_val(
                        VStackValue::Constant(Value::Integer { ty, val }),
                        loc.clone(),
                    );
                    VStackValue::OpaqueScalar(ty, loc)
                }
                Value::GlobalAddress { ty, item } => {
                    let pty = PointerType {
                        inner: Box::new(ty.clone()),
                        ..Default::default()
                    };
                    let loc = self.inner.allocate(&Type::Pointer(pty.clone()), false);
                    self.move_val(
                        VStackValue::Constant(Value::GlobalAddress { ty, item }),
                        loc.clone(),
                    );
                    VStackValue::Pointer(pty, LValue::OpaquePointer(loc))
                }
                Value::ByteString { content } => {
                    let pty = PointerType {
                        inner: Box::new(Type::Scalar(ScalarType {
                            header: ScalarTypeHeader {
                                bitsize: 8,
                                ..Default::default()
                            },
                            kind: ScalarTypeKind::Char {
                                flags: CharFlags::empty(),
                            },
                        })),
                        ..Default::default()
                    };
                    let loc = self.inner.allocate(&Type::Pointer(pty.clone()), false);
                    self.move_val(
                        VStackValue::Constant(Value::ByteString { content }),
                        loc.clone(),
                    );
                    VStackValue::Pointer(pty, LValue::OpaquePointer(loc))
                }
                Value::String { encoding, utf8, ty } => {
                    let loc = self.inner.allocate(&ty, false);
                    self.move_val(
                        VStackValue::Constant(Value::String {
                            encoding,
                            utf8,
                            ty: ty.clone(),
                        }),
                        loc.clone(),
                    );
                    self.opaque_value(&ty, loc)
                }
                Value::LabelAddress(n) => {
                    let pty = PointerType {
                        inner: Box::new(Type::Void),
                        ..Default::default()
                    };
                    let loc = self.inner.allocate(&Type::Pointer(pty.clone()), false);
                    self.move_val(VStackValue::Constant(Value::LabelAddress(n)), loc.clone());
                    VStackValue::Pointer(pty, LValue::OpaquePointer(loc))
                }
            },
            VStackValue::LValue(ty, LValue::OpaquePointer(loc)) => {
                VStackValue::LValue(ty, LValue::OpaquePointer(loc))
            }
            VStackValue::Pointer(ty, LValue::OpaquePointer(loc)) => {
                VStackValue::Pointer(ty, LValue::OpaquePointer(loc))
            }
            VStackValue::LValue(ty, lval) => {
                let loc = self.inner.allocate_lvalue(false);
                self.move_val(VStackValue::LValue(ty.clone(), lval), loc.clone());
                VStackValue::LValue(ty, LValue::OpaquePointer(loc))
            }
            VStackValue::Pointer(ty, lval) => {
                let loc = self.inner.allocate_lvalue(false);
                self.move_val(VStackValue::Pointer(ty.clone(), lval), loc.clone());
                VStackValue::Pointer(ty, LValue::OpaquePointer(loc))
            }
            VStackValue::OpaqueScalar(sty, loc) => VStackValue::OpaqueScalar(sty, loc),
            VStackValue::AggregatePieced(ty, pieces) => {
                let loc = self.inner.allocate(&ty, false);
                self.move_val(
                    VStackValue::AggregatePieced(ty.clone(), pieces),
                    loc.clone(),
                );
                VStackValue::OpaqueAggregate(ty, loc)
            }
            VStackValue::OpaqueAggregate(ty, loc) => VStackValue::OpaqueAggregate(ty, loc),
            VStackValue::CompareResult(_, _) => todo!("compare results"),
            VStackValue::Trapped => VStackValue::Trapped,
            VStackValue::ArrayRepeat(_, _) => todo!("array repeat"),
        }
    }

    /// Pushes an opaque value of the given type
    pub fn push_opaque(&mut self, ty: &Type, loc: F::Loc) {
        let val = self.opaque_value(ty, loc);
        self.push_value(val);
    }

    /// Clears the expression stack
    pub fn clear_stack(&mut self) {
        self.vstack.clear()
    }

    /// Calls a function by memory address stored in `loc`
    pub fn call_indirect(
        &mut self,
        callty: &FnType,
        realty: &FnType,
        loc: F::Loc,
        vals: Vec<VStackValue<F::Loc>>,
    ) {
        self.inner.prepare_call_frame(callty, realty);
        if let std::option::Option::Some(place) =
            self.inner.get_callconv().pass_return_place(&callty.ret)
        {
            todo!("return place {:?}", place);
        }

        for (i, val) in vals.into_iter().enumerate() {
            let param_loc =
                self.inner
                    .get_callconv()
                    .find_param(callty, realty, i.try_into().unwrap(), false);
            self.move_val(val, param_loc);
        }

        self.inner.call_indirect(loc);
        match &callty.ret {
            Type::Void => {}
            Type::Scalar(ScalarType {
                kind: kind @ ScalarTypeKind::Integer { .. },
                header: header @ ScalarTypeHeader { bitsize: 0, .. },
            }) if header.validity.contains(ScalarValidity::NONZERO) => {
                // special case uint nonzero(0)/int nonzero(0)
                self.push_value(VStackValue::Constant(Value::Uninitialized(Type::Scalar(
                    ScalarType {
                        kind: *kind,
                        header: *header,
                    },
                ))));
            }
            ty => {
                let retloc = self.inner.get_callconv().find_return_val(callty);
                self.push_opaque(ty, retloc);
            }
        }
    }

    /// Calls a function by name
    pub fn call_fn(
        &mut self,
        callty: &FnType,
        realty: &FnType,
        path: &Path,
        vals: Vec<VStackValue<F::Loc>>,
    ) {
        self.inner.prepare_call_frame(callty, realty);
        if let std::option::Option::Some(place) =
            self.inner.get_callconv().pass_return_place(&callty.ret)
        {
            todo!("return place {:?}", place);
        }

        for (i, val) in vals.into_iter().enumerate() {
            let param_loc =
                self.inner
                    .get_callconv()
                    .find_param(callty, realty, i.try_into().unwrap(), false);
            self.move_val(val, param_loc);
        }

        self.inner.call_direct(path);
        match &callty.ret {
            Type::Void => {}
            Type::Scalar(ScalarType {
                kind: kind @ ScalarTypeKind::Integer { .. },
                header: header @ ScalarTypeHeader { bitsize: 0, .. },
            }) if header.validity.contains(ScalarValidity::NONZERO) => {
                // special case uint nonzero(0)/int nonzero(0)
                self.push_value(VStackValue::Constant(Value::Invalid(Type::Scalar(
                    ScalarType {
                        kind: *kind,
                        header: *header,
                    },
                ))));
            }
            ty => {
                let retloc = self.inner.get_callconv().find_return_val(callty);
                self.push_opaque(ty, retloc);
            }
        }
    }

    /// Writes the exit point of the given block with the given number of values
    pub fn write_exit(&mut self, values: u16) {
        self.diverged = true;
        if values == 1 {
            let val = self.pop_value().unwrap();
            match val {
                VStackValue::Constant(Value::Invalid(_)) => {
                    self.inner.write_trap(Trap::Unreachable);
                    return;
                }
                VStackValue::Trapped => return,
                val => {
                    let loc = self.inner.get_callconv().find_return_val(&self.fnty);
                    self.move_val(val, loc);
                }
            }
            self.inner.leave_function();
        } else if values == 0 {
            self.inner.leave_function();
        } else {
            panic!("Attempt to exit function with more than one value");
        }
    }

    /// Writes the given binary operator to the stream
    pub fn write_binary_op(&mut self, op: BinaryOp, v: OverflowBehaviour) {
        let [val1, val2] = self.pop_values_static().unwrap();

        match (val1, val2) {
            (VStackValue::Trapped, _) | (_, VStackValue::Trapped) => {
                self.push_value(VStackValue::Trapped)
            }
            (VStackValue::LValue(_, _), _) | (_, VStackValue::LValue(_, _)) => {
                panic!("Cannot apply {:?} to an lvalue", op)
            }
            (VStackValue::Constant(Value::Invalid(_)), _)
            | (_, VStackValue::Constant(Value::Invalid(_))) => {
                self.inner.write_trap(Trap::Unreachable);
                self.push_value(VStackValue::Trapped);
            }
            (VStackValue::Constant(Value::Uninitialized(ty)), _)
            | (_, VStackValue::Constant(Value::Uninitialized(ty))) => match op {
                BinaryOp::Cmp | BinaryOp::CmpInt => self.push_value(VStackValue::Constant(
                    Value::Uninitialized(Type::Scalar(ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: true,
                            min: None,
                            max: None,
                        },
                    })),
                )),
                BinaryOp::CmpLt
                | BinaryOp::CmpGt
                | BinaryOp::CmpLe
                | BinaryOp::CmpGe
                | BinaryOp::CmpEq
                | BinaryOp::CmpNe => self.push_value(VStackValue::Constant(Value::Uninitialized(
                    Type::Scalar(ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: true,
                            min: None,
                            max: None,
                        },
                    }),
                ))),
                _ => match v {
                    OverflowBehaviour::Wrap | OverflowBehaviour::Unchecked => {
                        self.push_value(VStackValue::Constant(Value::Uninitialized(ty)))
                    }
                    OverflowBehaviour::Trap => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    OverflowBehaviour::Checked => {
                        self.push_values([
                            VStackValue::Constant(Value::Uninitialized(ty)),
                            VStackValue::Constant(Value::Uninitialized(Type::Scalar(ScalarType {
                                header: ScalarTypeHeader {
                                    bitsize: 1,
                                    ..Default::default()
                                },
                                kind: ScalarTypeKind::Integer {
                                    signed: false,
                                    min: None,
                                    max: None,
                                },
                            }))),
                        ]);
                    }
                    v => todo!("Unexpected Overflow behaviour {:?}", v),
                },
            },
            (
                VStackValue::Constant(Value::Integer {
                    ty:
                        ty1 @ ScalarType {
                            header:
                                ScalarTypeHeader {
                                    vectorsize: None, ..
                                },
                            kind: ScalarTypeKind::Integer { signed: false, .. },
                        },
                    val: val1,
                }),
                VStackValue::Constant(Value::Integer {
                    ty:
                        ty2 @ ScalarType {
                            header:
                                ScalarTypeHeader {
                                    vectorsize: None, ..
                                },
                            kind: ScalarTypeKind::Integer { signed: false, .. },
                        },
                    val: val2,
                }),
            ) if ty1.header.bitsize == ty2.header.bitsize => match op {
                BinaryOp::Cmp | BinaryOp::CmpInt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = match val1.cmp(&val2) {
                        std::cmp::Ordering::Less => !0,
                        std::cmp::Ordering::Equal => 0,
                        std::cmp::Ordering::Greater => 1,
                    };

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpLt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 < val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpGt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 > val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpLe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 <= val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpGe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 >= val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpEq => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 == val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpNe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 != val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                _ => {
                    let (val, overflow) = match op {
                        BinaryOp::Add => val1.overflowing_add(val2),
                        BinaryOp::Sub => val1.overflowing_sub(val2),
                        BinaryOp::Mul => val1.overflowing_mul(val2),
                        BinaryOp::Div => {
                            if val2 == 0 {
                                (0, true)
                            } else {
                                val1.overflowing_div(val2)
                            }
                        }
                        BinaryOp::Mod => {
                            if val2 == 0 {
                                (0, true)
                            } else {
                                val1.overflowing_rem(val2)
                            }
                        }
                        BinaryOp::BitOr => (val1 | val2, false),
                        BinaryOp::BitAnd => (val1 & val2, false),
                        BinaryOp::BitXor => (val1 ^ val2, false),
                        BinaryOp::Lsh => (
                            val1.wrapping_shl(val2 as u32),
                            val2 > (ty1.header.bitsize.into()),
                        ),
                        BinaryOp::Rsh => (
                            val1.wrapping_shr(val2 as u32),
                            val2 > (ty1.header.bitsize.into()),
                        ),
                        BinaryOp::Cmp
                        | BinaryOp::CmpInt
                        | BinaryOp::CmpLt
                        | BinaryOp::CmpLe
                        | BinaryOp::CmpEq
                        | BinaryOp::CmpNe
                        | BinaryOp::CmpGe
                        | BinaryOp::CmpGt => unreachable!(),
                        op => todo!("{:?}", op),
                    };

                    let overflow = overflow || (val.leading_zeros() < (ty1.header.bitsize as u32));
                    let val = val & (!((!0u128).wrapping_shl(128 - (ty1.header.bitsize as u32))));

                    match v {
                        OverflowBehaviour::Wrap => self
                            .vstack
                            .push_back(VStackValue::Constant(Value::Integer { ty: ty1, val })),
                        OverflowBehaviour::Unchecked => {
                            if overflow {
                                self.vstack
                                    .push_back(VStackValue::Constant(Value::Uninitialized(
                                        Type::Scalar(ty1),
                                    )))
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }))
                            }
                        }
                        OverflowBehaviour::Checked => {
                            self.vstack
                                .push_back(VStackValue::Constant(Value::Integer { ty: ty1, val }));
                            self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                ty: ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 1,
                                        ..Default::default()
                                    },
                                    kind: ScalarTypeKind::Integer {
                                        signed: false,
                                        min: None,
                                        max: None,
                                    },
                                },
                                val: overflow as u128,
                            }));
                        }
                        OverflowBehaviour::Trap => {
                            if overflow {
                                self.inner.write_trap(Trap::Overflow);
                                self.vstack.push_back(VStackValue::Trapped);
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }));
                            }
                        }
                        OverflowBehaviour::Saturate => {
                            if (op == BinaryOp::Sub || op == BinaryOp::Rsh) && overflow {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val: 0,
                                }));
                            } else if overflow {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val: !((!0u128)
                                        .wrapping_shl(128 - (ty1.header.bitsize as u32))),
                                }));
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }));
                            }
                        }
                        v => todo!("{:?} {:?}", op, v),
                    }
                }
            },
            (
                VStackValue::Constant(Value::Integer {
                    ty:
                        ty1 @ ScalarType {
                            header:
                                ScalarTypeHeader {
                                    vectorsize: None, ..
                                },
                            kind: ScalarTypeKind::Integer { signed: true, .. },
                        },
                    val: val1,
                }),
                VStackValue::Constant(Value::Integer {
                    ty:
                        ty2 @ ScalarType {
                            header:
                                ScalarTypeHeader {
                                    vectorsize: None, ..
                                },
                            kind: ScalarTypeKind::Integer { signed: true, .. },
                        },
                    val: val2,
                }),
            ) if ty1.header.bitsize == ty2.header.bitsize => match op {
                BinaryOp::Cmp | BinaryOp::CmpInt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = match (val1 as i128).cmp(&(val2 as i128)) {
                        std::cmp::Ordering::Less => !0,
                        std::cmp::Ordering::Equal => 0,
                        std::cmp::Ordering::Greater => 1,
                    };

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpLt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = ((val1 as i128) < (val2 as i128)) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpGt => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = ((val1 as i128) > (val2 as i128)) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpLe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = ((val1 as i128) <= (val2 as i128)) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpGe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = ((val1 as i128) >= (val2 as i128)) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpEq => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 == val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                BinaryOp::CmpNe => {
                    let sty = ScalarType {
                        header: ScalarTypeHeader {
                            bitsize: 32,
                            ..Default::default()
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: None,
                            max: None,
                        },
                    };
                    let val = (val1 != val2) as u128;

                    self.push_value(VStackValue::Constant(Value::Integer { ty: sty, val }))
                }
                _ => {
                    let val1 = val1 as i128;
                    let val2 = val2 as i128;
                    let (val, overflow) = match op {
                        BinaryOp::Add => val1.overflowing_add(val2),
                        BinaryOp::Sub => val1.overflowing_sub(val2),
                        BinaryOp::Mul => val1.overflowing_mul(val2),
                        BinaryOp::Div => {
                            if val2 == 0 {
                                (0, true)
                            } else {
                                val1.overflowing_div(val2)
                            }
                        }
                        BinaryOp::Mod => {
                            if val2 == 0 {
                                (0, true)
                            } else {
                                val1.overflowing_rem(val2)
                            }
                        }
                        BinaryOp::BitOr => (val1 | val2, false),
                        BinaryOp::BitAnd => (val1 & val2, false),
                        BinaryOp::BitXor => (val1 ^ val2, false),
                        BinaryOp::Lsh => (
                            val1.wrapping_shl(val2 as u32),
                            val2 > (ty1.header.bitsize.into()),
                        ),
                        BinaryOp::Rsh => (
                            val1.wrapping_shr(val2 as u32),
                            val2 > (ty1.header.bitsize.into()),
                        ),
                        BinaryOp::Cmp
                        | BinaryOp::CmpInt
                        | BinaryOp::CmpLt
                        | BinaryOp::CmpLe
                        | BinaryOp::CmpEq
                        | BinaryOp::CmpNe
                        | BinaryOp::CmpGe
                        | BinaryOp::CmpGt => unreachable!(),
                        op => todo!("{:?}", op),
                    };

                    let overflow = overflow
                        || ((val.leading_zeros() < (ty1.header.bitsize as u32))
                            && (val.leading_ones() < (ty1.header.bitsize as u32)));
                    let val = val & (!((!0i128).wrapping_shl(128 - (ty1.header.bitsize as u32))));

                    let val = (((val as i128) << ((128 - (ty1.header.bitsize - 1)) as u32))
                        >> ((128 - (ty1.header.bitsize - 1)) as u32))
                        as u128; // sign extend signed integers. This makes implementing Cmp et. al above easier

                    match v {
                        OverflowBehaviour::Wrap => self
                            .vstack
                            .push_back(VStackValue::Constant(Value::Integer { ty: ty1, val })),
                        OverflowBehaviour::Unchecked => {
                            if overflow {
                                self.vstack
                                    .push_back(VStackValue::Constant(Value::Uninitialized(
                                        Type::Scalar(ty1),
                                    )))
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }))
                            }
                        }
                        OverflowBehaviour::Checked => {
                            self.vstack
                                .push_back(VStackValue::Constant(Value::Integer { ty: ty1, val }));
                            self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                ty: ScalarType {
                                    header: ScalarTypeHeader {
                                        bitsize: 1,
                                        ..Default::default()
                                    },
                                    kind: ScalarTypeKind::Integer {
                                        signed: false,
                                        min: None,
                                        max: None,
                                    },
                                },
                                val: overflow as u128,
                            }));
                        }
                        OverflowBehaviour::Trap => {
                            if overflow {
                                self.inner.write_trap(Trap::Abort);
                                self.vstack.push_back(VStackValue::Trapped);
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }));
                            }
                        }
                        OverflowBehaviour::Saturate => {
                            if (op == BinaryOp::Sub || op == BinaryOp::Rsh) && overflow {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val: 0,
                                }));
                            } else if overflow {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val: !((!0u128)
                                        .wrapping_shl(128 - (ty1.header.bitsize as u32))),
                                }));
                            } else {
                                self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                    ty: ty1,
                                    val,
                                }));
                            }
                        }
                        v => todo!("{:?} {:?}", op, v),
                    }
                }
            },
            (
                VStackValue::OpaqueScalar(
                    st @ ScalarType {
                        kind: ScalarTypeKind::Integer { .. },
                        ..
                    },
                    loc,
                ),
                VStackValue::Constant(Value::Integer { ty, val }),
            ) if st == ty => {
                let header = st.header;
                match header.vectorsize {
                    None => {
                        if header.bitsize.is_power_of_two()
                            && header.bitsize <= self.inner.native_int_size()
                        {
                            match v {
                                OverflowBehaviour::Wrap | OverflowBehaviour::Unchecked => {
                                    self.inner.write_int_binary_imm(
                                        loc.clone(),
                                        val,
                                        &Type::Scalar(st),
                                        op,
                                    );
                                    self.push_value(VStackValue::OpaqueScalar(st, loc));
                                }
                                v => todo!("{:?} {:?}", op, v),
                            }
                        } else {
                            todo!("Non-native integer")
                        }
                    }
                    Some(vector) => todo!("vectorsize({:?})", vector),
                }
            }
            (a, b) => todo!("{:?}: {:?}, {:?}", op, a, b),
        }
    }

    fn branch_to(&mut self, target: u32) {
        let values = self.pop_values(self.targets[&target].len()).unwrap();
        for (targ_val, val) in self.targets[&target].clone().into_iter().zip(values) {
            let loc = targ_val.opaque_location().unwrap().clone();
            self.move_val(val, loc);
        }

        if !self.locals_opaque {
            let mut locals = std::mem::take(&mut self.locals);

            for (local, _) in &mut locals {
                let val = core::mem::replace(local, VStackValue::Trapped);

                *local = self.make_opaque(val);
            }

            self.locals = locals;
            self.locals_opaque = true;
        }

        self.diverged = true;
        if let StdSome(targ) = self.cfg.get(&target) {
            if targ.fallthrough_from == self.ctarg {
                return;
            }
        }

        self.inner.branch_unconditional(target);
    }

    /// Writes a (potentially conditional) branch to `target` based on `cond`
    pub fn write_branch(&mut self, cond: BranchCondition, target: u32) {
        match cond {
            BranchCondition::Always => {
                self.branch_to(target);
            }
            BranchCondition::Never => {}
            cond => {
                let control = self.pop_value().unwrap();
                match control {
                    VStackValue::Constant(Value::Uninitialized(_))
                    | VStackValue::Constant(Value::Invalid(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.vstack.push_back(VStackValue::Trapped);
                    }
                    VStackValue::Constant(Value::Integer {
                        ty:
                            ScalarType {
                                kind: ScalarTypeKind::Integer { signed, .. },
                                ..
                            },
                        val,
                    }) => {
                        let taken = match cond {
                            BranchCondition::Equal => val == 0,
                            BranchCondition::NotEqual => val != 0,
                            BranchCondition::Less => signed && ((val as i128) < 0),
                            BranchCondition::LessEqual => {
                                (signed && ((val as i128) <= 0)) || val == 0
                            }
                            BranchCondition::Greater => {
                                if signed {
                                    (val as i128) > 0
                                } else {
                                    val > 0
                                }
                            }
                            BranchCondition::GreaterEqual => (!signed) || ((val as i128) >= 0),
                            _ => unreachable!(),
                        };

                        if taken {
                            self.branch_to(target);
                        }
                    }
                    VStackValue::OpaqueScalar(_, _) => todo!("opaque scalar"),
                    VStackValue::CompareResult(_, _) => todo!("compare"),
                    VStackValue::Trapped => {
                        self.push_value(VStackValue::Trapped);
                    }
                    val => panic!("Invalid Branch Control {:?}", val),
                }
            }
        }
    }

    /// Writes an expression in linear order into the codegen
    pub fn write_expr(&mut self, expr: &Expr) {
        if self.diverged {
            return;
        }
        self.print_vstack();
        match expr {
            Expr::Null => {}
            Expr::Const(v) => self.push_value(VStackValue::Constant(v.clone())),
            Expr::Exit { values } => self.write_exit(*values),
            Expr::BinaryOp(op, v) => self.write_binary_op(*op, *v),
            Expr::UnaryOp(_, _) => todo!(),
            Expr::CallFunction(fnty) => {
                let vals = self.pop_values(fnty.params.len()).unwrap();
                let target = self.pop_value().unwrap();
                match target {
                    VStackValue::Constant(Value::GlobalAddress { ty, item }) => {
                        let realty = match &ty {
                            Type::FnType(ty) => &**ty,
                            _ => fnty,
                        };
                        self.call_fn(fnty, realty, &item, vals);
                    }
                    VStackValue::Constant(Value::Invalid(_))
                    | VStackValue::Constant(Value::Uninitialized(_))
                    | VStackValue::Constant(Value::LabelAddress(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    VStackValue::Pointer(pty, lvalue) => {
                        let ty = &*pty.inner;
                        let realty = match &ty {
                            Type::FnType(ty) => &**ty,
                            _ => fnty,
                        };
                        match lvalue {
                            LValue::OpaquePointer(loc) => {
                                self.call_indirect(fnty, realty, loc, vals)
                            }
                            LValue::GlobalAddress(path) => self.call_fn(fnty, realty, &path, vals),
                            _ => {
                                self.inner.write_trap(Trap::Unreachable);
                                self.push_value(VStackValue::Trapped);
                            }
                        }
                    }
                    val => panic!("Invalid value {}", val),
                }
            }
            Expr::Branch { cond, target } => self.write_branch(*cond, *target),
            Expr::BranchIndirect => {
                let val = self.pop_value().unwrap();

                match val {
                    VStackValue::Constant(Value::LabelAddress(n)) => {
                        self.branch_to(n);
                    }
                    VStackValue::Pointer(_, LValue::Label(n)) => self.branch_to(n),
                    VStackValue::Pointer(_, LValue::OpaquePointer(loc)) => {
                        self.inner.branch_indirect(loc);
                    }
                    VStackValue::Pointer(_, _) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.diverged = true;
                    }
                    VStackValue::Trapped => self.diverged = true,
                    val => panic!("Invalid value {}", val),
                }
            }

            Expr::Convert(_, Type::Pointer(pty)) => match self.pop_value().unwrap() {
                VStackValue::Constant(Value::LabelAddress(n)) => {
                    self.push_value(VStackValue::Pointer(pty.clone(), LValue::Label(n)))
                }
                VStackValue::Constant(Value::String { encoding, utf8, .. }) => {
                    self.push_value(VStackValue::Pointer(
                        pty.clone(),
                        LValue::StringLiteral(Encoding::XLang(encoding), utf8.into_bytes()),
                    ))
                }
                VStackValue::Constant(Value::ByteString { content }) => {
                    self.push_value(VStackValue::Pointer(
                        pty.clone(),
                        LValue::StringLiteral(Encoding::Byte, content),
                    ))
                }
                VStackValue::Constant(Value::GlobalAddress { item, .. }) => self.push_value(
                    VStackValue::Pointer(pty.clone(), LValue::GlobalAddress(item)),
                ),
                VStackValue::Constant(Value::Uninitialized(Type::Pointer(_))) => self.push_value(
                    VStackValue::Constant(Value::Uninitialized(Type::Pointer(pty.clone()))),
                ),
                VStackValue::Constant(Value::Invalid(Type::Pointer(_))) => {
                    self.push_value(VStackValue::Trapped);
                    self.inner.write_trap(Trap::Unreachable)
                }
                VStackValue::Pointer(_, lval) => {
                    self.push_value(VStackValue::Pointer(pty.clone(), lval))
                }
                VStackValue::Trapped => {
                    self.push_value(VStackValue::Trapped);
                }
                VStackValue::OpaqueScalar(sty, _) => todo!("{:?} as {:?}", sty, pty),
                val => panic!("Invalid value for convert _ {:?}: {:?}", pty, val),
            },
            Expr::Convert(str, ty) => todo!("convert {:?} {:?}", str, ty),
            Expr::Derive(_, expr) => {
                self.write_expr(expr);
            }
            Expr::Local(n) => {
                let param_cnt = u32::try_from(self.fnty.params.len()).unwrap();
                let ty = if *n < param_cnt {
                    self.fnty.params[*n as usize].clone()
                } else {
                    self.locals[((*n) - param_cnt) as usize].1.clone()
                };
                self.push_value(VStackValue::LValue(ty, LValue::Local(*n)))
            }
            Expr::Pop(n) => {
                for val in self.pop_values((*n).try_into().unwrap()).unwrap() {
                    match val {
                        VStackValue::Constant(Value::Invalid(_)) => {
                            self.inner.write_trap(Trap::Unreachable);
                            self.diverged = true;
                            break;
                        }
                        VStackValue::Trapped => self.diverged = true,
                        _ => {}
                    }
                }
            }
            Expr::Dup(n) => {
                let values = self.pop_values((*n).try_into().unwrap()).unwrap();
                self.push_values(values.clone());
                self.push_values(values);
            }
            Expr::Pivot(n, m) => {
                let vals1 = self.pop_values((*m).try_into().unwrap()).unwrap();
                let vals2 = self.pop_values((*n).try_into().unwrap()).unwrap();
                self.push_values(vals1);
                self.push_values(vals2);
            }
            Expr::Aggregate(ctor) => {
                let vals = self.pop_values(ctor.fields.len()).unwrap();
                self.push_value(VStackValue::AggregatePieced(
                    ctor.ty.clone(),
                    ctor.fields.iter().cloned().zip(vals).collect(),
                ));
            }
            Expr::Member(_) => todo!(),
            Expr::MemberIndirect(_) => todo!(),
            Expr::Assign(_) => {
                let value = self.vstack.pop_back().unwrap();
                let lval = self.vstack.pop_back().unwrap();

                let (_, lval) = match lval {
                    VStackValue::LValue(ty, lval) => (ty, lval),
                    val => panic!("Cannot assign to rvalue stack value {:?}", val),
                };

                match (lval, value) {
                    (LValue::Null, _)
                    | (LValue::Label(_), _)
                    | (LValue::StringLiteral(_, _), _)
                    | (_, VStackValue::Constant(Value::Invalid(_))) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    (_, VStackValue::Trapped) => {
                        self.push_value(VStackValue::Trapped);
                    }
                    (LValue::Local(n), val) => {
                        let loc = self.locals[n as usize].0.clone();

                        match loc {
                            VStackValue::Trapped => {
                                self.push_value(VStackValue::Trapped);
                            }
                            VStackValue::OpaqueScalar(_, loc)
                            | VStackValue::OpaqueAggregate(_, loc)
                            | VStackValue::Pointer(_, LValue::OpaquePointer(loc)) => {
                                self.move_val(val, loc);
                            }
                            _ => {
                                self.locals[n as usize].0 = val;
                            }
                        }
                    }
                    (LValue::OpaquePointer(loc), val) => todo!("store [{:?}] <- {:?}", loc, val),
                    (a, b) => todo!("store {:?} <- {:?}", a, b),
                }
            }
            Expr::AsRValue(_) => {
                let lvalue = self.pop_value().unwrap();
                let (ty, lvalue) = match lvalue {
                    VStackValue::LValue(ty, lvalue) => (ty, lvalue),
                    val => panic!("Invalid value for as_rvalue {:?}", val),
                };

                match lvalue {
                    LValue::Null => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    LValue::Temporary(val) => self.push_value(Box::into_inner(val)),
                    LValue::OpaquePointer(loc) => todo!("as_rvalue [{:?}]", loc),

                    LValue::Local(n) => {
                        let local = self.locals[n as usize].0.clone();
                        if let Some(loc) = local.opaque_location() {
                            let dst = self.inner.allocate(&ty, false);
                            self.inner.move_val(loc.clone(), dst.clone());
                            self.push_opaque(&ty, dst);
                        } else {
                            self.push_value(local)
                        }
                    }
                    LValue::GlobalAddress(_) => todo!("as_rvalue global_address"),
                    LValue::Label(_) => {
                        self.inner.write_trap(Trap::Unreachable);
                        self.push_value(VStackValue::Trapped);
                    }
                    LValue::Field(_, _, _) => todo!("as_rvalue field"),
                    LValue::StringLiteral(_, _) => todo!("as_rvalue string_literal"),
                    LValue::Offset(_, _) => todo!("as_rvalue offset"),
                }
            }
            Expr::CompoundAssign(_, _, _) => todo!(),
            Expr::LValueOp(_, _, _) => todo!(),
            Expr::Indirect => {
                let val = self.pop_value().unwrap();
                match val {
                    VStackValue::Pointer(pty, lval) => {
                        self.push_value(VStackValue::LValue(Box::into_inner(pty.inner), lval))
                    }
                    val => panic!("Invalid value for instruction {:?}", val),
                }
            }
            Expr::AddrOf => {
                let val = self.pop_value().unwrap();
                match val {
                    VStackValue::LValue(ty, lval) => self.push_value(VStackValue::Pointer(
                        PointerType {
                            inner: Box::new(ty),
                            ..Default::default()
                        },
                        lval,
                    )),
                    val => panic!("Invalid value for instruction {:?}", val),
                }
            }
            Expr::Sequence(_) => {}
            Expr::Fence(barrier) => self.inner.write_barrier(*barrier),
            Expr::Switch(Switch::Linear(s)) => {
                let ctrl = self.pop_value().unwrap();
                match ctrl {
                    VStackValue::Constant(Value::Uninitialized(_) | Value::Invalid(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                    }
                    VStackValue::Constant(Value::Integer { val, .. }) => {
                        let val = val.wrapping_sub(s.min);
                        let (idx, rem) = ((val / (s.scale as u128)), val % (s.scale as u128));
                        let target = if rem != 0 || idx >= (s.cases.len() as u128) {
                            s.default
                        } else {
                            s.cases[usize::try_from(idx).unwrap()]
                        };

                        self.branch_to(target);
                    }
                    VStackValue::OpaqueScalar(ty, loc) => {
                        todo!("switch OpaqueScalar({:?},{:?})", ty, loc)
                    }
                    VStackValue::CompareResult(a, b) => {
                        todo!("switch CompareResult({:?},{:?})", a, b)
                    }
                    VStackValue::Trapped => {}
                    v => panic!("Invalid value for switch {:?}", v),
                }
            }
            Expr::Switch(Switch::Hash(s)) => {
                let ctrl = self.pop_value().unwrap();
                match ctrl {
                    VStackValue::Constant(Value::Uninitialized(_) | Value::Invalid(_)) => {
                        self.inner.write_trap(Trap::Unreachable);
                    }
                    VStackValue::Constant(v @ Value::Integer { .. }) => {
                        let mut found = None;
                        for Pair(case, target) in &s.cases {
                            if &v == case {
                                found = Some(*target);
                                break;
                            }
                        }
                        let target = found.unwrap_or(s.default);

                        self.branch_to(target);
                    }
                    VStackValue::OpaqueScalar(ty, loc) => {
                        todo!("switch OpaqueScalar({:?},{:?})", ty, loc)
                    }
                    VStackValue::CompareResult(a, b) => {
                        todo!("switch CompareResult({:?},{:?})", a, b)
                    }
                    VStackValue::Trapped => {}
                    v => panic!("Invalid value for switch {:?}", v),
                }
            }
            Expr::Tailcall(_) => todo!("tailcall"),
        }
    }

    /// Writes the body of a function to the codegen
    pub fn write_function_body(&mut self, body: &FunctionBody) {
        let fnty = self.fnty.clone();
        self.locals.reserve(fnty.params.len());

        for (i, ty) in fnty.params.iter().enumerate() {
            let loc =
                self.inner
                    .get_callconv()
                    .find_param(&fnty, &fnty, u32::try_from(i).unwrap(), true);
            let val = self.opaque_value(ty, loc);
            self.locals.push((val, ty.clone()));
        }

        self.locals.reserve(body.locals.len());
        for ty in &body.locals {
            self.locals.push((
                VStackValue::Constant(Value::Uninitialized(ty.clone())),
                ty.clone(),
            ))
        }
        self.write_block(&body.block, 0);
        if !self.diverged {
            self.inner.leave_function();
        }
    }

    /// Writes the elements of a block to the codegen, usually the top level block of a function
    pub fn write_block(&mut self, block: &Block, _: u32) {
        for item in &block.items {
            match item {
                xlang::ir::BlockItem::Target { num, stack } => {
                    self.cfg
                        .get_or_insert_with_mut(*num, |_| BranchToInfo::default())
                        .fallthrough_from = self.ctarg;

                    self.ctarg = *num;

                    let values = stack
                        .iter()
                        .map(|item| match item {
                            StackItem {
                                kind: StackValueKind::LValue,
                                ty,
                            } => VStackValue::LValue(
                                ty.clone(),
                                LValue::OpaquePointer(self.inner.allocate_lvalue(false)),
                            ),
                            StackItem {
                                kind: StackValueKind::RValue,
                                ty,
                            } => {
                                let loc = self.inner.allocate(ty, false);
                                self.opaque_value(ty, loc)
                            }
                        })
                        .collect();
                    self.targets.insert(*num, values);
                }
                xlang::ir::BlockItem::Expr(expr) => match expr {
                    Expr::Branch {
                        cond: BranchCondition::Never,
                        ..
                    } => {}
                    Expr::Const(Value::LabelAddress(targ)) => {
                        self.cfg
                            .get_or_insert_with_mut(*targ, |_| BranchToInfo::default())
                            .branch_from
                            .insert(self.ctarg);
                    }
                    Expr::Branch { target, .. } => {
                        self.cfg
                            .get_or_insert_with_mut(*target, |_| BranchToInfo::default())
                            .branch_from
                            .insert(self.ctarg);
                    }
                    Expr::Switch(s) => match s {
                        Switch::Linear(LinearSwitch { default, cases, .. }) => {
                            self.cfg
                                .get_or_insert_with_mut(*default, |_| BranchToInfo::default())
                                .branch_from
                                .insert(self.ctarg);

                            for targ in cases {
                                self.cfg
                                    .get_or_insert_with_mut(*targ, |_| BranchToInfo::default())
                                    .branch_from
                                    .insert(self.ctarg);
                            }
                        }
                        Switch::Hash(HashSwitch { default, cases, .. }) => {
                            self.cfg
                                .get_or_insert_with_mut(*default, |_| BranchToInfo::default())
                                .branch_from
                                .insert(self.ctarg);

                            for Pair(_, targ) in cases {
                                self.cfg
                                    .get_or_insert_with_mut(*targ, |_| BranchToInfo::default())
                                    .branch_from
                                    .insert(self.ctarg);
                            }
                        }
                    },
                    _ => {}
                },
            }
        }
        self.ctarg = !0; // reset block position
        for item in &block.items {
            match item {
                xlang::ir::BlockItem::Expr(expr) => self.write_expr(expr),
                xlang::ir::BlockItem::Target { num, .. } => {
                    self.ctarg = *num;
                    if !self.locals_opaque {
                        let mut locals = std::mem::take(&mut self.locals);

                        for (local, _) in &mut locals {
                            let val = core::mem::replace(local, VStackValue::Trapped);

                            *local = self.make_opaque(val);
                        }

                        self.locals = locals;
                        self.locals_opaque = true;
                    }
                    if !self.diverged {
                        let locs = self.targets[num].clone();
                        let vals = self.pop_values(locs.len()).unwrap();
                        for (val, stack_val) in vals.into_iter().zip(locs) {
                            self.move_val(val, stack_val.opaque_location().unwrap().clone());
                        }
                        self.clear_stack();
                        for val in self.targets[num].clone() {
                            self.push_value(val.clone());
                        }
                    } else if let StdSome(br) = self.cfg.get(num) {
                        if !br.branch_from.is_empty() {
                            self.clear_stack();
                            for val in self.targets[num].clone() {
                                self.push_value(val.clone());
                            }
                            self.diverged = false;
                        } else {
                            self.clear_stack();
                        }
                    }

                    self.inner.write_target(*num);
                }
            }
        }
    }
}
