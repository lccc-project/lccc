use core::cell::Cell;

use core::cell::RefCell;
use std::rc::Rc;

use crate::expr::*;
use crate::mach::mce::BuiltMceFunction;
use crate::mach::Machine;
use crate::ty::TypeInformation;

use crate::str::StringMap;

use arch_ops::traits::Address;
use xlang::abi::pair::Pair;
use xlang::ir::BranchCondition;
use xlang::ir::CompareOp;
use xlang::ir::JumpTargetFlags;
use xlang::{ir::JumpTarget, targets::properties::TargetProperties};

use xlang::ir;

use xlang::abi::{
    boxed::Box as XLangBox, collection::HashMap, option::None as XLangNone, vec::Vec,
};

struct SharedCounter(Cell<u32>);

impl SharedCounter {
    const fn new() -> Self {
        Self(Cell::new(0))
    }
    fn next(&self) -> u32 {
        let val = self.0.get();
        self.0.set(val + 1);

        val
    }
}

/// An Aggregate type containing the parameter and return locations of an [`SsaInstruction::Call`]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallLocations {
    /// The locations of the parameters
    pub params: Vec<OpaqueLocation>,
    /// The location of the return value
    pub ret: Option<OpaqueLocation>,
}

/// An instruction for ssa codegen lowered from a [`Expr`][xlang::ir::Expr]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaInstruction {
    /// Performs a call to the given [`CallTarget`] with the parameters in the specified [`OpaqueLocation`]s
    Call(CallTarget, CallLocations),
    /// Jumps to the destination basic block, mapping the given `OpaqueLocation`s to the incoming locations in the destination.
    Jump(u32, Vec<OpaqueLocation>),
    /// Falls through to the destination basic block, mapping the given `OpaqueLocation`s to the incoming locations in the destination.
    /// The Code generator may generate code that is invalid if the destination basic block does not immediately follow the current basic block
    Fallthrough(u32, Vec<OpaqueLocation>),
    /// Exits from the function, with the values in the given location
    Exit(Vec<OpaqueLocation>),
    /// Performs a tailcall to the given [`CallTarget`] with the parameters in the specified [`OpaqueLocation`]s
    Tailcall(CallTarget, Vec<OpaqueLocation>),
    /// Executes the specified [`Trap`]
    Trap(Trap),
    /// Loads a scalar immediate value into the given [`OpaqueLocation`]
    LoadImmediate(OpaqueLocation, u128),
    /// Loads an address
    LoadSymAddr(OpaqueLocation, Address),
    /// Initializes a (potentially large) memory location with zeroes
    ZeroInit(OpaqueLocation),
    /// Conditionally branches to the destination basic block if the comparison is satisfied.
    Branch(
        BranchCondition,
        OpaqueLocation,
        OpaqueLocation,
        u32,
        Vec<OpaqueLocation>,
    ),
    /// Conditionally branches to the destination basic block if the comparison with the value `0` is satisfied.
    BranchZero(BranchCondition, OpaqueLocation, u32, Vec<OpaqueLocation>),
}

impl core::fmt::Display for SsaInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaInstruction::Call(targ, locs) => {
                f.write_fmt(format_args!("call {}(", targ))?;
                let mut sep = "";
                for item in &locs.params {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str(")")?;
                if let Some(ret) = &locs.ret {
                    f.write_str(" -> ")?;
                    ret.fmt(f)?;
                }
                Ok(())
            }
            SsaInstruction::Jump(val, stack) => {
                f.write_fmt(format_args!("jump @{} [", val))?;
                let mut sep = "";
                for item in stack {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
            SsaInstruction::Fallthrough(val, stack) => {
                f.write_fmt(format_args!("fallthrough @{} [", val))?;
                let mut sep = "";
                for item in stack {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
            SsaInstruction::Exit(estack) => {
                f.write_fmt(format_args!("exit ["))?;
                let mut sep = "";
                for item in estack {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
            SsaInstruction::Tailcall(targ, params) => {
                f.write_fmt(format_args!("tailcall {}(", targ))?;
                let mut sep = "";
                for item in params {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str(")")
            }
            SsaInstruction::Trap(trap) => f.write_fmt(format_args!("trap {}", trap)),
            SsaInstruction::LoadImmediate(dest, val) => {
                f.write_fmt(format_args!("loadimm {}, {}", dest, val))
            }
            SsaInstruction::LoadSymAddr(dest, addr) => {
                f.write_fmt(format_args!("loadsymaddr {}, {}", dest, addr))
            }
            SsaInstruction::ZeroInit(dest) => f.write_fmt(format_args!("zeroinit {}", dest)),
            SsaInstruction::Branch(op, l, r, dest, stack) => {
                f.write_fmt(format_args!("branch {op} ({l}, {r}) @{dest} ["))?;
                let mut sep = "";
                for item in stack {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
            SsaInstruction::BranchZero(op, l, dest, stack) => {
                f.write_fmt(format_args!("branch {op} ({l}, 0) @{dest} ["))?;
                let mut sep = "";
                for item in stack {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str("]")
            }
        }
    }
}

/// Specifies the target of a call site for either [`SsaInstruction::Call`] or [`SsaInstruction::Tailcall`]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallTarget {
    /// The pointer to call, which either may be inside an indirect location or a symbol name
    pub ptr: OpaquePtr,
    /// The real (def) type of the pointer, used for computing the calling convention of the function.
    ///
    /// This may differ from the `call_ty` if the function is variadic
    pub real_ty: ir::FnType,
    /// The callsite type of the pointer, used for computing the calling convention of the function.
    pub call_ty: ir::FnType,
}

impl core::fmt::Display for CallTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {}", self.ptr, self.real_ty))
    }
}

/// A pointer which can be directly called or read from
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum OpaquePtr {
    /// A symbol, such as a global variable, function, or label addres
    Symbol(String),
    /// A pointer already stored in another location (where the value is unknown)
    Pointer(OpaqueLocation),
}

impl core::fmt::Display for OpaquePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpaquePtr::Symbol(s) => f.write_str(s),
            OpaquePtr::Pointer(ptr) => f.write_fmt(format_args!("[{}]", ptr)),
        }
    }
}

/// An opaque value location
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OpaqueLocation {
    /// The type of the stored value
    pub ty: Rc<ir::Type>,
    /// The kind (lvalue or rvalue) of the stored value.
    pub kind: ir::StackValueKind,
    /// The unique identifier of the value location
    pub num: u32,
    has_addr: bool,
}

impl core::fmt::Display for OpaqueLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%{}: ", self.num))?;
        if self.kind == ir::StackValueKind::LValue {
            f.write_str("lvalue ")?;
        }
        self.ty.fmt(f)
    }
}

impl ValLocation for OpaqueLocation {
    fn addressible(&self) -> bool {
        self.has_addr
    }

    fn val_type(&self) -> &ir::Type {
        &self.ty
    }
}

/// A builder that can convert [`ir::FunctionBody`]s into Machine code, lowering through [`SsaInstruction`]s.
pub struct FunctionBuilder<M> {
    tys: Rc<TypeInformation>,
    sym_name: String,
    mach: Rc<M>,
    basic_blocks: Vec<BasicBlockBuilder<M>>,
    target: &'static TargetProperties<'static>,
    locals: Rc<Vec<OpaqueLocation>>,
    loc_id_counter: Rc<SharedCounter>,
    incoming_locations: HashMap<u32, Vec<OpaqueLocation>>,
    incoming_count: Rc<HashMap<u32, usize>>,
    fnty: Rc<ir::FnType>,
    string_interner: Rc<RefCell<StringMap>>,
}

impl<M> FunctionBuilder<M> {
    /// Constructs a new [`FunctionBuilder`] for the function, that uses the given [`Machine`], [`TypeInformation`], and [`TargetProperties`].
    ///
    /// `sym_name` and `fnty` are the symbol (mangled) named and definition type of the function definition, respectively.
    pub fn new(
        sym_name: String,
        mach: Rc<M>,
        tys: Rc<TypeInformation>,
        target: &'static TargetProperties<'static>,
        fnty: Rc<ir::FnType>,
        string_interner: Rc<RefCell<StringMap>>,
    ) -> Self {
        Self {
            sym_name,
            mach,
            tys,
            target,
            basic_blocks: Vec::new(),
            locals: Rc::new(Vec::new()),
            loc_id_counter: Rc::new(SharedCounter::new()),
            incoming_locations: HashMap::new(),
            fnty,
            incoming_count: Rc::new(HashMap::new()),
            string_interner,
        }
    }

    /// Inserts a new local variable of type `ty`
    pub fn push_local(&mut self, ty: ir::Type) {
        Rc::get_mut(&mut self.locals)
            .expect("No basic blocks may have been pushed yet")
            .push(OpaqueLocation {
                ty: Rc::new(ty),
                kind: ir::StackValueKind::RValue,
                num: self.loc_id_counter.next(),
                has_addr: true,
            })
    }

    /// Allocates space for the incoming set of basic block `id`, according to `incoming`
    pub fn push_incoming(&mut self, id: u32, incoming: &Vec<ir::StackItem>) {
        let incoming_count = Rc::get_mut(&mut self.incoming_count)
            .expect("new_basic_block may not have been called yet");
        incoming_count.insert(id, incoming.len());
    }

    /// Creates a new [`BasicBlockBuilder`] that refers to basic block `id`, with the given `incoming` stack values.
    pub fn new_basic_block(
        &mut self,
        id: u32,
        incoming: &Vec<ir::StackItem>,
    ) -> &mut BasicBlockBuilder<M> {
        let mut vstack = Vec::new();
        let mut incoming_locs = Vec::new();

        for nloc in incoming {
            let ty = nloc.ty.clone();
            let loc = OpaqueLocation {
                ty: Rc::new(ty.clone()),
                kind: nloc.kind,
                num: self.loc_id_counter.next(),
                has_addr: false,
            };

            match nloc.kind {
                ir::StackValueKind::LValue => {
                    vstack.push(VStackValue::opaque_lvalue(ty, loc.clone()))
                }
                ir::StackValueKind::RValue => {
                    vstack.push(VStackValue::opaque_value(ty, loc.clone()))
                }
            }
            incoming_locs.push(loc);
        }

        self.incoming_locations.insert(id, incoming_locs);

        let builder = BasicBlockBuilder {
            id,
            tys: self.tys.clone(),
            mach: self.mach.clone(),
            insns: Vec::new(),
            target: self.target,
            locals: self.locals.clone(),
            vstack,
            incoming_count: self.incoming_count.clone(),
            loc_id_counter: self.loc_id_counter.clone(),
            string_interner: self.string_interner.clone(),
        };

        self.basic_blocks.push_mut(builder)
    }
}

impl<M: Machine<SsaInstruction>> FunctionBuilder<M> {
    /// Builds the [`SsaInstruction`] function into a [`BuiltMceFunction`]
    pub fn build(&self) -> BuiltMceFunction<M::Instruction> {
        let mut assignments = self.mach.new_assignments();

        let mut block_clobbers = Vec::new();
        let mut call_first = false;

        for bb in &self.basic_blocks {
            let incoming = &self.incoming_locations[&bb.id];
            if !call_first {
                call_first = true;
                self.mach.assign_call_conv(
                    &mut assignments,
                    incoming,
                    &self.fnty,
                    &self.tys,
                    bb.id,
                );
            }
            block_clobbers.push(self.mach.assign_locations(
                &mut assignments,
                &bb.insns,
                incoming,
                bb.id,
                &self.incoming_locations,
                &self.tys,
            ));
        }

        let mut mce = BuiltMceFunction::new(self.mach.codegen_prologue(&assignments));

        for (bb, clobbers) in self.basic_blocks.iter().zip(block_clobbers) {
            let st = format!("{}._T{}", self.sym_name, bb.id);
            let insns = self.mach.codegen_block(
                &assignments,
                &bb.insns,
                clobbers,
                |id| format!("{}._T{}", self.sym_name, id),
                bb.id,
                &self.tys,
            );
            mce.push_basic_block(st, insns);
        }

        mce
    }
}

impl<M> core::fmt::Display for FunctionBuilder<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{\n")?;
        for (i, loc) in self.locals.iter().enumerate() {
            f.write_fmt(format_args!("  local _{}: {};\n", i, loc.ty))?;
        }
        for bb in &self.basic_blocks {
            bb.fmt(f)?;
        }
        f.write_str("}\n")?;
        Ok(())
    }
}

/// A builder for code generation of [`ir::Block`]s into [`SsaInstruction`]s
#[allow(dead_code)] // ignore unused variables, they'll be used when there's less `todo!()`
pub struct BasicBlockBuilder<M> {
    id: u32,
    tys: Rc<TypeInformation>,
    mach: Rc<M>,
    insns: Vec<SsaInstruction>,
    target: &'static TargetProperties<'static>,
    locals: Rc<Vec<OpaqueLocation>>,
    vstack: Vec<VStackValue<OpaqueLocation>>,
    incoming_count: Rc<HashMap<u32, usize>>,
    loc_id_counter: Rc<SharedCounter>,
    string_interner: Rc<RefCell<StringMap>>,
}

impl<M: Machine<SsaInstruction>> BasicBlockBuilder<M> {
    /// Moves the given [`VStackValue`] into the specified [`OpaqueLocation`], generating appropriate loads and move instructions to place both transparent and opaque values in the new location
    pub fn move_into(&mut self, val: VStackValue<OpaqueLocation>, loc: OpaqueLocation) {
        match val {
            VStackValue::Constant(val) => match val {
                ir::Value::Invalid(_) => self.insns.push(SsaInstruction::Trap(Trap::Unreachable)),
                ir::Value::Uninitialized(_) => {}
                ir::Value::GenericParameter(_) => panic!("Cannot handle generics this late"),
                ir::Value::Integer { val, .. } => {
                    self.insns.push(SsaInstruction::LoadImmediate(loc, val))
                }
                ir::Value::GlobalAddress { .. } => todo!(),
                ir::Value::ByteString { .. } => todo!(),
                ir::Value::String {
                    encoding,
                    utf8,
                    ty: ir::Type::Pointer(_),
                } => {
                    let mut string_map = self.string_interner.borrow_mut();
                    let sym = string_map.get_string_symbol(
                        utf8.into_bytes(),
                        crate::str::Encoding::XLang(encoding),
                    );

                    self.insns.push(SsaInstruction::LoadSymAddr(
                        loc,
                        Address::Symbol {
                            name: sym.to_string(),
                            disp: 0,
                        },
                    ));
                }
                ir::Value::String { ty, .. } => todo!("String const as {}", ty),
                ir::Value::LabelAddress(_) => todo!(),
                ir::Value::Empty => panic!("Empty IR value"),
            },
            VStackValue::LValue(_, _) => todo!(),
            VStackValue::Pointer(_, _) => todo!(),
            VStackValue::OpaqueScalar(_, _) => todo!(),
            VStackValue::AggregatePieced(ty, fields) => {
                let aggregate_info = self
                    .tys
                    .aggregate_layout(&ty)
                    .expect("Required a aggregate type");
                self.insns.push(SsaInstruction::ZeroInit(loc.clone()));
                for Pair(field, val) in fields {
                    todo!(
                        "{} (offset {}): {}",
                        field,
                        aggregate_info.fields[&field].0,
                        val
                    );
                }
            }
            VStackValue::OpaqueAggregate(_, _) => todo!(),
            VStackValue::CompareResult(_, _, _, _) => todo!(),
            VStackValue::Trapped => todo!(),
            VStackValue::ArrayRepeat(_, _) => todo!(),
        }
    }

    /// Pushes a single [`VStackValue`] to the basic block's eval stack
    pub fn push(&mut self, val: VStackValue<OpaqueLocation>) {
        self.vstack.push(val);
    }

    /// Pushes all of the specified [`VStackValue`]s to the basic block's eval stack

    pub fn push_values<I: IntoIterator<Item = VStackValue<OpaqueLocation>>>(&mut self, vals: I) {
        self.vstack.extend(vals)
    }

    /// Pops a single [`VStackValue`] from the basic block's eval stack
    ///
    /// ## Panics
    /// Panics if the eval stack is empty at the current location
    pub fn pop(&mut self) -> VStackValue<OpaqueLocation> {
        self.vstack
            .pop()
            .expect("BasicBlockBuilder::pop called with an empty stack")
    }

    /// Pops `N` values from the basic block's eval stack as an array, to allow easier destructuring of several values
    ///
    /// ## Panics
    /// Panics if the eval stack has fewer than `N` values at the current location
    pub fn pop_values_static<const N: usize>(&mut self) -> [VStackValue<OpaqueLocation>; N] {
        use core::mem::MaybeUninit;
        let mut val = MaybeUninit::<[_; N]>::uninit();
        let mut ptr = val.as_mut_ptr().cast::<VStackValue<OpaqueLocation>>();

        for val in self.vstack.drain_back(N) {
            unsafe { ptr.write(val) };
            ptr = unsafe { ptr.add(1) };
        }
        // SAFETY: We just initialized all `N` elements
        unsafe { val.assume_init() }
    }

    /// Pops `n` values from the basic block's eval stack and returns them as a vector.
    ///
    /// ## Panics
    /// Panics if the eval stack has fewer than `n` values at the current location
    pub fn pop_values(&mut self, n: usize) -> Vec<VStackValue<OpaqueLocation>> {
        self.vstack.split_off_back(n)
    }

    /// Pops `n` values as opaque locations from the basic block's eval stack and returns them as a vector, moving them into a new [`OpaqueLocation`] if necessary
    ///
    /// ## Panics
    /// Panics if the eval stack has fewer than `n` values at the current location
    pub fn pop_opaque(&mut self, n: usize) -> Vec<OpaqueLocation> {
        let mut vstack = core::mem::take(&mut self.vstack);

        let ret = vstack
            .drain_back(n)
            .map(|val| self.make_opaque(val))
            .collect();
        self.vstack = vstack;
        ret
    }

    /// Pops `N` values as opaque locations from the basic block's eval stack and returns them as a vector, moving them into a new [`OpaqueLocation`] if necessary.
    ///
    /// This provides better destructuring when the number of values is known.
    pub fn pop_opaque_static<const N: usize>(&mut self) -> [OpaqueLocation; N] {
        use core::mem::MaybeUninit;
        let mut val = MaybeUninit::<[_; N]>::uninit();
        let mut ptr = val.as_mut_ptr().cast::<OpaqueLocation>();
        let mut vstack = core::mem::take(&mut self.vstack);

        for val in vstack.drain_back(N) {
            unsafe { ptr.write(self.make_opaque(val)) };
            ptr = unsafe { ptr.add(1) };
        }
        self.vstack = vstack;
        // SAFETY: We just initialized all `N` elements
        unsafe { val.assume_init() }
    }

    /// Returns the [`OpaqueLocation`] currently storing the specified [`VStackValue`], if any,
    ///  or otherwise allocates a new [`OpaqueLocation`] and moves `val` into it as though by [`BasicBlockBuilder::move_into`]
    pub fn make_opaque(&mut self, val: VStackValue<OpaqueLocation>) -> OpaqueLocation {
        if let Some(loc) = val.opaque_location() {
            loc.clone()
        } else {
            let loc = match &val {
                VStackValue::LValue(ty, _) => OpaqueLocation {
                    ty: Rc::new(ty.clone()),
                    kind: ir::StackValueKind::LValue,
                    num: self.loc_id_counter.next(),
                    has_addr: false,
                },
                val => OpaqueLocation {
                    ty: Rc::new(val.value_type()),
                    kind: ir::StackValueKind::RValue,
                    num: self.loc_id_counter.next(),
                    has_addr: false,
                },
            };

            self.move_into(val, loc.clone());
            loc
        }
    }

    /// Writes a function call with the given parameters to the given target using the specified `call_ty`.
    ///
    /// If `next` is Some, treats this as a normal call, and ends by jumping to the `next`.
    /// Otherwise, treats this as a tail call, with no following jump.
    pub fn write_call(
        &mut self,
        targ: VStackValue<OpaqueLocation>,
        params: Vec<VStackValue<OpaqueLocation>>,
        call_ty: ir::FnType,
        next: Option<ir::JumpTarget>,
    ) {
        match targ {
            VStackValue::Constant(ir::Value::GlobalAddress { ty, item }) => {
                let real_ty = match ty {
                    ir::Type::FnType(fnty) => XLangBox::into_inner(fnty),
                    ty => panic!("Cannot call a global constant of type {}", ty),
                };

                let params = params
                    .into_iter()
                    .map(|val| self.make_opaque(val))
                    .collect::<Vec<_>>();

                let sym = match &*item.components {
                    [ir::PathComponent::Text(name)]
                    | [ir::PathComponent::Root, ir::PathComponent::Text(name)] => name.to_string(),
                    [ir::PathComponent::Root, rest @ ..] | [rest @ ..] => self.mach.mangle(rest),
                };

                if let Some(next) = next {
                    if call_ty.ret == ir::Type::Void {
                        self.insns.push(SsaInstruction::Call(
                            CallTarget {
                                ptr: OpaquePtr::Symbol(sym),
                                real_ty,
                                call_ty,
                            },
                            CallLocations { params, ret: None },
                        ));
                        self.write_jump(&next, []);
                    } else {
                        let ret_loc = OpaqueLocation {
                            ty: Rc::new(real_ty.ret.clone()),
                            kind: ir::StackValueKind::RValue,
                            num: self.loc_id_counter.next(),
                            has_addr: false,
                        };
                        self.insns.push(SsaInstruction::Call(
                            CallTarget {
                                ptr: OpaquePtr::Symbol(sym),
                                real_ty,
                                call_ty,
                            },
                            CallLocations {
                                params,
                                ret: Some(ret_loc.clone()),
                            },
                        ));
                        self.write_jump(&next, [ret_loc]);
                    }
                } else {
                    self.insns.push(SsaInstruction::Tailcall(
                        CallTarget {
                            ptr: OpaquePtr::Symbol(sym),
                            real_ty,
                            call_ty,
                        },
                        params,
                    ));
                }
            }
            VStackValue::Constant(val) => todo!("{}", val),
            VStackValue::LValue(_, _) => todo!(),
            VStackValue::Pointer(_, _) => todo!(),
            VStackValue::OpaqueScalar(_, _) => todo!(),
            VStackValue::AggregatePieced(_, _) => todo!(),
            VStackValue::OpaqueAggregate(_, _) => todo!(),
            VStackValue::CompareResult(_, _, _, _) => todo!(),
            VStackValue::Trapped => todo!(),
            VStackValue::ArrayRepeat(_, _) => todo!(),
        }
    }

    /// Writes a (possibly [`FALLTHROUGH`][ir::JumpTargetFlags::FALLTHROUGH]) jump to the specified target.
    ///
    /// `head_vals` contains value placed on the head of the incoming target stack, which much be at most the total number of incoming values
    pub fn write_jump<H: IntoIterator<Item = OpaqueLocation>>(
        &mut self,
        targ: &JumpTarget,
        head_vals: H,
    ) where
        H::IntoIter: ExactSizeIterator,
    {
        let head_vals = head_vals.into_iter();
        let head_count = head_vals.len();
        let vals = self.incoming_count[&targ.target] - head_count;

        let mut vals = self.pop_opaque(vals);

        vals.extend(head_vals);

        if targ.flags.contains(ir::JumpTargetFlags::FALLTHROUGH) {
            self.insns
                .push(SsaInstruction::Fallthrough(targ.target, vals));
        } else {
            self.insns.push(SsaInstruction::Jump(targ.target, vals));
        }
    }

    /// Writes the given [`ir::Expr`] to the block, generating all necessary instructions, and modifying the basic block's eval stack according to the results of the expression
    ///
    /// ## Panics
    /// The function may panic (or otherwise have incorrect behaviour) if the current state of the basic block's eval stack violates the input constraints in the "Typechecking" specification for `expr`,
    ///  or if the basic block terminator has already been written.
    pub fn write_expr(&mut self, expr: &ir::Expr) {
        match expr {
            ir::Expr::Sequence(_) => todo!("sequence"),
            ir::Expr::Const(val) => {
                self.push(VStackValue::Constant(val.clone()));
            }
            ir::Expr::BinaryOp(_, _) => todo!("binary op"),
            ir::Expr::UnaryOp(_, _) => todo!("unary op"),
            ir::Expr::Convert(strength, new_ty) => match (self.pop(), strength, new_ty) {
                (
                    VStackValue::Pointer(_, val),
                    ir::ConversionStrength::Reinterpret,
                    ir::Type::Pointer(new_ty),
                ) => {
                    self.push(VStackValue::Pointer(new_ty.clone(), val));
                }
                (
                    VStackValue::Constant(ir::Value::String { encoding, utf8, .. }),
                    ir::ConversionStrength::Reinterpret,
                    ir::Type::Pointer(new_ty),
                ) => {
                    self.push(VStackValue::Constant(ir::Value::String {
                        encoding,
                        utf8,
                        ty: ir::Type::Pointer(new_ty.clone()),
                    }));
                }
                (
                    VStackValue::Constant(ir::Value::Integer {
                        ty:
                            ir::ScalarType {
                                header:
                                    ir::ScalarTypeHeader {
                                        vectorsize: XLangNone,
                                        ..
                                    },
                                kind: ir::ScalarTypeKind::Integer { .. },
                            },
                        val,
                    }),
                    _,
                    ir::Type::Scalar(
                        sty @ ir::ScalarType {
                            header:
                                ir::ScalarTypeHeader {
                                    bitsize: b2,
                                    vectorsize: XLangNone,
                                    ..
                                },
                            kind: ir::ScalarTypeKind::Integer { signed: s2, .. },
                        },
                    ),
                ) => {
                    let mask = !0u128 >> (128 - *b2);

                    let signext = *s2;
                    let signmask = !0u128 << *b2;
                    let mut val = val & mask;

                    if *b2 != 0 && signext {
                        let sbit = (!(val >> (*b2 - 1))).wrapping_add(1);
                        val |= sbit & signmask;
                    }

                    let val = ir::Value::Integer { ty: *sty, val };

                    self.push(VStackValue::Constant(val));
                }
                x => todo!("{:?}", x),
            },
            ir::Expr::Derive(_, _) => todo!("derive"),
            ir::Expr::Local(_) => todo!("local"),
            ir::Expr::Pop(n) => {
                self.pop_values(*n as usize);
            }
            ir::Expr::Dup(n) => {
                let vals = self.pop_values(*n as usize);

                self.push_values(vals.clone());
                self.push_values(vals);
            }
            ir::Expr::Pivot(m, n) => {
                let first = self.pop_values(*n as usize);
                let second = self.pop_values(*m as usize);

                self.push_values(first);
                self.push_values(second);
            }
            ir::Expr::Aggregate(ctor) => {
                let ty = ctor.ty.clone();

                let field_vals = self.pop_values(ctor.fields.len());

                let fields = ctor
                    .fields
                    .iter()
                    .map(|s| s.to_string())
                    .zip(field_vals)
                    .collect();

                self.push(VStackValue::AggregatePieced(ty, fields));
            }
            ir::Expr::Member(_) => todo!("member"),
            ir::Expr::MemberIndirect(_) => todo!("member indirect"),
            ir::Expr::Assign(_) => todo!("assign"),
            ir::Expr::AsRValue(_) => todo!("as_rvalue"),
            ir::Expr::CompoundAssign(_, _, _) => todo!("compound_assign"),
            ir::Expr::FetchAssign(_, _, _) => todo!("fetch_assign"),
            ir::Expr::LValueOp(_, _) => todo!("lvalue op"),
            ir::Expr::UnaryLValue(_, _, _) => todo!("unary lvalue op"),
            ir::Expr::Indirect => todo!("indirect"),
            ir::Expr::AddrOf => todo!("addr_of"),
            ir::Expr::Fence(_) => todo!("fence"),
            ir::Expr::BeginStorage(_) => todo!("begin storage"),
            ir::Expr::EndStorage(_) => todo!("end storage"),
            ir::Expr::Select(_) => todo!("select"),
            ir::Expr::CompareOp(op, sty) => {
                let [val1, val2] = self.pop_opaque_static();

                self.push(VStackValue::CompareResult(*op, *sty, val1, val2));
            }
        }
    }

    /// Writes the given terminator to the basic block.
    /// The eval stack is left in an unspecified state,
    ///  and operations that place constraints on the stack of the eval stack may not be validly used (they may panic or produce incorrect results)
    ///
    /// ## Panics
    /// The function may panic (or otherwise have incorrect behaviour) if the current state of the basic block's eval stack violates the input constraints in the "Typechecking" specification for `term`,
    ///  or if the basic block terminator has already been written.
    pub fn write_terminator(&mut self, term: &ir::Terminator) {
        match term {
            ir::Terminator::Empty => panic!("Invalid (empty) Terminator"),
            ir::Terminator::Jump(targ) => {
                self.write_jump(targ, core::iter::empty());
            }
            ir::Terminator::Branch(cond, targ1, targ2) => {
                let cond_val = self.pop();

                let (real_cond, val1, val2) = match (cond_val, cond) {
                    (_, cond @ (BranchCondition::Always | BranchCondition::Never)) => {
                        (*cond, None, None)
                    }
                    (VStackValue::CompareResult(CompareOp::Cmp, _, l, r), cond) => {
                        (*cond, Some(l), Some(r))
                    }
                    (
                        VStackValue::CompareResult(CompareOp::CmpEq, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpNe, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::NotEqual, Some(l), Some(r)),
                    (
                        VStackValue::CompareResult(CompareOp::CmpNe, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpEq, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::NotEqual, Some(l), Some(r)),
                    (
                        VStackValue::CompareResult(CompareOp::CmpLt, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpGe, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::Less, Some(l), Some(r)),
                    (
                        VStackValue::CompareResult(CompareOp::CmpLe, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpGt, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::LessEqual, Some(l), Some(r)),
                    (
                        VStackValue::CompareResult(CompareOp::CmpGt, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpLe, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::Greater, Some(l), Some(r)),
                    (
                        VStackValue::CompareResult(CompareOp::CmpGe, _, l, r),
                        BranchCondition::NotEqual,
                    )
                    | (
                        VStackValue::CompareResult(CompareOp::CmpLt, _, l, r),
                        BranchCondition::Equal,
                    ) => (BranchCondition::GreaterEqual, Some(l), Some(r)),
                    (val, cond) => {
                        let loc = self.make_opaque(val);
                        (*cond, Some(loc), None)
                    }
                };

                let top_values_count =
                    self.incoming_count[&targ1.target].min(self.incoming_count[&targ2.target]);

                let targ1_skip = top_values_count - self.incoming_count[&targ1.target];
                let targ2_skip = top_values_count - self.incoming_count[&targ2.target];

                let top_values = self.pop_opaque(top_values_count);

                match real_cond {
                    BranchCondition::Always => {
                        self.write_jump(targ1, top_values.into_iter().skip(targ1_skip));
                    }
                    BranchCondition::Never => {
                        self.write_jump(targ2, top_values.into_iter().skip(targ2_skip));
                    }
                    cond => {
                        if targ1.flags.contains(JumpTargetFlags::FALLTHROUGH)
                            && targ2.flags.contains(JumpTargetFlags::FALLTHROUGH)
                        {
                            // Branches must be the same, fallthrough to take your pick of target
                            self.write_jump(targ1, top_values.into_iter().skip(targ1_skip))
                        } else if targ1.flags.contains(JumpTargetFlags::FALLTHROUGH)
                            || targ2.flags.contains(JumpTargetFlags::COLD)
                        {
                            let cond = cond.invert();
                            let targ2_vals = top_values.iter().skip(targ2_skip).cloned().collect();

                            match (val1, val2) {
                                (Some(l), Some(r)) => self.insns.push(SsaInstruction::Branch(
                                    cond,
                                    l,
                                    r,
                                    targ2.target,
                                    targ2_vals,
                                )),
                                (Some(l), None) => self.insns.push(SsaInstruction::BranchZero(
                                    cond,
                                    l,
                                    targ2.target,
                                    targ2_vals,
                                )),
                                (None, _) => unreachable!(
                                    "Left compare val always provided for non-trivial conditions"
                                ),
                            }
                            self.write_jump(&targ1, top_values.into_iter().skip(targ1_skip));
                        } else {
                            let targ1_vals = top_values.iter().skip(targ1_skip).cloned().collect();

                            match (val1, val2) {
                                (Some(l), Some(r)) => self.insns.push(SsaInstruction::Branch(
                                    cond,
                                    l,
                                    r,
                                    targ1.target,
                                    targ1_vals,
                                )),
                                (Some(l), None) => self.insns.push(SsaInstruction::BranchZero(
                                    cond,
                                    l,
                                    targ1.target,
                                    targ1_vals,
                                )),
                                (None, _) => unreachable!(
                                    "Left compare val always provided for non-trivial conditions"
                                ),
                            }
                            self.write_jump(&targ2, top_values.into_iter().skip(targ1_skip));
                        }
                    }
                }
            }
            ir::Terminator::BranchIndirect => todo!("branch indirect"),
            ir::Terminator::Call(_, call_fnty, next) => {
                let params_count = call_fnty.params.len();
                let vals = self.pop_values(params_count);

                let target = self.pop();

                self.write_call(target, vals, (**call_fnty).clone(), Some(*next));
            }
            ir::Terminator::Tailcall(_, call_fnty) => {
                let params_count = call_fnty.params.len();
                let vals = self.pop_values(params_count);

                let target = self.pop();

                self.write_call(target, vals, (**call_fnty).clone(), None);
            }
            ir::Terminator::Exit(n) => {
                let vals = self.pop_opaque(*n as usize);
                self.insns.push(SsaInstruction::Exit(vals));
            }
            ir::Terminator::Asm(_) => todo!("asm"),
            ir::Terminator::Switch(_) => todo!("switch"),
            ir::Terminator::Unreachable => self.insns.push(SsaInstruction::Trap(Trap::Unreachable)),
        }
    }
}

impl<M> core::fmt::Display for BasicBlockBuilder<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("  target @{}:\n", self.id))?;

        for insn in &self.insns {
            f.write_fmt(format_args!("    {};\n", insn))?;
        }
        Ok(())
    }
}
