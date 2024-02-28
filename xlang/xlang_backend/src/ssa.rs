use core::cell::Cell;

use std::rc::Rc;

use crate::expr::*;
use crate::mach::Machine;
use crate::ty::TypeInformation;

use arch_ops::traits::InsnWrite;
use xlang::{ir::JumpTarget, targets::properties::TargetProperties};

use xlang::ir;

use xlang::abi::{boxed::Box as XLangBox, collection::HashMap, vec::Vec};

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
    pub ret: OpaqueLocation,
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
                f.write_str(") -> ")?;

                locs.ret.fmt(f)
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
        };

        self.basic_blocks.push_mut(builder)
    }
}

impl<M: Machine> FunctionBuilder<M> {
    /// Writes the function to the given [`InsnWrite`], with the given function to accept a new symbol
    pub fn write<W: InsnWrite, F: FnMut(String, u128)>(
        &mut self,
        out: &mut W,
        mut sym_accepter: F,
    ) -> std::io::Result<()> {
        let mut assigns = self.mach.new_assignments();
        let mut block_clobbers = vec![];

        if let Some(bb) = &self.basic_blocks.first() {
            self.mach.assign_call_conv(
                &mut assigns,
                &self.incoming_locations[&bb.id],
                &self.fnty,
                &self.tys,
            )
        }

        for bb in &self.basic_blocks {
            block_clobbers.push(self.mach.assign_locations(
                &mut assigns,
                &bb.insns,
                self.incoming_locations.get(&bb.id).unwrap(),
                bb.id,
                &self.incoming_locations,
                &self.tys,
            ));
        }

        self.mach.codegen_prologue(&assigns, out)?;
        for (bb, block_clobbers) in self.basic_blocks.iter().zip(block_clobbers) {
            let symbol = format!("{}._B{}", self.sym_name, bb.id);
            sym_accepter(symbol, out.offset() as u128);
            self.mach.codegen_block(
                &assigns,
                &bb.insns,
                block_clobbers,
                out,
                |id| format!("{}._B{}", self.sym_name, id),
                bb.id,
                &self.tys,
            )?;
        }
        Ok(())
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
}

impl<M: Machine> BasicBlockBuilder<M> {
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
                ir::Value::String { .. } => todo!("{}", loc),
                ir::Value::LabelAddress(_) => todo!(),
                ir::Value::Empty => panic!("Empty IR value"),
            },
            VStackValue::LValue(_, _) => todo!(),
            VStackValue::Pointer(_, _) => todo!(),
            VStackValue::OpaqueScalar(_, _) => todo!(),
            VStackValue::AggregatePieced(_, _) => todo!(),
            VStackValue::OpaqueAggregate(_, _) => todo!(),
            VStackValue::CompareResult(_, _) => todo!(),
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
                            ret: ret_loc.clone(),
                        },
                    ));
                    self.write_jump(&next, [ret_loc]);
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
            VStackValue::CompareResult(_, _) => todo!(),
            VStackValue::Trapped => todo!(),
            VStackValue::ArrayRepeat(_, _) => todo!(),
        }
    }

    /// Writes a (possibly [`FALLTHROUGH`][ir::JumpFlags::FALLTHROUGH]) jump to the specified target.
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
            ir::Expr::Aggregate(_) => todo!("aggregate"),
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
            ir::Terminator::Jump(targ) => {
                self.write_jump(targ, core::iter::empty());
            }
            ir::Terminator::Branch(_, _, _) => todo!("branch"),
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
