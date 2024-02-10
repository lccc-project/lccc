#![allow(missing_docs)]
use core::cell::{Cell, RefCell};
use std::panic::Location;
use std::rc::Rc;

use crate::expr::ValLocation;
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SsaInstruction {
    Call(CallTarget, Vec<OpaqueLocation>),
    Jump(u32, Vec<OpaqueLocation>),
    Fallthrough(u32, Vec<OpaqueLocation>),
    Exit(Vec<OpaqueLocation>),
    Tailcall(CallTarget, Vec<OpaqueLocation>),
    Trap(Trap),
    LoadImmediate(OpaqueLocation, u128),
}

impl core::fmt::Display for SsaInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaInstruction::Call(targ, params) => {
                f.write_fmt(format_args!("call {}(", targ))?;
                let mut sep = "";
                for item in params {
                    f.write_str(sep)?;
                    sep = ", ";
                    item.fmt(f)?;
                }
                f.write_str(")")
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CallTarget {
    pub ptr: OpaquePtr,
    pub real_ty: ir::FnType,
    pub call_ty: ir::FnType,
}

impl core::fmt::Display for CallTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {}", self.ptr, self.real_ty))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum OpaquePtr {
    Symbol(String),
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct OpaqueLocation {
    pub ty: Rc<ir::Type>,
    pub kind: ir::StackValueKind,
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
}

impl<M: Machine> FunctionBuilder<M> {
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

    pub fn push_incoming(&mut self, id: u32, incoming: &Vec<ir::StackItem>) {
        let incoming_count = Rc::get_mut(&mut self.incoming_count)
            .expect("new_basic_block may not have been called yet");
        incoming_count.insert(id, incoming.len());
    }

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

    pub fn write<W: InsnWrite, F: FnMut(String, u128)>(
        &mut self,
        out: &mut W,
        mut sym_accepter: F,
    ) -> std::io::Result<()> {
        let mut assigns = self.mach.new_assignments();
        let mut block_clobbers = vec![];
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
    pub fn move_into(&mut self, val: VStackValue<OpaqueLocation>, loc: OpaqueLocation) {
        match val {
            VStackValue::Constant(val) => match val {
                ir::Value::Invalid(_) => self.insns.push(SsaInstruction::Trap(Trap::Unreachable)),
                ir::Value::Uninitialized(_) => {}
                ir::Value::GenericParameter(_) => panic!("Cannot handle generics this late"),
                ir::Value::Integer { val, .. } => {
                    self.insns.push(SsaInstruction::LoadImmediate(loc, val))
                }
                ir::Value::GlobalAddress { ty, item } => todo!(),
                ir::Value::ByteString { content } => todo!(),
                ir::Value::String { encoding, utf8, ty } => todo!(),
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

    pub fn push(&mut self, val: VStackValue<OpaqueLocation>) {
        self.vstack.push(val);
    }

    pub fn pop(&mut self) -> VStackValue<OpaqueLocation> {
        self.vstack
            .pop()
            .expect("BasicBlockBuilder::pop called with an empty stack")
    }

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

    pub fn pop_values(&mut self, n: usize) -> Vec<VStackValue<OpaqueLocation>> {
        self.vstack.split_off_back(n)
    }
    pub fn pop_opaque(&mut self, n: usize) -> Vec<OpaqueLocation> {
        let mut vstack = core::mem::take(&mut self.vstack);

        let ret = vstack
            .drain_back(n)
            .map(|val| self.make_opaque(val))
            .collect();
        self.vstack = vstack;
        ret
    }

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
                    self.insns.push(SsaInstruction::Call(
                        CallTarget {
                            ptr: OpaquePtr::Symbol(sym),
                            real_ty,
                            call_ty,
                        },
                        params,
                    ));
                    self.write_jump(&next);
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

    pub fn write_jump(&mut self, targ: &JumpTarget) {
        let vals = self.incoming_count[&targ.target];

        let vals = self.pop_opaque(vals);

        if targ.flags.contains(ir::JumpTargetFlags::FALLTHROUGH) {
            self.insns
                .push(SsaInstruction::Fallthrough(targ.target, vals));
        } else {
            self.insns.push(SsaInstruction::Jump(targ.target, vals));
        }
    }

    pub fn write_expr(&mut self, expr: &ir::Expr) {
        match expr {
            ir::Expr::Sequence(_) => todo!("sequence"),
            ir::Expr::Const(val) => {
                self.push(VStackValue::Constant(val.clone()));
            }
            ir::Expr::BinaryOp(_, _) => todo!("binary op"),
            ir::Expr::UnaryOp(_, _) => todo!("unary op"),
            ir::Expr::Convert(_, _) => todo!("convert"),
            ir::Expr::Derive(_, _) => todo!("derive"),
            ir::Expr::Local(_) => todo!("local"),
            ir::Expr::Pop(_) => todo!("pop"),
            ir::Expr::Dup(_) => todo!("dup"),
            ir::Expr::Pivot(_, _) => todo!("pivot"),
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

    pub fn write_terminator(&mut self, term: &ir::Terminator) {
        match term {
            ir::Terminator::Jump(targ) => {
                self.write_jump(targ);
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
            ir::Terminator::Exit(_) => todo!("exit"),
            ir::Terminator::Asm(_) => todo!("asm"),
            ir::Terminator::Switch(_) => todo!("switch"),
            ir::Terminator::Unreachable => todo!("unreachable"),
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
