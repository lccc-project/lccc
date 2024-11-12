#![allow(unused_variables)]
use std::{collections::VecDeque, ops::Deref, rc::Rc, str::FromStr};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        codegen::{
            X86CodegenOpcode, X86Displacement, X86Encoder, X86Instruction, X86MemoryOperand,
            X86Operand,
        },
        features::X86Feature,
        X86Mode, X86Register, X86RegisterClass,
    },
};
use target_tuples::Architecture;
use xlang::{
    abi::{
        collection::{HashMap, HashSet},
        pair::Pair,
    },
    targets::properties::TargetProperties,
};
use xlang_backend::{
    callconv::CallingConvention,
    expr::ValLocation,
    mc::{MCInsn, MCWriter, MachineFeatures, MaybeResolved},
    ty::TypeInformation,
};
use xlang_struct::{BinaryOp, BranchCondition, CharFlags, ScalarType, ScalarTypeKind, Type};

pub enum X86MCInstruction {}

pub struct X86MachineFeatures {
    mach_features: HashSet<X86Feature>,
    mode: X86Mode,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConditionCode {
    Above,
    AboveEqual,
    Below,
    BelowEqual,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    NotEqual,
    Overflow,
    NotOverflow,
    Sign,
    NotSign,
    Parity,
    NotParity,
}

impl ConditionCode {
    pub fn inverse(&self) -> ConditionCode {
        match self {
            ConditionCode::Above => ConditionCode::BelowEqual,
            ConditionCode::AboveEqual => ConditionCode::Below,
            ConditionCode::Below => ConditionCode::AboveEqual,
            ConditionCode::BelowEqual => ConditionCode::Above,
            ConditionCode::Equal => ConditionCode::NotEqual,
            ConditionCode::Greater => ConditionCode::LessEqual,
            ConditionCode::GreaterEqual => ConditionCode::Less,
            ConditionCode::Less => ConditionCode::GreaterEqual,
            ConditionCode::LessEqual => ConditionCode::Less,
            ConditionCode::NotEqual => ConditionCode::Equal,
            ConditionCode::Overflow => ConditionCode::NotOverflow,
            ConditionCode::NotOverflow => ConditionCode::Overflow,
            ConditionCode::Sign => ConditionCode::NotSign,
            ConditionCode::NotSign => ConditionCode::Sign,
            ConditionCode::Parity => ConditionCode::NotParity,
            ConditionCode::NotParity => ConditionCode::Parity,
        }
    }

    pub fn as_jmp(&self) -> X86CodegenOpcode {
        match self {
            ConditionCode::Above => X86CodegenOpcode::Jnbe,
            ConditionCode::AboveEqual => X86CodegenOpcode::Jnb,
            ConditionCode::Below => X86CodegenOpcode::Jb,
            ConditionCode::BelowEqual => X86CodegenOpcode::Jbe,
            ConditionCode::Equal => X86CodegenOpcode::Jz,
            ConditionCode::Greater => X86CodegenOpcode::Jnle,
            ConditionCode::GreaterEqual => X86CodegenOpcode::Jnl,
            ConditionCode::Less => X86CodegenOpcode::Jl,
            ConditionCode::LessEqual => X86CodegenOpcode::Jle,
            ConditionCode::NotEqual => X86CodegenOpcode::Jnz,
            ConditionCode::Overflow => X86CodegenOpcode::Jo,
            ConditionCode::NotOverflow => X86CodegenOpcode::Jno,
            ConditionCode::Sign => X86CodegenOpcode::Js,
            ConditionCode::NotSign => X86CodegenOpcode::Jns,
            ConditionCode::Parity => X86CodegenOpcode::Jp,
            ConditionCode::NotParity => X86CodegenOpcode::Jnp,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86ValLocation {
    Null,
    Register(X86Register),
    SpDisp(Option<X86RegisterClass>, i32),
}

impl ValLocation for X86ValLocation {
    fn addressible(&self) -> bool {
        todo!("addressible")
    }

    fn unassigned(n: usize) -> Self {
        todo!("unassigned")
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86Tag {
    CDecl,
    Stdcall,
    Fastcall,
    Thiscall,
    Vectorcall,
    SysV64,
    Win64,
    Pascal,
    RegisterBorland,
    RegisterWatcom,
}

fn tag_from_name(tag: &str) -> X86Tag {
    match tag {
        "cdecl" => X86Tag::CDecl,
        "stdcall" => X86Tag::Stdcall,
        "fastcall" => X86Tag::Fastcall,
        "thiscall" => X86Tag::Thiscall,
        "vectorcall" => X86Tag::Vectorcall,
        "SysV64" => X86Tag::SysV64,
        "Win64" => X86Tag::Win64,
        "pascal" => X86Tag::Pascal,
        "register-borland" => X86Tag::RegisterBorland,
        "register-watcom" => X86Tag::RegisterWatcom,
        tag => panic!("unknown tag \"{}\"", tag),
    }
}

pub enum StackCleanup {
    Caller,
    Callee,
}

pub enum ParameterRegisterAssignmentMode {
    Interspere, // Like Sys-V, allow arbitrary interspertion of parameter types
    Consume,    // Like Windows, assign parameters strictly by position regardles of class
}

impl X86Tag {
    pub fn stack_cleanup(&self, mode: X86Mode) -> StackCleanup {
        match self {
            X86Tag::CDecl => StackCleanup::Caller,
            X86Tag::Stdcall => StackCleanup::Callee,
            X86Tag::Fastcall => StackCleanup::Callee,
            X86Tag::Thiscall => StackCleanup::Callee,
            X86Tag::Vectorcall => {
                if mode == X86Mode::Long {
                    StackCleanup::Caller
                } else {
                    StackCleanup::Callee
                }
            }
            X86Tag::SysV64 => StackCleanup::Caller,
            X86Tag::Win64 => StackCleanup::Caller,
            X86Tag::Pascal => StackCleanup::Callee,
            X86Tag::RegisterBorland => StackCleanup::Callee,
            X86Tag::RegisterWatcom => StackCleanup::Callee,
        }
    }

    pub fn int_regs(&self, mode: X86Mode) -> &[X86Register] {
        match self {
            X86Tag::CDecl => &[],
            X86Tag::Stdcall => &[],
            X86Tag::Fastcall => &[X86Register::Ecx, X86Register::Edx],
            X86Tag::Thiscall => &[X86Register::Ecx],
            X86Tag::Vectorcall => {
                if mode == X86Mode::Long {
                    &[X86Register::Ecx, X86Register::Edx]
                } else {
                    &[
                        X86Register::Rcx,
                        X86Register::Rdx,
                        X86Register::R8,
                        X86Register::R9,
                    ]
                }
            }
            X86Tag::SysV64 => &[
                X86Register::Rdi,
                X86Register::Rsi,
                X86Register::Rdx,
                X86Register::Rcx,
                X86Register::R8,
                X86Register::R9,
            ],
            X86Tag::Win64 => &[
                X86Register::Rcx,
                X86Register::Rdx,
                X86Register::R8,
                X86Register::R9,
            ],
            X86Tag::Pascal => &[],
            X86Tag::RegisterBorland => &[X86Register::Eax, X86Register::Edx, X86Register::Ecx],
            X86Tag::RegisterWatcom => &[
                X86Register::Eax,
                X86Register::Edx,
                X86Register::Ebx,
                X86Register::Ecx,
            ],
        }
    }

    pub fn vector_regs(&self, mode: X86Mode) -> &[X86Register] {
        match self {
            X86Tag::CDecl => &[],
            X86Tag::Fastcall => &[],
            X86Tag::Stdcall => &[],
            X86Tag::Thiscall => &[],
            X86Tag::Vectorcall => &[
                X86Register::Xmm(0),
                X86Register::Xmm(1),
                X86Register::Xmm(2),
                X86Register::Xmm(3),
                X86Register::Xmm(4),
                X86Register::Xmm(5),
            ],
            X86Tag::SysV64 => &[
                X86Register::Xmm(0),
                X86Register::Xmm(1),
                X86Register::Xmm(2),
                X86Register::Xmm(3),
                X86Register::Xmm(4),
                X86Register::Xmm(5),
                X86Register::Xmm(6),
                X86Register::Xmm(7),
            ],
            X86Tag::Win64 => &[
                X86Register::Xmm(0),
                X86Register::Xmm(1),
                X86Register::Xmm(2),
                X86Register::Xmm(3),
            ],
            X86Tag::Pascal => &[],
            X86Tag::RegisterBorland => &[],
            X86Tag::RegisterWatcom => &[],
        }
    }

    pub fn saved_regs(&self) -> &[X86Register] {
        match self {
            X86Tag::CDecl => todo!(),
            X86Tag::Stdcall => todo!(),
            X86Tag::Fastcall => todo!(),
            X86Tag::Thiscall => todo!(),
            X86Tag::Vectorcall => todo!(),
            X86Tag::SysV64 => &[
                X86Register::Rbx,
                X86Register::Rbp,
                X86Register::R12,
                X86Register::R13,
                X86Register::R14,
                X86Register::R15,
            ],
            X86Tag::Win64 => todo!(),
            X86Tag::Pascal => todo!(),
            X86Tag::RegisterBorland => todo!(),
            X86Tag::RegisterWatcom => todo!(),
        }
    }
}

#[allow(dead_code)]
pub struct X86CallConv<'a> {
    mode: X86Mode,
    tag: X86Tag,
    properties: &'a TargetProperties<'a>,
    ty_info: Rc<TypeInformation>,
}

impl CallingConvention for X86CallConv<'_> {
    type Loc = X86ValLocation;

    fn pass_return_place(&self, ty: &xlang_struct::Type) -> Option<Self::Loc> {
        None // TODO: memory returns
    }

    #[allow(unused_mut, unused_variables)]
    fn find_param(
        &self,
        fnty: &xlang_struct::FnType,
        real: &xlang_struct::FnType,
        param: u32,
        infn: bool,
    ) -> Self::Loc {
        let mut intregs = self.tag.int_regs(self.mode);

        let mut vectorregs = self.tag.vector_regs(self.mode);

        if param > 0 {
            todo!()
        }

        let size = self.ty_info.type_size(&real.params[0]).unwrap();

        if size <= ((self.mode.width() >> 3) as u64) {
            X86ValLocation::Register(intregs[0])
        } else {
            todo!()
        }
    }

    fn find_return_val(&self, fnty: &xlang_struct::FnType) -> Self::Loc {
        let size = self.ty_info.type_size(&fnty.ret).unwrap_or(0);

        if size == 0 {
            X86ValLocation::Null
        } else if size <= ((self.mode.width() >> 3) as u64) {
            X86ValLocation::Register(
                X86Register::from_class(
                    self.mode.largest_gpr(),
                    0, // rAX
                )
                .unwrap(),
            ) // TODO: Floating-point
        } else {
            todo!()
        }
    }
}

impl MachineFeatures for X86MachineFeatures {
    type Loc = X86ValLocation;

    type CallConv = X86CallConv<'static>;

    fn native_int_size(&self) -> u16 {
        self.mode.width()
    }

    fn native_float_size(&self) -> Option<u16> {
        if self.mach_features.contains(&X86Feature::X87) {
            Some(80)
        } else if self.mach_features.contains(&X86Feature::Sse) {
            Some(64)
        } else {
            None
        }
    }

    fn lockfree_use_libatomic(&self, size: u64) -> bool {
        false
    }

    fn lockfree_cmpxchg_use_libatomic(&self, size: u64) -> bool {
        false
    }

    fn has_wait_free_compound(&self, op: xlang_struct::BinaryOp, size: u64) -> bool {
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Lsh
            | BinaryOp::Rsh => (size << 3) <= self.native_int_size() as u64,
            _ => false,
        }
    }

    fn has_wait_free_compound_fetch(&self, op: xlang_struct::BinaryOp, size: u64) -> bool {
        match op {
            BinaryOp::Add | BinaryOp::Sub => (size << 3) < self.native_int_size() as u64,
            _ => false,
        }
    }

    fn mangle(&self, path: &[xlang_struct::PathComponent]) -> String {
        xlang_backend::mangle::mangle_itanium(path) // TODO: Windows
    }
}

pub struct X86Clobbers {
    used_regs: HashMap<X86Register, u32>,
    clobbered_regs: HashMap<u32, Vec<(X86Register, X86ValLocation)>>,
    restore_locations: HashMap<u32, (u32, Option<X86ValLocation>)>,
    stack_width: u32,
    last_used_regs: VecDeque<X86Register>,
    mode: X86Mode,
    load_val: HashMap<u32, Vec<(X86Register, X86ValLocation)>>,
}

pub struct X86MCWriter {}

fn resolve_location(
    loc: &mut MaybeResolved<X86ValLocation>,
    assignments: &mut HashMap<u32, (Type, X86ValLocation)>,
    clobbers: &mut X86Clobbers,
) {
    match loc {
        MaybeResolved::Unresolved(inner) => {
            let id = inner.id();

            if let Some((ty, assign)) = assignments.get(&id).cloned() {
                let X86Clobbers {
                    restore_locations,
                    stack_width,
                    mode,
                    ..
                } = clobbers;
                if let Some((_, loc)) = restore_locations.get_mut(&id) {
                    loc.get_or_insert_with(|| {
                        let offset = (*stack_width) as i32;
                        let (cl, size) = match assign {
                            X86ValLocation::Register(reg) => {
                                (Some(reg.class()), reg.class().size(*mode) as u32)
                            }
                            X86ValLocation::SpDisp(cl, _) => (cl, cl.unwrap().size(*mode) as u32),
                            X86ValLocation::Null => (None, 0),
                        };
                        *stack_width += size;
                        X86ValLocation::SpDisp(cl, -offset)
                    });
                    assignments.remove(&id);
                } else {
                    *loc = MaybeResolved::Resolved(ty, assign);
                }
            }
        }
        MaybeResolved::Resolved(_, _) => {}
    }
}

fn resolve_locations_in(
    insn: &mut MCInsn<X86ValLocation>,
    assignments: &mut HashMap<u32, (Type, X86ValLocation)>,
    clobbers: &mut X86Clobbers,
) {
    match insn {
        MCInsn::Null => todo!(),
        MCInsn::Mov { dest, src } => {
            resolve_location(dest, assignments, clobbers);
            resolve_location(src, assignments, clobbers);
        }
        MCInsn::MovImm { dest, .. } => {
            resolve_location(dest, assignments, clobbers);
        }
        MCInsn::StoreIndirect { dest_ptr, src, .. } => {
            resolve_location(dest_ptr, assignments, clobbers);
            resolve_location(src, assignments, clobbers);
        }
        MCInsn::StoreIndirectImm { dest_ptr, .. } => {
            resolve_location(dest_ptr, assignments, clobbers);
        }
        MCInsn::BinaryOpImm { dest, src, .. } => {
            resolve_location(dest, assignments, clobbers);
            resolve_location(src, assignments, clobbers);
        }
        MCInsn::BinaryOp {
            dest, src1, src2, ..
        } => {
            resolve_location(dest, assignments, clobbers);
            resolve_location(src1, assignments, clobbers);
            resolve_location(src2, assignments, clobbers);
        }
        MCInsn::UnaryOp { dest, .. } => {
            resolve_location(dest, assignments, clobbers);
        }

        MCInsn::LoadSym { loc, .. } => {
            resolve_location(loc, assignments, clobbers);
        }
        MCInsn::LoadIndirect { dest, src_ptr, .. } => {
            resolve_location(dest, assignments, clobbers);
            resolve_location(src_ptr, assignments, clobbers);
        }
        MCInsn::ZeroExtend { dest, src, .. } => {
            resolve_location(dest, assignments, clobbers);
            resolve_location(src, assignments, clobbers);
        }
        _ => {}
    }
}

fn allocate_registers_for(
    dest: &mut MaybeResolved<X86ValLocation>,
    clobbers: &mut X86Clobbers,
    assignments: &mut HashMap<u32, (Type, X86ValLocation)>,
    num: u32,
) -> bool {
    let iregs = &[
        X86Register::Rax,
        X86Register::Rcx,
        X86Register::Rdx,
        X86Register::Rbx,
        X86Register::Rsi,
        X86Register::Rdi,
        X86Register::R8,
        X86Register::R9,
        X86Register::R10,
        X86Register::R11,
        X86Register::R12,
        X86Register::R13,
        X86Register::R14,
        X86Register::R15,
    ];
    match dest {
        MaybeResolved::Unresolved(loc) => {
            let ty = loc.type_of().clone();
            if loc.size_of() == 0 {
                let id = loc.id();
                let loc = X86ValLocation::Null;
                *dest = MaybeResolved::Resolved(ty.clone(), loc.clone());
                assignments.insert(id, (ty, loc));
                false
            } else if loc.size_of() <= 8 {
                for reg in iregs {
                    if clobbers.used_regs.contains_key(reg) {
                        continue;
                    } else {
                        let id = loc.id();
                        let reg = *reg;
                        let loc = X86ValLocation::Register(reg);
                        *dest = MaybeResolved::Resolved(ty.clone(), loc.clone());
                        assignments.insert(id, (ty, loc));
                        clobbers.last_used_regs.push_back(reg);

                        if let Some(Pair(_, (addr, Some(loc)))) =
                            clobbers.restore_locations.remove(&id)
                        {
                            clobbers
                                .load_val
                                .get_or_insert_with_mut(num, |_| Vec::new())
                                .push((reg, loc.clone()));

                            clobbers
                                .clobbered_regs
                                .get_or_insert_with_mut(addr, |_| Vec::new())
                                .push((reg, loc));
                        }
                        return false;
                    }
                }

                if let Some(reg) = clobbers.last_used_regs.pop_front() {
                    clobbers.last_used_regs.push_back(reg);
                    let save_id = clobbers.used_regs[&reg];

                    clobbers.restore_locations.insert(save_id, (num, None));

                    let id = loc.id();
                    if let Some(Pair(_, (addr, Some(loc)))) = clobbers.restore_locations.remove(&id)
                    {
                        clobbers
                            .load_val
                            .get_or_insert_with_mut(num, |_| Vec::new())
                            .push((reg, loc.clone()));

                        clobbers
                            .clobbered_regs
                            .get_or_insert_with_mut(addr, |_| Vec::new())
                            .push((reg, loc));
                    }
                    let loc = X86ValLocation::Register(reg);
                    *dest = MaybeResolved::Resolved(ty.clone(), loc.clone());
                    assignments.insert(id, (ty, loc));

                    false
                } else {
                    panic!("We don't have any registers?")
                }
            } else {
                todo!("memory")
            }
        }
        _ => true,
    }
}

fn allocate_registers_in(
    insn: &mut MCInsn<X86ValLocation>,
    clobbers: &mut X86Clobbers,
    assignments: &mut HashMap<u32, (Type, X86ValLocation)>,
    num: u32,
) -> bool {
    match insn {
        MCInsn::Null => true,
        MCInsn::Mov { dest, src } => match (&mut *dest, src) {
            (MaybeResolved::Unresolved(_), MaybeResolved::Unresolved(_)) => {
                allocate_registers_for(dest, clobbers, assignments, num);
                false
            }
            _ => true,
        },
        MCInsn::MovImm { dest, src } => allocate_registers_for(dest, clobbers, assignments, num),
        MCInsn::StoreIndirect { dest_ptr, src, cl } => true,
        MCInsn::Trap { kind } => true,
        MCInsn::Barrier(_) => true,
        MCInsn::BinaryOpImm { dest, src, val, op } => match src {
            src @ MaybeResolved::Unresolved(_) => {
                allocate_registers_for(src, clobbers, assignments, num);
                false
            }
            _ => true,
        },
        MCInsn::BinaryOp {
            dest,
            src1,
            src2,
            op,
        } => true,
        MCInsn::UnaryOp { dest, op } => true,
        MCInsn::CallSym(_, _) => true,
        MCInsn::Return => true,
        MCInsn::LoadSym { loc, sym } => allocate_registers_for(loc, clobbers, assignments, num),
        MCInsn::Label(_) => true,
        _ => true,
    }
}

fn mov_opcode(cl: X86RegisterClass, align: u64, is_int_vec_hint: bool) -> X86CodegenOpcode {
    match cl {
        X86RegisterClass::Byte
        | X86RegisterClass::ByteRex
        | X86RegisterClass::Word
        | X86RegisterClass::Double
        | X86RegisterClass::Quad
        | X86RegisterClass::Sreg => X86CodegenOpcode::Mov,
        X86RegisterClass::Mmx => todo!("mmx"),
        X86RegisterClass::Xmm => match (align >= 16, is_int_vec_hint) {
            (true, true) => todo!("movdqa"),
            (true, false) => todo!("movaps"),
            (false, true) => todo!("movdqu"),
            (false, false) => todo!("movups"),
        },
        X86RegisterClass::Ymm => match (align >= 32, is_int_vec_hint) {
            (true, true) => todo!("vmovdqa"),
            (true, false) => todo!("vmovaps"),
            (false, true) => todo!("vmovdqu"),
            (false, false) => todo!("vmovups"),
        },
        X86RegisterClass::Zmm => match (align >= 64, is_int_vec_hint) {
            (true, true) => todo!("vmovdqa"),
            (true, false) => todo!("vmovaps"),
            (false, true) => todo!("vmovdqu"),
            (false, false) => todo!("vmovups"),
        },

        X86RegisterClass::Cr | X86RegisterClass::Dr | X86RegisterClass::Tr => {
            panic!("Why are we codegenning to control/debug/trace registers?")
        }

        X86RegisterClass::AvxMask => todo!("movemask"),
        X86RegisterClass::Tmm => todo!("tmm handling"),
        X86RegisterClass::St => panic!("FP Stack needs to be handled specially"),
        _ => todo!(),
    }
}

impl MCWriter for X86MCWriter {
    type Features = X86MachineFeatures;

    type Clobbers = X86Clobbers;

    fn resolve_locations(
        &self,
        insns: &mut [xlang_backend::mc::MCInsn<
            <Self::Features as xlang_backend::mc::MachineFeatures>::Loc,
        >],
        callconv: &<Self::Features as xlang_backend::mc::MachineFeatures>::CallConv,
    ) -> Self::Clobbers {
        let mut curr_assignments = HashMap::new();
        let mut clobbers = X86Clobbers {
            used_regs: HashMap::new(),
            clobbered_regs: HashMap::new(),
            restore_locations: HashMap::new(),
            stack_width: 0,
            last_used_regs: VecDeque::new(),
            mode: callconv.mode,
            load_val: HashMap::new(),
        };
        let iregs = &[
            X86Register::Rax,
            X86Register::Rcx,
            X86Register::Rdx,
            X86Register::Rbx,
            X86Register::Rsi,
            X86Register::Rdi,
            X86Register::R8,
            X86Register::R9,
            X86Register::R10,
            X86Register::R11,
            X86Register::R12,
            X86Register::R13,
            X86Register::R14,
            X86Register::R15,
        ];

        let saved_regs = callconv.tag.saved_regs();

        for (reg, loc) in saved_regs.iter().zip(((!0 - saved_regs.len())..=!0).rev()) {
            clobbers.used_regs.insert(*reg, loc as u32);
        }

        let mut done = false;
        while !done {
            done = true;

            for (insn, addr) in insns.iter_mut().zip(0u32..) {
                match insn {
                    xlang_backend::mc::MCInsn::Mov { dest, src } => match (dest, src) {
                        (MaybeResolved::Resolved(_, reg), MaybeResolved::Unresolved(loc)) => {
                            match reg {
                                X86ValLocation::Register(reg) => {}
                                _ => {}
                            }
                            curr_assignments.insert(loc.id(), (loc.type_of().clone(), reg.clone()));
                            done = false;
                        }
                        (MaybeResolved::Unresolved(loc), MaybeResolved::Resolved(_, reg)) => {
                            match reg {
                                X86ValLocation::Register(reg) => {}
                                _ => {}
                            }
                            curr_assignments.insert(loc.id(), (loc.type_of().clone(), reg.clone()));
                            done = false;
                        }
                        (MaybeResolved::Unresolved(l), MaybeResolved::Unresolved(r)) => {
                            if let Some(val) = curr_assignments.get(&l.id()) {
                                let val = val.clone();
                                curr_assignments.insert(r.id(), val);
                                done = false;
                            } else if let Some(val) = curr_assignments.get(&r.id()) {
                                let val = val.clone();
                                curr_assignments.insert(r.id(), val);
                                done = false;
                            } else {
                            }
                        }
                        _ => {}
                    },
                    MCInsn::Return | MCInsn::TailcallSym(_) => {
                        for (reg, loc) in
                            saved_regs.iter().zip(((!0 - saved_regs.len())..=!0).rev())
                        {
                            let X86Clobbers {
                                restore_locations,
                                stack_width,
                                mode,
                                ..
                            } = &mut clobbers;
                            if let Some((_, loc)) = restore_locations.get_mut(&(loc as u32)) {
                                let assign = loc
                                    .get_or_insert_with(|| {
                                        let offset = (*stack_width) as i32;
                                        let (cl, size) =
                                            (reg.class(), reg.class().size(*mode) as u32);
                                        *stack_width += size;
                                        X86ValLocation::SpDisp(Some(cl), -offset)
                                    })
                                    .clone();
                                clobbers
                                    .load_val
                                    .get_or_insert_with_mut(addr, |_| Vec::new())
                                    .push((*reg, assign));
                            }
                        }
                    }
                    MCInsn::CallSym(tag, _) => {
                        let tag = tag_from_name(tag);
                        let save_regs = tag.saved_regs();
                        for Pair(reg, loc) in &clobbers.used_regs {
                            if !save_regs.contains(reg) {
                                clobbers.restore_locations.insert(*loc, (addr, None));
                            }
                        }
                    }
                    _ => {}
                }
            }

            for (insn, num) in insns.iter_mut().zip(0u32..) {
                resolve_locations_in(insn, &mut curr_assignments, &mut clobbers);
                done &= allocate_registers_in(insn, &mut clobbers, &mut curr_assignments, num);
            }
        }
        clobbers
    }

    fn write_machine_code<I: arch_ops::traits::InsnWrite, F: FnMut(String, u64)>(
        &self,
        insns: &[xlang_backend::mc::MCInsn<
            <Self::Features as xlang_backend::mc::MachineFeatures>::Loc,
        >],
        clobbers: Self::Clobbers,
        tys: Rc<TypeInformation>,
        out: &mut I,
        mut sym_accepter: F,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(out, X86Mode::Long);
        let mut frame_size = clobbers.stack_width;
        let mut last_cc = None;

        let sp = X86Register::from_class(clobbers.mode.largest_gpr(), 4).unwrap();
        if frame_size > 0 {
            encoder.write_insn(X86Instruction::new(
                X86CodegenOpcode::Sub,
                vec![
                    X86Operand::Register(sp),
                    X86Operand::Immediate(frame_size as i64),
                ],
            ))?;
        }

        for (insn, num) in insns.iter().zip(0u32..) {
            for (reg, loc) in clobbers.clobbered_regs.get(&num).into_iter().flatten() {
                match loc {
                    X86ValLocation::Null => {}
                    X86ValLocation::Register(reg2) => {
                        let mov =
                            mov_opcode(reg.class(), reg.class().size(clobbers.mode) as u64, false);
                        encoder.write_insn(X86Instruction::new(
                            mov,
                            vec![X86Operand::Register(*reg), X86Operand::Register(*reg2)],
                        ))?;
                    }
                    X86ValLocation::SpDisp(_, off) => {
                        let mov =
                            mov_opcode(reg.class(), (1 << (*off).trailing_zeros()).min(16), false);
                        // TODO: Windows Ix86 and pre-IA-32 have lower stack alignment
                        encoder.write_insn(X86Instruction::new(
                            mov,
                            vec![
                                X86Operand::Register(*reg),
                                X86Operand::Memory(
                                    reg.class(),
                                    None,
                                    X86MemoryOperand::Indirect {
                                        reg: sp,
                                        disp: Some(X86Displacement::Offset(*off)),
                                    },
                                ),
                            ],
                        ))?;
                    }
                }
            }
            for (reg, loc) in clobbers.load_val.get(&num).into_iter().flatten() {
                match loc {
                    X86ValLocation::Null => {}
                    X86ValLocation::Register(reg2) => {
                        let mov =
                            mov_opcode(reg.class(), reg.class().size(clobbers.mode) as u64, false);
                        encoder.write_insn(X86Instruction::new(
                            mov,
                            vec![X86Operand::Register(*reg2), X86Operand::Register(*reg)],
                        ))?;
                    }
                    X86ValLocation::SpDisp(_, off) => {
                        let mov =
                            mov_opcode(reg.class(), (1 << (*off).trailing_zeros()).min(16), false);
                        encoder.write_insn(X86Instruction::new(
                            mov,
                            vec![
                                X86Operand::Memory(
                                    reg.class(),
                                    None,
                                    X86MemoryOperand::Indirect {
                                        reg: sp,
                                        disp: Some(X86Displacement::Offset(*off)),
                                    },
                                ),
                                X86Operand::Register(*reg),
                            ],
                        ))?;
                    }
                }
            }
            match insn {
                MCInsn::Null => {}
                MCInsn::Mov { dest, src } => {
                    match (dest, src) {
                        (MaybeResolved::Resolved(_, dest), MaybeResolved::Resolved(_, src)) => {
                            if dest == src {
                                // no-op
                            } else {
                                match (dest, src) {
                                    (X86ValLocation::Null, _) | (_, X86ValLocation::Null) => {}
                                    (
                                        X86ValLocation::Register(destreg),
                                        X86ValLocation::Register(srcreg),
                                    ) if destreg.class() == srcreg.class() => match destreg.class()
                                    {
                                        arch_ops::x86::X86RegisterClass::Byte
                                        | arch_ops::x86::X86RegisterClass::ByteRex
                                        | arch_ops::x86::X86RegisterClass::Word
                                        | arch_ops::x86::X86RegisterClass::Double
                                        | arch_ops::x86::X86RegisterClass::Quad => {
                                            encoder.write_insn(X86Instruction::new(
                                                X86CodegenOpcode::Mov,
                                                vec![
                                                    X86Operand::Register(*destreg),
                                                    X86Operand::Register(*srcreg),
                                                ],
                                            ))?;
                                        }
                                        arch_ops::x86::X86RegisterClass::Mmx => todo!("mmx"),
                                        arch_ops::x86::X86RegisterClass::Xmm => todo!("xmm"),
                                        arch_ops::x86::X86RegisterClass::Ymm => todo!("ymm"),
                                        arch_ops::x86::X86RegisterClass::Zmm => todo!("zmm"),
                                        arch_ops::x86::X86RegisterClass::Tmm => todo!("tmm"),
                                        arch_ops::x86::X86RegisterClass::Sreg => todo!("sreg"),
                                        arch_ops::x86::X86RegisterClass::Cr => todo!("cr"),
                                        arch_ops::x86::X86RegisterClass::Dr => todo!("dr"),
                                        arch_ops::x86::X86RegisterClass::Tr => todo!("tr"),
                                        arch_ops::x86::X86RegisterClass::St => todo!("fp"),
                                        arch_ops::x86::X86RegisterClass::AvxMask => todo!("kreg"),
                                        _ => todo!(),
                                    },
                                    _ => todo!(),
                                }
                            }
                        }
                        _ => panic!("Unresolved locations made it to write_machine_code"),
                    }
                }
                MCInsn::MovImm { dest, src } => match dest {
                    MaybeResolved::Resolved(_, loc) => match loc {
                        X86ValLocation::Null => {}
                        X86ValLocation::Register(reg) => match reg.class() {
                            arch_ops::x86::X86RegisterClass::Byte
                            | arch_ops::x86::X86RegisterClass::ByteRex
                            | arch_ops::x86::X86RegisterClass::Word
                            | arch_ops::x86::X86RegisterClass::Double
                            | arch_ops::x86::X86RegisterClass::Quad => {
                                if *src == 0 {
                                    encoder.write_insn(X86Instruction::new(
                                        X86CodegenOpcode::Xor,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Register(*reg),
                                        ],
                                    ))?;
                                } else {
                                    encoder.write_insn(X86Instruction::new(
                                        X86CodegenOpcode::Mov,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Immediate(*src as i64),
                                        ],
                                    ))?;
                                }
                            }
                            arch_ops::x86::X86RegisterClass::Mmx => todo!(),
                            arch_ops::x86::X86RegisterClass::Xmm => todo!(),
                            arch_ops::x86::X86RegisterClass::Ymm => todo!(),
                            arch_ops::x86::X86RegisterClass::Zmm => todo!(),
                            arch_ops::x86::X86RegisterClass::Tmm => todo!(),
                            arch_ops::x86::X86RegisterClass::Sreg => todo!(),
                            arch_ops::x86::X86RegisterClass::Cr => todo!(),
                            arch_ops::x86::X86RegisterClass::Dr => todo!(),
                            arch_ops::x86::X86RegisterClass::Tr => todo!(),
                            arch_ops::x86::X86RegisterClass::St => todo!(),
                            arch_ops::x86::X86RegisterClass::AvxMask => todo!(),
                            _ => todo!(),
                        },
                        X86ValLocation::SpDisp(_, _) => todo!("spdisp"),
                    },
                    _ => panic!("UNresolved location"),
                },
                MCInsn::StoreIndirect { dest_ptr, src, .. } => match (dest_ptr, src) {
                    (MaybeResolved::Resolved(_, ptr), MaybeResolved::Resolved(ty, loc)) => {
                        match (ptr, loc) {
                            (X86ValLocation::Null, _) => panic!("Pointer in nowhere"),
                            (_, X86ValLocation::Null) => {}
                            (X86ValLocation::Register(ptr), X86ValLocation::Register(reg)) => {
                                let align = tys.type_align(ty).unwrap();

                                let int_vec_hint = matches!(
                                    ty,
                                    Type::Scalar(ScalarType {
                                        kind: ScalarTypeKind::Integer { .. }
                                            | ScalarTypeKind::Char { .. }
                                            | ScalarTypeKind::Fixed { .. },
                                        ..
                                    })
                                );

                                let class = reg.class();

                                let instr = mov_opcode(class, align, int_vec_hint);

                                encoder.write_insn(X86Instruction::new(
                                    instr,
                                    vec![
                                        X86Operand::Memory(
                                            class,
                                            None,
                                            X86MemoryOperand::Indirect {
                                                reg: *ptr,
                                                disp: None,
                                            },
                                        ),
                                        X86Operand::Register(*reg),
                                    ],
                                ))?;
                            }
                            (X86ValLocation::Register(_), X86ValLocation::SpDisp(_, _)) => todo!(),
                            (X86ValLocation::SpDisp(_, _), X86ValLocation::Register(_)) => todo!(),
                            (X86ValLocation::SpDisp(_, _), X86ValLocation::SpDisp(_, _)) => todo!(),
                        }
                    }
                    _ => panic!("Unresolved Location"),
                },
                MCInsn::LoadIndirect { dest, src_ptr, .. } => match (src_ptr, dest) {
                    (MaybeResolved::Resolved(ptr_ty, ptr), MaybeResolved::Resolved(ty, loc)) => {
                        match (ptr, loc) {
                            (X86ValLocation::Null, _) => panic!("Pointer in nowhere"),
                            (_, X86ValLocation::Null) => {}
                            (X86ValLocation::Register(ptr), X86ValLocation::Register(reg)) => {
                                eprintln!("{}: {}", ptr_ty, ty);
                                let align = tys.type_align(ty).unwrap();

                                let int_vec_hint = matches!(
                                    ty,
                                    Type::Scalar(ScalarType {
                                        kind: ScalarTypeKind::Integer { .. }
                                            | ScalarTypeKind::Char { .. }
                                            | ScalarTypeKind::Fixed { .. },
                                        ..
                                    })
                                );

                                let class = reg.class();

                                let instr = mov_opcode(class, align, int_vec_hint);

                                encoder.write_insn(X86Instruction::new(
                                    instr,
                                    vec![
                                        X86Operand::Register(*reg),
                                        X86Operand::Memory(
                                            class,
                                            None,
                                            X86MemoryOperand::Indirect {
                                                reg: *ptr,
                                                disp: None,
                                            },
                                        ),
                                    ],
                                ))?;
                            }
                            (X86ValLocation::Register(_), X86ValLocation::SpDisp(_, _)) => todo!(),
                            (X86ValLocation::SpDisp(_, _), X86ValLocation::Register(_)) => todo!(),
                            (X86ValLocation::SpDisp(_, _), X86ValLocation::SpDisp(_, _)) => todo!(),
                        }
                    }
                    _ => panic!("Unresolved Location"),
                },
                MCInsn::StoreIndirectImm { dest_ptr, src, .. } => match dest_ptr {
                    MaybeResolved::Resolved(ty, loc) => match loc {
                        X86ValLocation::Register(reg) => {
                            let inner_ty = match ty {
                                Type::Pointer(pty) => &pty.inner,
                                _ => panic!("Store indirect to not a pointer"),
                            };
                            let size = tys.type_size(inner_ty).unwrap();
                            eprintln!("{} ({})", ty, size);

                            let size = match size {
                                1 => X86RegisterClass::Byte,
                                2 => X86RegisterClass::Word,
                                4 => X86RegisterClass::Double,
                                8 => X86RegisterClass::Quad,
                                _ => todo!(),
                            };

                            encoder.write_insn(X86Instruction::new(
                                X86CodegenOpcode::Mov,
                                vec![
                                    X86Operand::Memory(
                                        size,
                                        None,
                                        X86MemoryOperand::Indirect {
                                            reg: *reg,
                                            disp: None,
                                        },
                                    ),
                                    X86Operand::Immediate(*src as i64),
                                ],
                            ))?;
                        }
                        loc => todo!("store_indirect({:?})", loc),
                    },
                    _ => panic!("unresolved location"),
                },
                MCInsn::Trap { kind } => match kind {
                    xlang_backend::expr::Trap::Breakpoint => {
                        encoder.write_insn(X86Instruction::Int3)?
                    }
                    _ => encoder.write_insn(X86Instruction::Ud2)?,
                },
                MCInsn::Barrier(_) => todo!(),
                MCInsn::BinaryOpImm { dest, src, val, op } => match *op {
                    BinaryOp::CmpEq
                    | BinaryOp::CmpNe
                    | BinaryOp::CmpLt
                    | BinaryOp::CmpGt
                    | BinaryOp::CmpLe
                    | BinaryOp::CmpGe
                    | BinaryOp::CmpInt
                    | BinaryOp::Cmp => {
                        let mut cc = None;
                        match src {
                            MaybeResolved::Unresolved(_) => panic!("Unresolved location"),
                            MaybeResolved::Resolved(ty, loc) => {
                                match (*op, ty) {
                                    (BinaryOp::CmpEq, _) => cc = Some(ConditionCode::Equal),
                                    (BinaryOp::CmpNe, _) => cc = Some(ConditionCode::NotEqual),
                                    (
                                        BinaryOp::CmpLt,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: true, .. },
                                            ..
                                        }),
                                    ) => cc = Some(ConditionCode::Less),
                                    (
                                        BinaryOp::CmpLt,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Char { flags, .. },
                                            ..
                                        }),
                                    ) if flags.contains(CharFlags::SIGNED) => {
                                        cc = Some(ConditionCode::Less)
                                    }
                                    (BinaryOp::CmpLt, _) => cc = Some(ConditionCode::Below),
                                    (
                                        BinaryOp::CmpGt,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: true, .. },
                                            ..
                                        }),
                                    ) => cc = Some(ConditionCode::Greater),
                                    (
                                        BinaryOp::CmpGt,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Char { flags, .. },
                                            ..
                                        }),
                                    ) if flags.contains(CharFlags::SIGNED) => {
                                        cc = Some(ConditionCode::Greater)
                                    }
                                    (BinaryOp::CmpGt, _) => cc = Some(ConditionCode::Above),
                                    (
                                        BinaryOp::CmpLe,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: true, .. },
                                            ..
                                        }),
                                    ) => cc = Some(ConditionCode::LessEqual),
                                    (
                                        BinaryOp::CmpLe,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Char { flags, .. },
                                            ..
                                        }),
                                    ) if flags.contains(CharFlags::SIGNED) => {
                                        cc = Some(ConditionCode::LessEqual)
                                    }
                                    (BinaryOp::CmpLe, _) => cc = Some(ConditionCode::BelowEqual),
                                    (
                                        BinaryOp::CmpGe,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Integer { signed: true, .. },
                                            ..
                                        }),
                                    ) => cc = Some(ConditionCode::GreaterEqual),
                                    (
                                        BinaryOp::CmpGe,
                                        Type::Scalar(ScalarType {
                                            kind: ScalarTypeKind::Char { flags, .. },
                                            ..
                                        }),
                                    ) if flags.contains(CharFlags::SIGNED) => {
                                        cc = Some(ConditionCode::GreaterEqual)
                                    }
                                    (BinaryOp::CmpGe, _) => cc = Some(ConditionCode::Less),
                                    _ => {}
                                }
                                match loc {
                                    X86ValLocation::Null => todo!("null"),
                                    X86ValLocation::Register(reg)
                                        if matches!(
                                            reg.class(),
                                            X86RegisterClass::Byte
                                                | X86RegisterClass::ByteRex
                                                | X86RegisterClass::Word
                                                | X86RegisterClass::Double
                                                | X86RegisterClass::Quad
                                        ) =>
                                    {
                                        encoder.write_insn(X86Instruction::new(
                                            X86CodegenOpcode::Cmp,
                                            vec![
                                                X86Operand::Register(*reg),
                                                X86Operand::Immediate(*val as i64),
                                            ],
                                        ))?;
                                    }
                                    X86ValLocation::Register(reg) => todo!("{:?}", reg),
                                    X86ValLocation::SpDisp(_, _) => todo!(),
                                }
                            }
                        }
                        match dest {
                            MaybeResolved::Unresolved(_) => {
                                last_cc = cc;
                            }
                            MaybeResolved::Resolved(ty_, loc) => todo!("{:?}", loc),
                        }
                    }
                    BinaryOp::BitAnd => match (dest, src) {
                        (MaybeResolved::Resolved(_, dest), MaybeResolved::Resolved(_, src)) => {
                            match (dest, src) {
                                (X86ValLocation::Null, _) | (_, X86ValLocation::Null) => {}
                                (X86ValLocation::Register(dest), X86ValLocation::Register(src)) => {
                                    let class = dest.class();
                                    // TODO: Make use of APX NDD
                                    if dest != src {
                                        let mov = mov_opcode(
                                            class,
                                            class.size(clobbers.mode) as u64,
                                            true,
                                        );
                                        encoder.write_insn(X86Instruction::new(
                                            mov,
                                            vec![
                                                X86Operand::Register(*dest),
                                                X86Operand::Register(*src),
                                            ],
                                        ))?;
                                    }

                                    match dest.class() {
                                        X86RegisterClass::Byte
                                        | X86RegisterClass::ByteRex
                                        | X86RegisterClass::Word
                                        | X86RegisterClass::Double
                                        | X86RegisterClass::Quad => {
                                            encoder.write_insn(X86Instruction::new(
                                                X86CodegenOpcode::And,
                                                vec![
                                                    X86Operand::Register(*dest),
                                                    X86Operand::Immediate(*val as i64),
                                                ],
                                            ))?;
                                        }
                                        cl => todo!("{:?}", cl),
                                    }
                                }
                                (X86ValLocation::Register(_), X86ValLocation::SpDisp(_, _)) => {
                                    todo!()
                                }
                                (X86ValLocation::SpDisp(_, _), X86ValLocation::Register(_)) => {
                                    todo!()
                                }
                                (X86ValLocation::SpDisp(_, _), X86ValLocation::SpDisp(_, _)) => {
                                    todo!()
                                }
                            }
                        }
                        _ => panic!("Unresolved location"),
                    },
                    op => todo!("{:?}", op),
                },
                MCInsn::BinaryOp {
                    dest,
                    src1,
                    src2,
                    op,
                } => todo!(),
                MCInsn::UnaryOp { dest, op } => todo!(),
                MCInsn::CallSym(_, sym) => {
                    if (frame_size & 0xF) != 8 {
                        let disp = 16 - ((frame_size + 8) & 0xF);

                        frame_size += disp;

                        encoder.write_insn(X86Instruction::new(
                            X86CodegenOpcode::Sub,
                            vec![
                                X86Operand::Register(X86Register::Rsp),
                                X86Operand::Immediate(disp as i64),
                            ],
                        ))?;
                    }

                    encoder.write_insn(X86Instruction::new(
                        X86CodegenOpcode::Call,
                        vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                    ))?;
                }
                MCInsn::TailcallSym(sym) => {
                    if frame_size > 0 {
                        encoder.write_insn(X86Instruction::new(
                            X86CodegenOpcode::Add,
                            vec![
                                X86Operand::Register(X86Register::Rsp),
                                X86Operand::Immediate(frame_size as i64),
                            ],
                        ))?;
                    }
                    encoder.write_insn(X86Instruction::new(
                        X86CodegenOpcode::Jmp,
                        vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                    ))?;
                }
                MCInsn::Return => {
                    if frame_size > 0 {
                        encoder.write_insn(X86Instruction::new(
                            X86CodegenOpcode::Add,
                            vec![
                                X86Operand::Register(X86Register::Rsp),
                                X86Operand::Immediate(frame_size as i64),
                            ],
                        ))?;
                    }
                    encoder.write_insn(X86Instruction::Ret)?;
                }
                MCInsn::LoadSym { loc, sym } => match loc {
                    MaybeResolved::Resolved(_, loc) => match loc {
                        X86ValLocation::Register(reg) => match reg.class() {
                            X86RegisterClass::Word
                            | X86RegisterClass::Double
                            | X86RegisterClass::Quad => encoder.write_insn(X86Instruction::new(
                                X86CodegenOpcode::Lea,
                                vec![
                                    X86Operand::Register(*reg),
                                    X86Operand::Memory(
                                        reg.class(),
                                        None,
                                        X86MemoryOperand::RelAddr(Address::PltSym {
                                            name: sym.to_string(),
                                        }),
                                    ),
                                ],
                            ))?,
                            _ => todo!(),
                        },
                        _ => todo!(),
                    },
                    _ => panic!("unresolved location"),
                },
                MCInsn::Label(label) => {
                    sym_accepter(label.clone(), encoder.offset() as u64);
                }
                MCInsn::UnconditionalBranch(label) => encoder.write_insn(X86Instruction::new(
                    X86CodegenOpcode::Jmp,
                    vec![X86Operand::RelOffset(Address::Symbol {
                        name: label.clone(),
                        disp: 0,
                    })],
                ))?,
                MCInsn::ZeroExtend {
                    dest,
                    src,
                    new_width,
                    old_width,
                } => {
                    if *new_width == 32 && old_width > new_width {
                        match (dest, src) {
                            (
                                MaybeResolved::Resolved(_, dest_reg),
                                MaybeResolved::Resolved(_, src_reg),
                            ) => match (dest_reg, src_reg) {
                                (
                                    X86ValLocation::Register(dest_reg),
                                    X86ValLocation::Register(src_reg),
                                ) => {
                                    let dest_reg = X86Register::from_class(
                                        X86RegisterClass::Double,
                                        dest_reg.regnum(),
                                    )
                                    .unwrap();
                                    let src_reg = X86Register::from_class(
                                        X86RegisterClass::Double,
                                        src_reg.regnum(),
                                    )
                                    .unwrap();
                                    encoder.write_insn(X86Instruction::new(
                                        X86CodegenOpcode::Mov,
                                        vec![
                                            X86Operand::Register(dest_reg),
                                            X86Operand::Register(src_reg),
                                        ],
                                    ))?;
                                }
                                _ => panic!(),
                            },
                            _ => panic!("unresolved location"),
                        }
                    } else {
                        todo!()
                    }
                }
                MCInsn::Branch { targ, cond, val } => {
                    let addr = X86Operand::RelOffset(Address::Symbol {
                        name: targ.clone(),
                        disp: -4,
                    });
                    match val {
                        MaybeResolved::Unresolved(_) => {
                            if let Some(cc) = last_cc {
                                match *cond {
                                    BranchCondition::NotEqual => encoder
                                        .write_insn(X86Instruction::new(cc.as_jmp(), vec![addr]))?,
                                    BranchCondition::Equal => encoder.write_insn(
                                        X86Instruction::new(cc.inverse().as_jmp(), vec![addr]),
                                    )?,
                                    cond => todo!("{:?}", cond),
                                }
                            } else {
                                todo!("cmp")
                            }
                        }
                        MaybeResolved::Resolved(_, loc) => todo!("{:?}", loc),
                    }
                }
                _ => todo!(),
            }
        }

        Ok(())
    }

    fn get_call_conv(
        &self,
        realty: &xlang_struct::FnType,
        targ: &'static xlang::targets::properties::TargetProperties<'static>,
        features: xlang::abi::span::Span<xlang::abi::string::StringView>,
        tys: Rc<TypeInformation>,
    ) -> <Self::Features as xlang_backend::mc::MachineFeatures>::CallConv {
        X86CallConv {
            properties: targ,
            mode: match targ.arch.width {
                16 => X86Mode::Real,
                32 => X86Mode::Protected,
                64 => X86Mode::Long,
                width => panic!("not a valid x86 width {}", width),
            },
            tag: { tag_from_name(&targ.default_tag_name) },
            ty_info: tys,
        }
    }

    fn get_features(
        &self,
        properties: &'static xlang::targets::properties::TargetProperties<'static>,
        features: xlang::abi::span::Span<xlang::abi::string::StringView>,
    ) -> Self::Features {
        X86MachineFeatures {
            mach_features: features
                .iter()
                .map(Deref::deref)
                .map(X86Feature::from_str)
                .collect::<Result<_, _>>()
                .unwrap(),
            mode: match properties.arch.width {
                16 => X86Mode::Real,
                32 => X86Mode::Protected,
                64 => X86Mode::Long,
                width => panic!("not a valid x86 width {}", width),
            },
        }
    }

    fn target_matches(&self, name: &str) -> bool {
        let arch_name = name
            .split_once("-")
            .map(|(arch, rest)| arch)
            .unwrap_or(name);

        let arch = Architecture::parse(arch_name);

        matches!(
            arch,
            Architecture::I86
                | Architecture::I8086
                | Architecture::I086
                | Architecture::I186
                | Architecture::I286
                | Architecture::I386
                | Architecture::I486
                | Architecture::I586
                | Architecture::I686
                | Architecture::I786
                | Architecture::X86_64
        )
    }
}

pub fn new_writer() -> X86MCWriter {
    X86MCWriter {}
}
