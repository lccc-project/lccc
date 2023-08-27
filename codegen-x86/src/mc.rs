#![allow(unused_variables)]
use std::{ops::Deref, rc::Rc, str::FromStr};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        features::X86Feature,
        insn::{X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register, X86RegisterClass,
    },
};
use target_tuples::Architecture;
use xlang::{
    abi::collection::{HashMap, HashSet},
    targets::properties::TargetProperties,
};
use xlang_backend::{
    callconv::CallingConvention,
    expr::ValLocation,
    mc::{MCInsn, MCWriter, MachineFeatures, MaybeResolved},
    ty::TypeInformation,
};
use xlang_struct::{BinaryOp, Type};

pub enum X86MCInstruction {}

pub struct X86MachineFeatures {
    mach_features: HashSet<X86Feature>,
    mode: X86Mode,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86ValLocation {
    Null,
    Register(X86Register),
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
                X86Register::Rsp,
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
    used_regs: HashSet<X86Register>,
}

pub struct X86MCWriter {}

fn resolve_location(
    loc: &mut MaybeResolved<X86ValLocation>,
    assignments: &HashMap<u32, X86ValLocation>,
) {
    match loc {
        MaybeResolved::Unresolved(inner) => {
            if let Some(assign) = assignments.get(&inner.id()) {
                *loc = MaybeResolved::Resolved(Type::Void, assign.clone())
            }
        }
        MaybeResolved::Resolved(_, _) => {}
    }
}

fn resolve_locations_in(
    insn: &mut MCInsn<X86ValLocation>,
    assignments: &HashMap<u32, X86ValLocation>,
) {
    match insn {
        MCInsn::Null => todo!(),
        MCInsn::Mov { dest, src } => {
            resolve_location(dest, assignments);
            resolve_location(src, assignments);
        }
        MCInsn::MovImm { dest, .. } => {
            resolve_location(dest, assignments);
        }
        MCInsn::StoreIndirect { dest_ptr, src, .. } => {
            resolve_location(dest_ptr, assignments);
            resolve_location(src, assignments);
        }

        MCInsn::BinaryOpImm { dest, .. } => {
            resolve_location(dest, assignments);
        }
        MCInsn::BinaryOp { dest, src, .. } => {
            resolve_location(dest, assignments);
            resolve_location(src, assignments);
        }
        MCInsn::UnaryOp { dest, .. } => {
            resolve_location(dest, assignments);
        }

        MCInsn::LoadSym { loc, .. } => {
            resolve_location(loc, assignments);
        }
        _ => {}
    }
}

fn allocate_registers_for(
    dest: &mut MaybeResolved<X86ValLocation>,
    clobbers: &mut X86Clobbers,
    assignments: &mut HashMap<u32, X86ValLocation>,
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
            if loc.size() == 0 {
                let id = loc.id();
                let loc = X86ValLocation::Null;
                *dest = MaybeResolved::Resolved(Type::Null, loc.clone());
                assignments.insert(id, loc);
                false
            } else if loc.size() <= 8 {
                for reg in iregs {
                    if clobbers.used_regs.contains(reg) {
                        continue;
                    } else {
                        let id = loc.id();
                        let loc = X86ValLocation::Register(*reg);
                        *dest = MaybeResolved::Resolved(Type::Null, loc.clone());
                        assignments.insert(id, loc);
                        return false;
                    }
                }
                todo!("no available registers")
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
    assignments: &mut HashMap<u32, X86ValLocation>,
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
        MCInsn::BinaryOpImm { dest, val, op } => true,
        MCInsn::BinaryOp { dest, src, op } => true,
        MCInsn::UnaryOp { dest, op } => true,
        MCInsn::CallSym(_) => true,
        MCInsn::Return => true,
        MCInsn::LoadSym { loc, sym } => allocate_registers_for(loc, clobbers, assignments, num),
        MCInsn::Label(_) => true,
        _ => true,
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
            used_regs: HashSet::new(),
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

        let saved_reg = callconv.tag.saved_regs();

        for reg in saved_reg {
            clobbers.used_regs.insert(*reg).ok();
        }

        let mut outer_done = false;

        while !outer_done {
            outer_done = true;

            let mut done = false;
            while !done {
                done = true;

                for (insn, addr) in insns.iter_mut().zip(0u32..) {
                    match insn {
                        xlang_backend::mc::MCInsn::Mov { dest, src } => match (dest, src) {
                            (MaybeResolved::Resolved(_, reg), MaybeResolved::Unresolved(loc)) => {
                                match reg {
                                    X86ValLocation::Register(reg) => {
                                        let _ = clobbers.used_regs.insert(*reg);
                                    }
                                    _ => {}
                                }
                                curr_assignments.insert(loc.id(), reg.clone());
                                done = false;
                            }
                            (MaybeResolved::Unresolved(loc), MaybeResolved::Resolved(_, reg)) => {
                                match reg {
                                    X86ValLocation::Register(reg) => {
                                        let _ = clobbers.used_regs.insert(*reg);
                                    }
                                    _ => {}
                                }
                                curr_assignments.insert(loc.id(), reg.clone());
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
                        _ => {}
                    }
                }

                for insn in insns.iter_mut() {
                    resolve_locations_in(insn, &curr_assignments);
                }
            }

            for (insn, num) in insns.iter_mut().zip(0u32..) {
                outer_done &=
                    allocate_registers_in(insn, &mut clobbers, &mut curr_assignments, num);
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
        out: &mut I,
        mut sym_accepter: F,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(out, X86Mode::Long);
        let mut frame_size = 0u32;
        for insn in insns.iter() {
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
                                        | arch_ops::x86::X86RegisterClass::ByteRex => {
                                            encoder.write_insn(X86Instruction::new(
                                                X86Opcode::MovMR8,
                                                vec![
                                                    X86Operand::Register(*destreg),
                                                    X86Operand::Register(*srcreg),
                                                ],
                                            ))?;
                                        }
                                        arch_ops::x86::X86RegisterClass::Word
                                        | arch_ops::x86::X86RegisterClass::Double
                                        | arch_ops::x86::X86RegisterClass::Quad => {
                                            encoder.write_insn(X86Instruction::new(
                                                X86Opcode::MovMR,
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
                            | arch_ops::x86::X86RegisterClass::ByteRex => {
                                if *src == 0 {
                                    encoder.write_insn(X86Instruction::new(
                                        X86Opcode::XorMR8,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Register(*reg),
                                        ],
                                    ))?;
                                } else {
                                    encoder.write_insn(X86Instruction::new(
                                        X86Opcode::MovImm8,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Immediate(*src as u64),
                                        ],
                                    ))?;
                                }
                            }
                            arch_ops::x86::X86RegisterClass::Word
                            | arch_ops::x86::X86RegisterClass::Double
                            | arch_ops::x86::X86RegisterClass::Quad => {
                                if *src == 0 {
                                    encoder.write_insn(X86Instruction::new(
                                        X86Opcode::XorMR,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Register(*reg),
                                        ],
                                    ))?;
                                } else {
                                    encoder.write_insn(X86Instruction::new(
                                        X86Opcode::MovImm,
                                        vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Immediate(*src as u64),
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
                    },
                    _ => panic!("UNresolved location"),
                },
                MCInsn::StoreIndirect { dest_ptr, src, cl } => todo!(),
                MCInsn::Trap { kind } => match kind {
                    xlang_backend::expr::Trap::Breakpoint => {
                        encoder.write_insn(X86Instruction::Int3)?
                    }
                    _ => encoder.write_insn(X86Instruction::Ud2)?,
                },
                MCInsn::Barrier(_) => todo!(),
                MCInsn::BinaryOpImm { dest, val, op } => todo!(),
                MCInsn::BinaryOp { dest, src, op } => todo!(),
                MCInsn::UnaryOp { dest, op } => todo!(),
                MCInsn::CallSym(sym) => {
                    if (frame_size & 0xF) != 8 {
                        let disp = 16 - ((frame_size + 8) & 0xF);

                        frame_size += disp;

                        encoder.write_insn(X86Instruction::new(
                            X86Opcode::SubImm,
                            vec![
                                X86Operand::Register(X86Register::Rsp),
                                X86Operand::Immediate(disp as u64),
                            ],
                        ))?;
                    }

                    encoder.write_insn(X86Instruction::new(
                        X86Opcode::Call,
                        vec![X86Operand::RelAddr(Address::PltSym { name: sym.clone() })],
                    ))?;
                }
                MCInsn::Return => {
                    if frame_size > 0 {
                        encoder.write_insn(X86Instruction::new(
                            X86Opcode::AddImm,
                            vec![
                                X86Operand::Register(X86Register::Rsp),
                                X86Operand::Immediate(frame_size as u64),
                            ],
                        ))?;
                    }
                    encoder.write_insn(X86Instruction::Retn)?;
                }
                MCInsn::LoadSym { loc, sym } => match loc {
                    MaybeResolved::Resolved(_, loc) => match loc {
                        X86ValLocation::Register(reg) => match reg.class() {
                            X86RegisterClass::Word
                            | X86RegisterClass::Double
                            | X86RegisterClass::Quad => encoder.write_insn(X86Instruction::new(
                                X86Opcode::Lea,
                                vec![
                                    X86Operand::Register(*reg),
                                    X86Operand::RelAddr(Address::PltSym { name: sym.clone() }),
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
                    X86Opcode::Jmp,
                    vec![X86Operand::RelAddr(Address::Symbol {
                        name: label.clone(),
                        disp: 0,
                    })],
                ))?,
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
