use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        codegen::{X86CodegenOpcode, X86Encoder, X86Instruction, X86MemoryOperand, X86Operand},
        X86Mode, X86Register, X86RegisterClass,
    },
};
use callconv::X86CallConvInfo;
use std::io::Write;
use std::vec as std_vec;
use target_tuples::{Architecture, Target};
use xlang::{
    abi::{pair::Pair, string::StringView},
    ir,
    plugin::XLangCodegen,
    prelude::v1::{vec, DynBox, HashMap, Vec},
    targets::properties::TargetProperties,
};
use xlang_backend::{
    callconv::{compute_call_conv, CallConvLocation},
    expr::Trap,
    mach::{
        mce::{MceInstruction, MceWriter},
        Machine,
    },
    ssa::{OpaquePtr, SsaInstruction},
    ty::TypeInformation,
    SsaCodegenPlugin,
};

mod callconv;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86ValLocation {
    Null,
    Register(X86Register),
    StackDisp(i32),
}

#[derive(Clone, Debug)]
pub struct LocationAssignment {
    foreign_location: X86ValLocation,
    owning_bb: u32,
    change_owner: Vec<(usize, X86ValLocation)>,
}

pub struct X86Machine {
    mode: Option<X86Mode>,
}

pub struct X86Assignments {
    mode: X86Mode,
    sp: X86Register,
    available_int_registers: Vec<X86Register>,
    return_reg: Option<X86Register>,
    stack_width: i32,
    stack_align: u64,
    assigns: HashMap<u32, LocationAssignment>,
}

pub struct X86Clobbers {
    reg_owners: HashMap<X86Register, u32>,
    clobbered_at: HashMap<u32, usize>,
}

impl X86Clobbers {
    pub fn mark_used(
        &mut self,
        val: u32,
        assign: &mut LocationAssignment,
        stack_width: &mut i32,
        ty_size: u64,
        ty_align: u64,
    ) {
        if let Some(Pair(_, pos)) = self.clobbered_at.remove(&val) {
            let align_minus1 = i32::try_from(ty_align).unwrap() - 1;
            *stack_width = (*stack_width + align_minus1) & align_minus1;
            let offset = -*stack_width;
            *stack_width += i32::try_from(ty_size).unwrap();
            assign
                .change_owner
                .push((pos, X86ValLocation::StackDisp(offset)));
        }
    }
    pub fn mark_clobbered(&mut self, reg: X86Register, which: usize, for_val: Option<u32>) {
        if let Some(Pair(_, val)) = self.reg_owners.remove(&reg) {
            if for_val == Some(val) {
                self.reg_owners.insert(reg, val);
            } else {
                self.clobbered_at.insert(val, which);
            }
        }
    }
}

fn xor_opcode(class: X86RegisterClass, hint_vec_is_int: bool) -> X86CodegenOpcode {
    match class {
        X86RegisterClass::Xmm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Pxor
            } else {
                X86CodegenOpcode::Xorps
            }
        }
        X86RegisterClass::Ymm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vpxor
            } else {
                X86CodegenOpcode::Vxorps
            }
        }
        X86RegisterClass::Zmm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vpxord
            } else {
                X86CodegenOpcode::Vxorps
            }
        }
        X86RegisterClass::Byte
        | X86RegisterClass::ByteRex
        | X86RegisterClass::Word
        | X86RegisterClass::Double
        | X86RegisterClass::Quad => X86CodegenOpcode::Xor,
        X86RegisterClass::AvxMask => todo!("kreg"),
        cl => panic!("Cannot xor {:?}", cl),
    }
}

fn move_opcode(
    class: X86RegisterClass,
    align: Option<u64>,
    hint_vec_is_int: bool,
) -> X86CodegenOpcode {
    match class {
        X86RegisterClass::Xmm if align.unwrap_or(16) >= 16 => {
            if hint_vec_is_int {
                X86CodegenOpcode::Movdqa
            } else {
                X86CodegenOpcode::Movaps
            }
        }
        X86RegisterClass::Ymm if align.unwrap_or(32) >= 32 => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vmovdqa
            } else {
                X86CodegenOpcode::Vmovaps
            }
        }
        X86RegisterClass::Zmm if align.unwrap_or(64) >= 64 => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vmovdqa32
            } else {
                X86CodegenOpcode::Vmovaps
            }
        }
        X86RegisterClass::Xmm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Movdqu
            } else {
                X86CodegenOpcode::Movups
            }
        }
        X86RegisterClass::Ymm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vmovdqu
            } else {
                X86CodegenOpcode::Vmovups
            }
        }
        X86RegisterClass::Zmm => {
            if hint_vec_is_int {
                X86CodegenOpcode::Vmovdqu32
            } else {
                X86CodegenOpcode::Vmovups
            }
        }
        X86RegisterClass::Tmm => panic!("No mov reg, reg for tmm"),
        X86RegisterClass::St => panic!("No mov for st reg"),
        _ => X86CodegenOpcode::Mov,
    }
}

impl X86Machine {
    pub fn write_move(
        &self,
        insns: &mut Vec<MceInstruction<X86Instruction>>,
        dest: &X86ValLocation,
        src: &X86ValLocation,
        assigns: &X86Assignments,
    ) {
        if dest == src {
            return;
        }
        match (dest, src) {
            (X86ValLocation::Null, X86ValLocation::Null)
            | (X86ValLocation::Null, X86ValLocation::StackDisp(_))
            | (X86ValLocation::StackDisp(_), X86ValLocation::Null) => {}
            (X86ValLocation::Register(dest), X86ValLocation::Register(src)) => {
                if dest.class() != src.class() {
                    panic!("Cannot move between register classes")
                }

                match dest.class() {
                    X86RegisterClass::St => todo!("fpreg"), // We need to do shenanigans to move between arbitrary st slots
                    X86RegisterClass::Tmm => todo!("tmmreg"), // Less shenanigans are needed here, but we need to spill to memory
                    _ => {
                        let opcode = move_opcode(dest.class(), None, false);

                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            opcode,
                            std_vec![X86Operand::Register(*dest), X86Operand::Register(*src)],
                        )))
                    }
                }
            }
            (X86ValLocation::StackDisp(off), X86ValLocation::Register(reg)) => {
                let align = (1 << (off | -0x80000000).trailing_zeros()).max(assigns.stack_align);
                let opcode: X86CodegenOpcode = move_opcode(reg.class(), Some(align), false);

                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                    opcode,
                    std_vec![
                        X86Operand::Memory(
                            reg.class(),
                            None,
                            X86MemoryOperand::Indirect {
                                reg: assigns.sp,
                                disp: Some(arch_ops::x86::codegen::X86Displacement::Offset(*off)),
                            },
                        ),
                        X86Operand::Register(*reg),
                    ],
                )));
            }
            (X86ValLocation::Register(reg), X86ValLocation::StackDisp(off)) => {
                let align = (1 << (off | -0x80000000).trailing_zeros()).max(assigns.stack_align);
                let opcode: X86CodegenOpcode = move_opcode(reg.class(), Some(align), false);

                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                    opcode,
                    std_vec![
                        X86Operand::Register(*reg),
                        X86Operand::Memory(
                            reg.class(),
                            None,
                            X86MemoryOperand::Indirect {
                                reg: assigns.sp,
                                disp: Some(arch_ops::x86::codegen::X86Displacement::Offset(*off)),
                            },
                        ),
                    ],
                )));
            }
            (X86ValLocation::Null, X86ValLocation::Register(_))
            | (X86ValLocation::Register(_), X86ValLocation::Null) => {
                panic!("Cannot move a register to/from null")
            }
            (X86ValLocation::StackDisp(_), X86ValLocation::StackDisp(_)) => {
                todo!("memory to memory")
            }
        }
    }
}

impl MceWriter for X86Machine {
    type Instruction = X86Instruction;

    fn write_machine_code<W: InsnWrite, F: FnMut(u128, String)>(
        &self,
        insn: &[xlang_backend::mach::mce::MceInstruction<Self::Instruction>],
        writer: &mut W,
        sym_accepter: &mut F,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(writer, self.mode.unwrap());
        for insn in insn {
            match insn {
                MceInstruction::BaseInsn(insn) => {
                    encoder.write_insn(insn.clone())?;
                }
                xlang_backend::mach::mce::MceInstruction::PrivateLabel(label) => {
                    sym_accepter(encoder.offset() as u128, label.clone());
                }
                xlang_backend::mach::mce::MceInstruction::RawBytes(bytes) => {
                    encoder.write_all(bytes)?
                }
                xlang_backend::mach::mce::MceInstruction::Split(insns) => {
                    self.write_machine_code(insns, &mut **encoder.inner_mut(), sym_accepter)?
                }
                xlang_backend::mach::mce::MceInstruction::Empty
                | xlang_backend::mach::mce::MceInstruction::DisableAnalyze
                | xlang_backend::mach::mce::MceInstruction::EnableAnalyze => {}
            }
        }

        Ok(())
    }

    fn write_assembly<W: core::fmt::Write>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
    ) -> core::fmt::Result {
        todo!("x86 assembly output")
    }
}

impl Machine<SsaInstruction> for X86Machine {
    fn matches_target(&self, targ: StringView) -> bool {
        let arch = Target::parse(&targ).arch();
        arch.is_x86() || arch == Architecture::X86_64
    }

    fn init_from_target(&mut self, properties: &TargetProperties) {
        let mode = match properties.arch.width {
            16 => X86Mode::Real,
            32 => X86Mode::Protected,
            64 => X86Mode::Long,
            _ => panic!("Not an x86 target"),
        };
        self.mode = Some(mode);
    }

    type Assignments = X86Assignments;

    type BlockClobbers = X86Clobbers;

    fn new_assignments(&self) -> Self::Assignments {
        let mode = self.mode.expect("Target must have been set");
        let gpr_size = mode.largest_gpr();
        let sp = X86Register::from_class(gpr_size, 4).unwrap();
        let max_gpr_num = match mode {
            X86Mode::Long => 16,
            _ => 8,
        };
        let int_registers = (0..max_gpr_num)
            .filter(|s| *s != 4)
            .flat_map(|x| X86Register::from_class(gpr_size, x))
            .collect();

        X86Assignments {
            mode,
            sp,
            available_int_registers: int_registers,
            return_reg: None,
            stack_width: 0,
            stack_align: 16,
            assigns: HashMap::new(),
        }
    }

    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        incoming: &[xlang_backend::ssa::OpaqueLocation],
        which: u32,
        incoming_set: &HashMap<u32, xlang::abi::vec::Vec<xlang_backend::ssa::OpaqueLocation>>,
        tys: &TypeInformation,
    ) -> Self::BlockClobbers {
        let mut clobbers = X86Clobbers {
            reg_owners: HashMap::new(),
            clobbered_at: HashMap::new(),
        };

        for loc in incoming {
            let id = loc.num;
            if let Some(loc) = assignments.assigns.get(&id) {
                match &loc.foreign_location {
                    X86ValLocation::Register(reg) => {
                        clobbers.reg_owners.insert(*reg, id);
                    }
                    X86ValLocation::Null | X86ValLocation::StackDisp(_) => {}
                }
            }
        }
        for (num, insn) in insns.iter().enumerate() {
            match insn {
                xlang_backend::ssa::SsaInstruction::Call(targ, locations) => {
                    eprintln!("Current width {:?}", assignments.stack_width);
                    assignments.stack_width = ((assignments.stack_width + 7) & !15) + 8;
                    eprintln!("Aligned width width {:?}", assignments.stack_width);

                    let callconv = X86CallConvInfo {
                        mode: assignments.mode,
                    };

                    let callconv = compute_call_conv(&callconv, &targ.real_ty, &targ.call_ty, tys);

                    // TODO: Clobber non-saved registers

                    for (loc, call_loc) in locations.params.iter().zip(callconv.params()) {
                        match call_loc {
                            CallConvLocation::Null => {}
                            CallConvLocation::Register(reg) => {
                                let ty_size = tys.type_size(&loc.ty).unwrap();
                                let ty_align = tys.type_align(&loc.ty).unwrap();
                                if let Some(assign) = assignments.assigns.get_mut(&loc.num) {
                                    clobbers.mark_used(
                                        loc.num,
                                        assign,
                                        &mut assignments.stack_width,
                                        ty_size,
                                        ty_align,
                                    );
                                }

                                clobbers.mark_clobbered(*reg, num, Some(loc.num));
                                assignments
                                    .assigns
                                    .get_or_insert_with_mut(loc.num, |_| LocationAssignment {
                                        foreign_location: X86ValLocation::Register(*reg),
                                        owning_bb: which,
                                        change_owner: vec![],
                                    })
                                    .change_owner
                                    .push((num, X86ValLocation::Register(*reg)));
                            }
                            loc => todo!("{:?}", loc),
                        }
                    }

                    for reg in callconv.tag().volatile_regs() {
                        clobbers.mark_clobbered(*reg, num, None);
                    }

                    if let Some(ret) = &locations.ret {
                        match callconv.ret_location() {
                            CallConvLocation::Null => {
                                assignments.assigns.insert(
                                    ret.num,
                                    LocationAssignment {
                                        foreign_location: X86ValLocation::Null,
                                        owning_bb: which,
                                        change_owner: vec![],
                                    },
                                );
                            }
                            CallConvLocation::Register(reg) => {
                                assignments.assigns.insert(
                                    ret.num,
                                    LocationAssignment {
                                        foreign_location: X86ValLocation::Register(*reg),
                                        owning_bb: which,
                                        change_owner: vec![],
                                    },
                                );
                            }
                            loc => todo!("{:?}", loc),
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::Jump(targ, old_locs)
                | xlang_backend::ssa::SsaInstruction::Fallthrough(targ, old_locs) => {
                    let foreign_locs = &incoming_set[targ];

                    eprintln!("{:?}", foreign_locs);

                    for (old_loc, new_loc) in old_locs.iter().zip(foreign_locs) {
                        match (
                            assignments
                                .assigns
                                .get_mut(&new_loc.num)
                                .map(|x| x.foreign_location.clone()),
                            assignments.assigns.get_mut(&old_loc.num),
                        ) {
                            (Some(foreign), Some(old)) => {
                                let ty_size = tys.type_size(&old_loc.ty).unwrap();
                                let ty_align = tys.type_align(&old_loc.ty).unwrap();
                                clobbers.mark_used(
                                    old_loc.num,
                                    old,
                                    &mut assignments.stack_width,
                                    ty_size,
                                    ty_align,
                                );
                                old.change_owner.push((num, foreign));
                            }
                            (Some(foreign), None) => {
                                assignments.assigns.insert(
                                    old_loc.num,
                                    LocationAssignment {
                                        foreign_location: foreign,
                                        owning_bb: which,
                                        change_owner: vec![],
                                    },
                                );
                            }
                            (None, Some(old)) => {
                                let incoming = old
                                    .change_owner
                                    .last()
                                    .map_or(&old.foreign_location, |(_, x)| x)
                                    .clone();
                                let ty_size = tys.type_size(&old_loc.ty).unwrap();
                                let ty_align = tys.type_align(&old_loc.ty).unwrap();
                                clobbers.mark_used(
                                    old_loc.num,
                                    old,
                                    &mut assignments.stack_width,
                                    ty_size,
                                    ty_align,
                                );

                                assignments.assigns.insert(
                                    new_loc.num,
                                    LocationAssignment {
                                        foreign_location: incoming,
                                        owning_bb: *targ,
                                        change_owner: vec![],
                                    },
                                );
                            }
                            (None, None) => {}
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::Exit(val) => match &**val {
                    [] => {}
                    [retval] => {
                        let reg = assignments.return_reg;
                        if let Some(reg) = reg {
                            let ty_size = tys.type_size(&retval.ty).unwrap();
                            let ty_align = tys.type_align(&retval.ty).unwrap();
                            if let Some(assign) = assignments.assigns.get_mut(&retval.num) {
                                clobbers.mark_used(
                                    retval.num,
                                    assign,
                                    &mut assignments.stack_width,
                                    ty_size,
                                    ty_align,
                                );
                            }
                            let loc = X86ValLocation::Register(reg);

                            assignments
                                .assigns
                                .get_or_insert_with_mut(retval.num, |_| LocationAssignment {
                                    foreign_location: loc.clone(),
                                    owning_bb: which,
                                    change_owner: vec![],
                                })
                                .change_owner
                                .push((num, loc));
                        }
                    }
                    vals => panic!("exit with more than 1 value: {:?}", vals),
                },
                xlang_backend::ssa::SsaInstruction::Tailcall(targ, locs) => {
                    let callconv = X86CallConvInfo {
                        mode: assignments.mode,
                    };

                    let callconv = compute_call_conv(&callconv, &targ.real_ty, &targ.call_ty, tys);

                    for (loc, call_loc) in locs.iter().zip(callconv.params()) {
                        match call_loc {
                            CallConvLocation::Null => {}
                            CallConvLocation::Register(reg) => {
                                let ty_size = tys.type_size(&loc.ty).unwrap();
                                let ty_align = tys.type_align(&loc.ty).unwrap();
                                if let Some(assign) = assignments.assigns.get_mut(&loc.num) {
                                    clobbers.mark_used(
                                        loc.num,
                                        assign,
                                        &mut assignments.stack_width,
                                        ty_size,
                                        ty_align,
                                    );
                                }
                                assignments
                                    .assigns
                                    .get_or_insert_with_mut(loc.num, |_| LocationAssignment {
                                        foreign_location: X86ValLocation::Register(*reg),
                                        owning_bb: which,
                                        change_owner: vec![],
                                    })
                                    .change_owner
                                    .push((num, X86ValLocation::Register(*reg)));
                            }
                            loc => todo!("{:?}", loc),
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::Trap(_) => {}
                xlang_backend::ssa::SsaInstruction::LoadImmediate(_, _)
                | xlang_backend::ssa::SsaInstruction::LoadSymAddr(_, _)
                | xlang_backend::ssa::SsaInstruction::ZeroInit(_) => {}
            }
        }
        clobbers
    }

    fn codegen_block<F: Fn(u32) -> std::prelude::v1::String>(
        &self,
        assignments: &Self::Assignments,
        ssa_insns: &[xlang_backend::ssa::SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> Vec<MceInstruction<X86Instruction>> {
        let mut insns = Vec::new();
        let mut cur_locations = HashMap::<_, _>::new();
        for Pair(addr, loc) in &assignments.assigns {
            if loc.owning_bb == which {
                cur_locations.insert(*addr, loc.foreign_location.clone());
            }
        }
        for (num, insn) in ssa_insns.iter().enumerate() {
            for Pair(addr, loc) in &assignments.assigns {
                if loc.owning_bb == which {
                    for (at, new_loc) in &loc.change_owner {
                        if *at == num {
                            self.write_move(&mut insns, new_loc, &cur_locations[addr], assignments);
                            break;
                        }
                    }
                }
            }

            match insn {
                xlang_backend::ssa::SsaInstruction::Call(targ, _) => match &targ.ptr {
                    OpaquePtr::Symbol(sym) => {
                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            X86CodegenOpcode::Call,
                            std_vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                        )))
                    }
                    OpaquePtr::Pointer(ptr) => todo!("indirect call"),
                },
                xlang_backend::ssa::SsaInstruction::Jump(targ, _) => {
                    insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                        X86CodegenOpcode::Jmp,
                        std_vec![X86Operand::RelOffset(Address::Symbol {
                            name: label_sym(*targ),
                            disp: 0,
                        })],
                    )));
                }
                xlang_backend::ssa::SsaInstruction::Fallthrough(_, _) => {}
                xlang_backend::ssa::SsaInstruction::Exit(val) => {
                    if assignments.stack_width > 0 {
                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            X86CodegenOpcode::Add,
                            std_vec![
                                X86Operand::Register(assignments.sp),
                                X86Operand::Immediate(assignments.stack_width as i64),
                            ],
                        )));
                    }
                    insns.push(MceInstruction::BaseInsn(X86Instruction::Ret));
                }
                xlang_backend::ssa::SsaInstruction::Tailcall(targ, _) => match &targ.ptr {
                    OpaquePtr::Symbol(sym) => {
                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            X86CodegenOpcode::Jmp,
                            std_vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                        )))
                    }
                    OpaquePtr::Pointer(ptr) => todo!("indirect call"),
                },
                xlang_backend::ssa::SsaInstruction::Trap(Trap::Breakpoint) => {
                    insns.push(MceInstruction::BaseInsn(X86Instruction::Int3))
                }
                xlang_backend::ssa::SsaInstruction::Trap(_) => {
                    insns.push(MceInstruction::BaseInsn(X86Instruction::Ud2))
                }
                xlang_backend::ssa::SsaInstruction::LoadImmediate(loc, val) => {
                    if let Some(location) = cur_locations.get(&loc.num) {
                        match location {
                            X86ValLocation::Register(reg) => {
                                let mov = move_opcode(reg.class(), None, true);
                                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                                    mov,
                                    std_vec![
                                        X86Operand::Register(*reg),
                                        X86Operand::Immediate((*val) as i64),
                                    ],
                                )));
                            }
                            X86ValLocation::Null => {}
                            X86ValLocation::StackDisp(_) => todo!("memory"),
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::LoadSymAddr(loc, addr) => {
                    if let Some(location) = cur_locations.get(&loc.num) {
                        match location {
                            X86ValLocation::Register(reg) => {
                                let lea = X86CodegenOpcode::Lea;
                                let cl = reg.class();

                                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                                    lea,
                                    std_vec![
                                        X86Operand::Register(*reg),
                                        X86Operand::Memory(
                                            cl,
                                            None,
                                            X86MemoryOperand::RelAddr(addr.clone()),
                                        ),
                                    ],
                                )));
                            }
                            X86ValLocation::Null => {}
                            X86ValLocation::StackDisp(_) => todo!("memory"),
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::ZeroInit(dest) => {
                    if let Some(loc) = cur_locations.get(&dest.num) {
                        match loc {
                            X86ValLocation::Register(reg) => {
                                let is_int = match &*dest.ty {
                                    ir::Type::Scalar(ir::ScalarType {
                                        kind:
                                            ir::ScalarTypeKind::Float { .. } | ir::ScalarTypeKind::Posit,
                                        ..
                                    }) => false,
                                    _ => true, // TODO: Be better with heauristics arround aggregate types
                                };

                                let xor = xor_opcode(reg.class(), is_int);

                                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                                    xor,
                                    std_vec![
                                        X86Operand::Register(*reg),
                                        X86Operand::Register(*reg)
                                    ],
                                )));
                            }
                            X86ValLocation::Null => {}
                            X86ValLocation::StackDisp(_) => todo!("memory"),
                        }
                    }
                }
            }
        }

        insns
    }

    fn codegen_prologue(
        &self,
        assignments: &Self::Assignments,
    ) -> Vec<MceInstruction<X86Instruction>> {
        let mut insns = Vec::new();
        if assignments.stack_width != 0 {
            insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                X86CodegenOpcode::Sub,
                std_vec![
                    X86Operand::Register(assignments.sp),
                    X86Operand::Immediate(assignments.stack_width as i64),
                ],
            )));
        }

        insns
    }

    fn assign_call_conv(
        &self,
        assignments: &mut Self::Assignments,
        incoming: &[xlang_backend::ssa::OpaqueLocation],
        fnty: &xlang::ir::FnType,
        tys: &TypeInformation,
        which: u32,
    ) {
        let callconv = X86CallConvInfo {
            mode: assignments.mode,
        };
        let callconv = compute_call_conv(&callconv, fnty, fnty, tys);

        for (param, incoming) in callconv.params().iter().zip(incoming) {
            let loc = match param {
                CallConvLocation::Null => X86ValLocation::Null,
                CallConvLocation::Register(reg) => X86ValLocation::Register(*reg),
                loc => todo!("Param in {:?}", loc),
            };

            let assignment = LocationAssignment {
                foreign_location: loc,
                change_owner: vec![],
                owning_bb: which,
            };

            assignments.assigns.insert(incoming.num, assignment);
        }

        match callconv.ret_location() {
            CallConvLocation::Null => {}
            CallConvLocation::Register(reg) => assignments.return_reg = Some(*reg),
            loc => todo!("Return in {:?}", loc),
        }
    }
}

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::abi::boxed::Box::new(SsaCodegenPlugin::new(X86Machine{mode: None})))
}}

xlang::plugin_abi_version!("0.1");
