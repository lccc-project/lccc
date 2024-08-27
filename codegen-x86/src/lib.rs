use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        codegen::{X86CodegenOpcode, X86Encoder, X86Instruction, X86MemoryOperand, X86Operand},
        insn::X86Opcode,
        X86Gpr, X86Mode, X86Register, X86RegisterClass,
    },
};
use callconv::X86CallConvInfo;
use core::cell::Cell;
use std::{hash::Hash, vec as std_vec};
use std::{io::Write, rc::Rc};
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
    regalloc::{Assignment, RegAllocClobbers, RegGroup},
    ssa::{OpaquePtr, SsaInstruction},
    ty::TypeInformation,
    SsaCodegenPlugin,
};
use xlang_struct::BranchCondition;

mod callconv;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86ValLocation {
    Null,
    Register(X86Register),
    StackDisp(i32),
}

#[derive(Clone, Copy, Debug)]
struct StackLayout {
    size: i32,
    align: i32,
}
#[derive(Clone, Debug)]
pub struct LocationAssignment {
    foreign_location: X86ValLocation,
    owning_bb: u32,
    change_owner: Vec<(usize, X86ValLocation)>,
    size: i32,
    align: i32,
    stack_slot: Option<i32>,
    stack_size_shared: Rc<Cell<StackLayout>>,
}

impl Assignment for LocationAssignment {
    type Register = X86Register;

    fn current_owned_register(&self) -> Option<Self::Register> {
        todo!()
    }

    fn move_to_register(&mut self, reg: Self::Register, at: usize) {
        self.change_owner.push((at, X86ValLocation::Register(reg)));
    }

    fn move_to_memory(&mut self, at: usize) {
        let slot = *self.stack_slot.get_or_insert_with(|| {
            let StackLayout { size, align } = self.stack_size_shared.get();
            let new_align = align.max(self.align);
            let slot = (size + (self.align - 1)) & !(self.align - 1);
            let new_size = slot + self.size;
            self.stack_size_shared.set(StackLayout {
                size: new_size,
                align: new_align,
            });
            -slot
        });

        self.change_owner
            .push((at, X86ValLocation::StackDisp(slot)));
    }
}

pub struct X86Machine {
    mode: Option<X86Mode>,
}

pub struct X86Assignments {
    mode: X86Mode,
    sp: X86Register,
    return_reg: Option<X86Register>,
    stack_layout: Rc<Cell<StackLayout>>,
    assigns: HashMap<u32, LocationAssignment>,
    available_groups: Vec<X86RegisterGroup>,
}

#[derive(Copy, Clone, Debug)]
pub struct X86RegisterGroup {
    class: X86RegisterClass,
    max_regno: u8,
}

impl Hash for X86RegisterGroup {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.class.hash(state)
    }
}

impl PartialEq for X86RegisterGroup {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class
    }
}

impl Eq for X86RegisterGroup {}

impl RegGroup for X86RegisterGroup {
    type Register = X86Register;

    fn registers(&self) -> impl IntoIterator<Item = Self::Register> + '_ {
        (0..self.max_regno)
            .filter_map(|r| X86Register::from_class(self.class, r))
            .filter(|r| !matches!(r.gpr(), Some(X86Gpr::Sp)))
    }
}

type X86Clobbers = RegAllocClobbers<X86RegisterGroup>;

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

fn cmp_opcode(class: X86RegisterClass) -> X86CodegenOpcode {
    match class {
        X86RegisterClass::Xmm
        | X86RegisterClass::Ymm
        | X86RegisterClass::Zmm
        | X86RegisterClass::St
        | X86RegisterClass::Tmm => todo!("fp, vectors, and tiles"),
        _ => X86CodegenOpcode::Cmp,
    }
}

fn move_opcode(
    class: X86RegisterClass,
    align: Option<i32>,
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
    pub fn write_cmp(
        &self,
        insns: &mut Vec<MceInstruction<X86Instruction>>,
        src1: &X86ValLocation,
        src2: &X86ValLocation,
        cmp_op: BranchCondition,
        assigns: &X86Assignments,
        override_mode: Option<X86RegisterClass>,
    ) {
        match (src1, src2) {
            (X86ValLocation::Null, X86ValLocation::Null)
            | (X86ValLocation::Null, X86ValLocation::StackDisp(_))
            | (X86ValLocation::StackDisp(_), X86ValLocation::Null) => {}
            (X86ValLocation::Register(dest), X86ValLocation::Register(src)) => {
                if dest.class() != src.class() {
                    panic!("Cannot move between register classes")
                }

                match dest.class() {
                    X86RegisterClass::St => todo!("fpreg"), // We need to do shenanigans to move between arbitrary st slots
                    X86RegisterClass::Tmm => todo!("tmmreg"),
                    X86RegisterClass::Xmm | X86RegisterClass::Ymm | X86RegisterClass::Zmm => {
                        todo!("xmmreg")
                    } // also need shenanigans for fp
                    _ => {
                        let opcode = cmp_opcode(dest.class());

                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            opcode,
                            std_vec![X86Operand::Register(*dest), X86Operand::Register(*src)],
                        )))
                    }
                }
            }
            (X86ValLocation::StackDisp(off), X86ValLocation::Register(reg)) => {
                let reg = override_mode
                    .and_then(|r| X86Register::from_class(r, reg.regnum()))
                    .unwrap_or(*reg);
                let opcode: X86CodegenOpcode = cmp_opcode(reg.class());

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
                        X86Operand::Register(reg),
                    ],
                )));
            }
            (X86ValLocation::Register(reg), X86ValLocation::StackDisp(off)) => {
                let reg = override_mode
                    .and_then(|r| X86Register::from_class(r, reg.regnum()))
                    .unwrap_or(*reg);
                let opcode: X86CodegenOpcode = cmp_opcode(reg.class());

                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                    opcode,
                    std_vec![
                        X86Operand::Register(reg),
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
    pub fn write_move(
        &self,
        insns: &mut Vec<MceInstruction<X86Instruction>>,
        dest: &X86ValLocation,
        src: &X86ValLocation,
        assigns: &X86Assignments,
        override_mode: Option<X86RegisterClass>,
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
                let reg = override_mode
                    .and_then(|r| X86Register::from_class(r, reg.regnum()))
                    .unwrap_or(*reg);
                let stack_layout = assigns.stack_layout.get();
                let align = (1 << (off | -0x80000000).trailing_zeros()).max(stack_layout.align);
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
                        X86Operand::Register(reg),
                    ],
                )));
            }
            (X86ValLocation::Register(reg), X86ValLocation::StackDisp(off)) => {
                let reg = override_mode
                    .and_then(|r| X86Register::from_class(r, reg.regnum()))
                    .unwrap_or(*reg);
                let stack_layout = assigns.stack_layout.get();
                let align = (1 << (off | -0x80000000).trailing_zeros()).max(stack_layout.align);
                let opcode: X86CodegenOpcode = move_opcode(reg.class(), Some(align), false);

                insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                    opcode,
                    std_vec![
                        X86Operand::Register(reg),
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

    fn real_stack_width(assignments: &X86Assignments) -> i32 {
        let stack_layout = assignments.stack_layout.get();

        (stack_layout.size
            + ((8 & (stack_layout.align - 1)) as i32)
            + (stack_layout.align as i32 - 1))
            & !(stack_layout.align - 1) as i32
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

    fn optimize_mce_context(&self, insns: &mut [MceInstruction<Self::Instruction>]) {
        let mut analysis_enabled = true;

        for insn in insns {
            match insn {
                MceInstruction::EnableAnalyze => {
                    analysis_enabled = true;
                }
                MceInstruction::DisableAnalyze => {
                    analysis_enabled = false;
                }
                MceInstruction::Empty
                | MceInstruction::PrivateLabel(_)
                | MceInstruction::RawBytes(_) => {}
                MceInstruction::Split(insns) => {
                    if analysis_enabled {
                        self.optimize_mce_context(insns)
                    }
                }
                MceInstruction::BaseInsn(insn) => {
                    if analysis_enabled {
                        match (insn.prefix(), insn.opcode(), insn.operands()) {
                            (
                                None,
                                X86CodegenOpcode::Mov,
                                [X86Operand::Register(reg), X86Operand::Immediate(0)],
                            ) => {
                                if reg.gpr().is_some() {
                                    *insn = X86Instruction::new(
                                        X86CodegenOpcode::Xor,
                                        std_vec![
                                            X86Operand::Register(*reg),
                                            X86Operand::Register(*reg),
                                        ],
                                    );
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
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

        let max_vector_num = max_gpr_num; // TODO Support APX and AVX512/AVX10

        let groups = vec![
            X86RegisterGroup {
                class: mode.largest_gpr(),
                max_regno: max_gpr_num,
            },
            X86RegisterGroup {
                class: X86RegisterClass::Xmm,
                max_regno: max_gpr_num,
            },
        ];

        X86Assignments {
            mode,
            sp,
            available_groups: groups,
            return_reg: None,
            stack_layout: Rc::new(Cell::new(StackLayout { size: 0, align: 1 })),
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
        let mut clobbers = X86Clobbers::from_groups(assignments.available_groups.iter().copied());

        for loc in incoming {
            let id = loc.num;
            if let Some(loc) = assignments.assigns.get(&id) {
                match &loc.foreign_location {
                    X86ValLocation::Register(reg) => clobbers.mark_owning(id, *reg, 0),
                    X86ValLocation::Null | X86ValLocation::StackDisp(_) => {}
                }
            }
        }
        for (num, insn) in insns.iter().enumerate() {
            match insn {
                xlang_backend::ssa::SsaInstruction::Call(targ, locations) => {
                    let StackLayout { size, align } = assignments.stack_layout.get();
                    assignments.stack_layout.set(StackLayout {
                        size,
                        align: align.max(16),
                    });

                    let callconv = X86CallConvInfo {
                        mode: assignments.mode,
                    };

                    let callconv = compute_call_conv(&callconv, &targ.real_ty, &targ.call_ty, tys);

                    // TODO: Clobber non-saved registers

                    for (loc, call_loc) in locations.params.iter().zip(callconv.params()) {
                        let size = tys.type_size(&loc.ty).unwrap() as i32;
                        let align = tys.type_align(&loc.ty).unwrap() as i32;
                        match call_loc {
                            CallConvLocation::Null => {}
                            CallConvLocation::Register(reg) => {
                                let ty_size = tys.type_size(&loc.ty).unwrap();
                                let ty_align = tys.type_align(&loc.ty).unwrap();
                                if let Some(assign) = assignments.assigns.get_mut(&loc.num) {
                                    clobbers.mark_used(loc.num, assign);
                                }

                                clobbers.mark_clobbered(*reg, Some(loc.num), num);
                                let stack_layout = assignments.stack_layout.clone();
                                assignments
                                    .assigns
                                    .get_or_insert_with_mut(loc.num, move |_| LocationAssignment {
                                        foreign_location: X86ValLocation::Register(*reg),
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: stack_layout,
                                    })
                                    .change_owner
                                    .push((num, X86ValLocation::Register(*reg)));

                                clobbers.mark_owning(loc.num, *reg, num);
                            }
                            loc => todo!("{:?}", loc),
                        }
                    }

                    for reg in callconv.tag().volatile_regs() {
                        clobbers.mark_clobbered(*reg, None, num);
                    }

                    if let Some(ret) = &locations.ret {
                        let size = tys.type_size(&ret.ty).unwrap() as i32;
                        let align = tys.type_align(&ret.ty).unwrap() as i32;

                        match callconv.ret_location() {
                            CallConvLocation::Null => {
                                assignments.assigns.insert(
                                    ret.num,
                                    LocationAssignment {
                                        foreign_location: X86ValLocation::Null,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
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
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                                clobbers.mark_owning(ret.num, *reg, num);
                            }
                            loc => todo!("{:?}", loc),
                        }
                    }
                }

                SsaInstruction::Branch(_, l, r, targ, old_locs) => {
                    let foreign_locs = &incoming_set[targ];

                    let l_size = tys.type_size(&l.ty).unwrap() as i32;
                    let l_align = tys.type_align(&l.ty).unwrap() as i32;
                    let r_size = tys.type_size(&r.ty).unwrap() as i32;
                    let r_align = tys.type_align(&r.ty).unwrap() as i32;

                    let largest_gpr = assignments.mode.largest_gpr();
                    let stack_layout = assignments.stack_layout.clone();

                    let l_assign = assignments.assigns.get_or_insert_with_mut(l.num, |_| {
                        let reg = clobbers.alloc_reg(
                            &X86RegisterGroup {
                                class: largest_gpr,
                                max_regno: 0,
                            },
                            num,
                        );
                        clobbers.mark_owning(l.num, reg, num);
                        LocationAssignment {
                            foreign_location: X86ValLocation::Register(reg),
                            owning_bb: which,
                            change_owner: Vec::new(),
                            size: l_size,
                            align: l_align,
                            stack_slot: None,
                            stack_size_shared: stack_layout.clone(),
                        }
                    });

                    clobbers.mark_used(l.num, l_assign);

                    let r_assign = assignments.assigns.get_or_insert_with_mut(l.num, |_| {
                        let reg = clobbers.alloc_reg(
                            &X86RegisterGroup {
                                class: largest_gpr,
                                max_regno: 0,
                            },
                            num,
                        );
                        clobbers.mark_owning(l.num, reg, num);
                        LocationAssignment {
                            foreign_location: X86ValLocation::Register(reg),
                            owning_bb: which,
                            change_owner: Vec::new(),
                            size: l_size,
                            align: l_align,
                            stack_slot: None,
                            stack_size_shared: stack_layout.clone(),
                        }
                    });

                    clobbers.mark_used(r.num, r_assign);

                    for (old_loc, new_loc) in old_locs.iter().zip(foreign_locs) {
                        let size = tys.type_size(&old_loc.ty).unwrap() as i32;
                        let align = tys.type_align(&old_loc.ty).unwrap() as i32;
                        match (
                            assignments
                                .assigns
                                .get_mut(&new_loc.num)
                                .map(|x| x.foreign_location.clone()),
                            assignments.assigns.get_mut(&old_loc.num),
                        ) {
                            (Some(foreign), Some(old)) => {
                                clobbers.mark_used(old_loc.num, old);

                                old.change_owner.push((num, foreign));
                            }
                            (Some(foreign), None) => {
                                assignments.assigns.insert(
                                    old_loc.num,
                                    LocationAssignment {
                                        foreign_location: foreign,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                            }
                            (None, Some(old)) => {
                                let ty_size = tys.type_size(&old_loc.ty).unwrap();
                                let ty_align = tys.type_align(&old_loc.ty).unwrap();
                                clobbers.mark_used(old_loc.num, old);
                                let incoming = old
                                    .change_owner
                                    .last()
                                    .map_or(&old.foreign_location, |(_, x)| x)
                                    .clone();

                                let old_slot = old.stack_slot;
                                assignments.assigns.insert(
                                    new_loc.num,
                                    LocationAssignment {
                                        foreign_location: incoming,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: old_slot,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                            }
                            (None, None) => {}
                        }
                    }
                }
                SsaInstruction::BranchZero(_, l, targ, old_locs) => {
                    let foreign_locs = &incoming_set[targ];

                    for (old_loc, new_loc) in old_locs.iter().zip(foreign_locs) {
                        let size = tys.type_size(&old_loc.ty).unwrap() as i32;
                        let align = tys.type_align(&old_loc.ty).unwrap() as i32;
                        match (
                            assignments
                                .assigns
                                .get_mut(&new_loc.num)
                                .map(|x| x.foreign_location.clone()),
                            assignments.assigns.get_mut(&old_loc.num),
                        ) {
                            (Some(foreign), Some(old)) => {
                                clobbers.mark_used(old_loc.num, old);

                                old.change_owner.push((num, foreign));
                            }
                            (Some(foreign), None) => {
                                assignments.assigns.insert(
                                    old_loc.num,
                                    LocationAssignment {
                                        foreign_location: foreign,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                            }
                            (None, Some(old)) => {
                                let ty_size = tys.type_size(&old_loc.ty).unwrap();
                                let ty_align = tys.type_align(&old_loc.ty).unwrap();
                                clobbers.mark_used(old_loc.num, old);
                                let incoming = old
                                    .change_owner
                                    .last()
                                    .map_or(&old.foreign_location, |(_, x)| x)
                                    .clone();

                                let old_slot = old.stack_slot;
                                assignments.assigns.insert(
                                    new_loc.num,
                                    LocationAssignment {
                                        foreign_location: incoming,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: old_slot,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                            }
                            (None, None) => {}
                        }
                    }
                }

                xlang_backend::ssa::SsaInstruction::Jump(targ, old_locs)
                | xlang_backend::ssa::SsaInstruction::Fallthrough(targ, old_locs) => {
                    let foreign_locs = &incoming_set[targ];

                    for (old_loc, new_loc) in old_locs.iter().zip(foreign_locs) {
                        let size = tys.type_size(&old_loc.ty).unwrap() as i32;
                        let align = tys.type_align(&old_loc.ty).unwrap() as i32;
                        match (
                            assignments
                                .assigns
                                .get_mut(&new_loc.num)
                                .map(|x| x.foreign_location.clone()),
                            assignments.assigns.get_mut(&old_loc.num),
                        ) {
                            (Some(foreign), Some(old)) => {
                                clobbers.mark_used(old_loc.num, old);

                                old.change_owner.push((num, foreign));
                            }
                            (Some(foreign), None) => {
                                assignments.assigns.insert(
                                    old_loc.num,
                                    LocationAssignment {
                                        foreign_location: foreign,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
                                    },
                                );
                            }
                            (None, Some(old)) => {
                                let ty_size = tys.type_size(&old_loc.ty).unwrap();
                                let ty_align = tys.type_align(&old_loc.ty).unwrap();
                                clobbers.mark_used(old_loc.num, old);
                                let incoming = old
                                    .change_owner
                                    .last()
                                    .map_or(&old.foreign_location, |(_, x)| x)
                                    .clone();

                                let old_slot = old.stack_slot;
                                assignments.assigns.insert(
                                    new_loc.num,
                                    LocationAssignment {
                                        foreign_location: incoming,
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: old_slot,
                                        stack_size_shared: assignments.stack_layout.clone(),
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
                        let size = tys.type_size(&retval.ty).unwrap() as i32;
                        let align = tys.type_align(&retval.ty).unwrap() as i32;
                        let reg = assignments.return_reg;
                        if let Some(reg) = reg {
                            if let Some(assign) = assignments.assigns.get_mut(&retval.num) {
                                clobbers.mark_used(retval.num, assign);
                            }
                            let loc = X86ValLocation::Register(reg);

                            assignments
                                .assigns
                                .get_or_insert_with_mut(retval.num, |_| LocationAssignment {
                                    foreign_location: loc.clone(),
                                    owning_bb: which,
                                    change_owner: vec![],
                                    size,
                                    align,
                                    stack_slot: None,
                                    stack_size_shared: assignments.stack_layout.clone(),
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
                                let size = tys.type_size(&loc.ty).unwrap() as i32;
                                let align = tys.type_align(&loc.ty).unwrap() as i32;
                                if let Some(assign) = assignments.assigns.get_mut(&loc.num) {
                                    clobbers.mark_used(loc.num, assign);
                                }
                                assignments
                                    .assigns
                                    .get_or_insert_with_mut(loc.num, |_| LocationAssignment {
                                        foreign_location: X86ValLocation::Register(*reg),
                                        owning_bb: which,
                                        change_owner: vec![],
                                        size,
                                        align,
                                        stack_slot: None,
                                        stack_size_shared: assignments.stack_layout.clone(),
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
        _: Self::BlockClobbers,
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
                            let size = loc.size;
                            let over = if (size <= 8) && (size == (size & -size)) {
                                X86RegisterClass::gpr_size(size as usize, assignments.mode)
                            } else {
                                None
                            };
                            self.write_move(
                                &mut insns,
                                new_loc,
                                &cur_locations[addr],
                                assignments,
                                over,
                            );
                            *cur_locations.get_mut(addr).unwrap() = new_loc.clone();
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
                    let real_stack_width = Self::real_stack_width(assignments);
                    if real_stack_width > 0 {
                        insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                            X86CodegenOpcode::Add,
                            std_vec![
                                X86Operand::Register(assignments.sp),
                                X86Operand::Immediate(real_stack_width as i64 - 8),
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
                SsaInstruction::Branch(op, l, r, targ, vals) => {
                    let jmp_insn = match (op, &*l.ty) {
                        (BranchCondition::Equal, _) => X86CodegenOpcode::Jz,
                        (BranchCondition::NotEqual, _) => X86CodegenOpcode::Jnz,
                        (
                            BranchCondition::Less,
                            ir::Type::Scalar(ir::ScalarType {
                                kind: ir::ScalarTypeKind::Integer { signed: false, .. },
                                ..
                            }),
                        ) => X86CodegenOpcode::Jb,
                        (BranchCondition::Less, _) => X86CodegenOpcode::Jl,
                        (
                            BranchCondition::Greater,
                            ir::Type::Scalar(ir::ScalarType {
                                kind: ir::ScalarTypeKind::Integer { signed: false, .. },
                                ..
                            }),
                        ) => X86CodegenOpcode::Jnbe,
                        (BranchCondition::Greater, _) => X86CodegenOpcode::Jnle,
                        (
                            BranchCondition::LessEqual,
                            ir::Type::Scalar(ir::ScalarType {
                                kind: ir::ScalarTypeKind::Integer { signed: false, .. },
                                ..
                            }),
                        ) => X86CodegenOpcode::Jbe,
                        (BranchCondition::LessEqual, _) => X86CodegenOpcode::Jle,
                        (
                            BranchCondition::GreaterEqual,
                            ir::Type::Scalar(ir::ScalarType {
                                kind: ir::ScalarTypeKind::Integer { signed: false, .. },
                                ..
                            }),
                        ) => X86CodegenOpcode::Jnb,
                        (BranchCondition::GreaterEqual, _) => X86CodegenOpcode::Jnl,
                        _ => unreachable!("Trivial branches aren't passed as branch"),
                    };

                    let lloc = &cur_locations[&l.num];
                    let rloc = &cur_locations[&r.num];
                    let size = tys.type_size(&l.ty).unwrap();
                    let over = if (size <= 8) && size.is_power_of_two() {
                        X86RegisterClass::gpr_size(size as usize, assignments.mode)
                    } else {
                        None
                    };

                    self.write_cmp(&mut insns, lloc, rloc, *op, assignments, over);
                    insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                        jmp_insn,
                        std_vec![X86Operand::RelOffset(Address::Symbol {
                            name: label_sym(*targ),
                            disp: 0,
                        })],
                    )));
                }
                SsaInstruction::BranchZero(_, _, _, _) => todo!("branch"),
            }
        }

        insns
    }

    fn codegen_prologue(
        &self,
        assignments: &Self::Assignments,
    ) -> Vec<MceInstruction<X86Instruction>> {
        let real_stack_width = Self::real_stack_width(assignments);
        let mut insns = Vec::new();
        if real_stack_width != 0 {
            insns.push(MceInstruction::BaseInsn(X86Instruction::new(
                X86CodegenOpcode::Sub,
                std_vec![
                    X86Operand::Register(assignments.sp),
                    X86Operand::Immediate((real_stack_width as i64).saturating_sub(8)),
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
            let size = tys.type_size(&incoming.ty).unwrap() as i32;
            let align = tys.type_align(&incoming.ty).unwrap() as i32;
            let loc = match param {
                CallConvLocation::Null => X86ValLocation::Null,
                CallConvLocation::Register(reg) => X86ValLocation::Register(*reg),
                loc => todo!("Param in {:?}", loc),
            };

            let assignment = LocationAssignment {
                foreign_location: loc,
                owning_bb: which,
                change_owner: vec![],
                size,
                align,
                stack_slot: None,
                stack_size_shared: assignments.stack_layout.clone(),
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
