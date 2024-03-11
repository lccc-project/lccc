use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        codegen::{X86CodegenOpcode, X86Encoder, X86Instruction, X86Operand},
        X86Mode, X86Register, X86RegisterClass,
    },
};
use callconv::X86CallConvInfo;
use target_tuples::{Architecture, Target};
use xlang::{
    abi::{pair::Pair, string::StringView},
    plugin::XLangCodegen,
    prelude::v1::{DynBox, HashMap},
    targets::properties::TargetProperties,
};
use xlang_backend::{
    callconv::{compute_call_conv, CallConvLocation},
    expr::Trap,
    mach::Machine,
    ssa::OpaquePtr,
    ty::TypeInformation,
    SsaCodegenPlugin,
};

mod callconv;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum X86ValLocation {
    Null,
    Register(X86Register),
}

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
    assigns: HashMap<u32, LocationAssignment>,
}

pub struct X86Clobbers {}

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
    pub fn write_move<W: InsnWrite>(
        &self,
        writer: &mut X86Encoder<W>,
        dest: &X86ValLocation,
        src: &X86ValLocation,
    ) -> std::io::Result<()> {
        if dest == src {
            return Ok(());
        }
        match (dest, src) {
            (X86ValLocation::Null, X86ValLocation::Null) => Ok(()),
            (X86ValLocation::Register(dest), X86ValLocation::Register(src)) => {
                if dest.class() != src.class() {
                    panic!("Cannot move between register classes")
                }

                match dest.class() {
                    X86RegisterClass::St => todo!("fpreg"), // We need to do shenanigans to move between arbitrary st slots
                    X86RegisterClass::Tmm => todo!("tmmreg"), // Less shenanigans are needed here, but we need to spill to memory
                    _ => {
                        let opcode = move_opcode(dest.class(), None, false);

                        writer.write_insn(X86Instruction::new(
                            opcode,
                            vec![X86Operand::Register(*dest), X86Operand::Register(*src)],
                        ))
                    }
                }
            }
            (X86ValLocation::Null, X86ValLocation::Register(_))
            | (X86ValLocation::Register(_), X86ValLocation::Null) => {
                panic!("Cannot move a register to/from null")
            }
        }
    }
}

impl Machine for X86Machine {
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
        let mut clobbers = X86Clobbers {};
        for (num, insn) in insns.iter().enumerate() {
            match insn {
                xlang_backend::ssa::SsaInstruction::Call(targ, locations) => {
                    let callconv = X86CallConvInfo {
                        mode: assignments.mode,
                    };

                    let callconv = compute_call_conv(&callconv, &targ.real_ty, &targ.call_ty, tys);

                    // TODO: Clobber non-saved registers

                    for (loc, call_loc) in locations.params.iter().zip(callconv.params()) {
                        match call_loc {
                            CallConvLocation::Null => {}
                            CallConvLocation::Register(reg) => {
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

                    if let Some(ret) = &locations.ret {
                        match callconv.ret_location() {
                            CallConvLocation::Null => {}
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

                    for (old_loc, new_loc) in old_locs.iter().zip(foreign_locs) {
                        match (
                            assignments
                                .assigns
                                .get_mut(&new_loc.num)
                                .map(|x| x.foreign_location.clone()),
                            assignments.assigns.get_mut(&old_loc.num),
                        ) {
                            (Some(foreign), Some(old)) => {
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

                                assignments.assigns.insert(
                                    new_loc.num,
                                    LocationAssignment {
                                        foreign_location: incoming,
                                        owning_bb: *targ,
                                        change_owner: vec![],
                                    },
                                );
                            }
                            (None, None) => {
                                todo!("Allocate register");
                            }
                        }
                    }
                }
                xlang_backend::ssa::SsaInstruction::Exit(val) => match &**val {
                    [] => {}
                    [retval] => {
                        let reg = assignments.return_reg;
                        if let Some(reg) = reg {
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
                xlang_backend::ssa::SsaInstruction::LoadImmediate(_, _) => {}
            }
        }
        clobbers
    }

    fn codegen_block<W: arch_ops::traits::InsnWrite, F: Fn(u32) -> std::prelude::v1::String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        out: &mut W,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(out, assignments.mode);
        let mut cur_locations = HashMap::<_, _>::new();
        for Pair(addr, loc) in &assignments.assigns {
            if loc.owning_bb == which {
                cur_locations.insert(*addr, loc.foreign_location.clone());
            }
        }
        for (num, insn) in insns.iter().enumerate() {
            for Pair(addr, loc) in &assignments.assigns {
                if loc.owning_bb == which {
                    for (at, new_loc) in &loc.change_owner {
                        if *at == num {
                            self.write_move(&mut encoder, new_loc, &cur_locations[addr])?;
                            break;
                        }
                    }
                }
            }

            match insn {
                xlang_backend::ssa::SsaInstruction::Call(targ, _) => match &targ.ptr {
                    OpaquePtr::Symbol(sym) => encoder.write_insn(X86Instruction::new(
                        X86CodegenOpcode::Call,
                        vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                    ))?,
                    OpaquePtr::Pointer(ptr) => todo!("indirect call"),
                },
                xlang_backend::ssa::SsaInstruction::Jump(targ, _) => {
                    encoder.write_insn(X86Instruction::new(
                        X86CodegenOpcode::Jmp,
                        vec![X86Operand::RelOffset(Address::Symbol {
                            name: label_sym(*targ),
                            disp: 0,
                        })],
                    ))?;
                }
                xlang_backend::ssa::SsaInstruction::Fallthrough(_, _) => {}
                xlang_backend::ssa::SsaInstruction::Exit(val) => {
                    if assignments.stack_width > 0 {
                        encoder.write_insn(X86Instruction::new(
                            X86CodegenOpcode::Add,
                            vec![
                                X86Operand::Register(assignments.sp),
                                X86Operand::Immediate(assignments.stack_width as i64),
                            ],
                        ))?;
                    }
                    encoder.write_insn(X86Instruction::Ret)?;
                }
                xlang_backend::ssa::SsaInstruction::Tailcall(targ, _) => match &targ.ptr {
                    OpaquePtr::Symbol(sym) => encoder.write_insn(X86Instruction::new(
                        X86CodegenOpcode::Jmp,
                        vec![X86Operand::RelOffset(Address::PltSym { name: sym.clone() })],
                    ))?,
                    OpaquePtr::Pointer(ptr) => todo!("indirect call"),
                },
                xlang_backend::ssa::SsaInstruction::Trap(Trap::Breakpoint) => {
                    encoder.write_insn(X86Instruction::Int3)?
                }
                xlang_backend::ssa::SsaInstruction::Trap(_) => {
                    encoder.write_insn(X86Instruction::Ud2)?
                }
                xlang_backend::ssa::SsaInstruction::LoadImmediate(loc, val) => {
                    if let Some(location) = cur_locations.get(&loc.num) {
                        match location {
                            X86ValLocation::Register(reg) => {
                                let mov = move_opcode(reg.class(), None, true);
                                encoder.write_insn(X86Instruction::new(
                                    mov,
                                    vec![
                                        X86Operand::Register(*reg),
                                        X86Operand::Immediate((*val) as i64),
                                    ],
                                ))?;
                            }
                            X86ValLocation::Null => {}
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn codegen_prologue<W: arch_ops::traits::InsnWrite>(
        &self,
        assignments: &Self::Assignments,
        out: &mut W,
    ) -> std::io::Result<()> {
        if assignments.stack_width != 0 {
            let mut encoder = X86Encoder::new(out, assignments.mode);
            encoder.write_insn(X86Instruction::new(
                X86CodegenOpcode::Sub,
                vec![
                    X86Operand::Register(assignments.sp),
                    X86Operand::Immediate(-assignments.stack_width as i64),
                ],
            ))?;
        }

        Ok(())
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
