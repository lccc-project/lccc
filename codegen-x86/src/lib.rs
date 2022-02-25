#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

pub mod callconv;

use std::cmp::Ordering;
use std::convert::TryInto;
use std::{
    cell::RefCell, collections::HashSet, convert::TryFrom, hash::Hash, rc::Rc, str::FromStr,
};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        features::X86Feature,
        insn::{ModRM, ModRMRegOrSib, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register, X86RegisterClass,
    },
};

use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use callconv::X86CallConv;
use target_tuples::Target;
use xlang::prelude::v1::HashMap;
use xlang::{
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox, Option as XLangOption, Pair},
    targets::properties::{MachineProperties, TargetProperties},
};
use xlang_backend::expr::{Trap, ValLocation as _};
use xlang_backend::ty::type_size;
use xlang_backend::{
    expr::{LValue, VStackValue},
    str::{Encoding, StringMap},
    FunctionCodegen, FunctionRawCodegen,
};
use xlang_struct::{
    AccessClass, BranchCondition, FnType, FunctionDeclaration, PathComponent, ScalarType,
    ScalarTypeHeader, ScalarTypeKind, ScalarValidity, Type, UnaryOp, Value,
};

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValLocation {
    BpDisp(i32),
    SpDisp(i32),
    Register(X86Register),
    Regs(Vec<X86Register>),
    ImpliedPtr(X86Register),
    /// Value in no location (for ZSTs)
    Null,
    Unassigned(usize),
}

impl ValLocation {
    fn as_modrm(&self, mode: X86Mode) -> Option<ModRM> {
        let addrclass = match mode {
            X86Mode::Real | X86Mode::Virtual8086 => X86RegisterClass::Word,
            X86Mode::Protected | X86Mode::Compatibility => X86RegisterClass::Double,
            X86Mode::Long => X86RegisterClass::Quad,
        };
        match self {
            ValLocation::BpDisp(disp) => Some(ModRM::IndirectDisp32 {
                mode: ModRMRegOrSib::Reg(X86Register::from_class(addrclass, 6).unwrap()),
                disp32: *disp,
            }),
            ValLocation::SpDisp(disp) => Some(ModRM::IndirectDisp32 {
                mode: ModRMRegOrSib::Reg(X86Register::from_class(addrclass, 5).unwrap()),
                disp32: *disp,
            }),
            ValLocation::Register(r) => Some(ModRM::Direct(*r)),
            ValLocation::Regs(_) => None,
            ValLocation::ImpliedPtr(r) => Some(ModRM::Indirect {
                mode: ModRMRegOrSib::Reg(*r),
            }),
            ValLocation::Null => Some(ModRM::Indirect {
                mode: ModRMRegOrSib::Abs(Address::Abs(1)),
            }),
            ValLocation::Unassigned(_) => panic!("Unassigned"),
        }
    }
}

impl xlang_backend::expr::ValLocation for ValLocation {
    fn addressible(&self) -> bool {
        matches!(
            self,
            Self::BpDisp(_) | Self::SpDisp(_) | Self::ImpliedPtr(_)
        )
    }

    fn unassigned(n: usize) -> Self {
        Self::Unassigned(n)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum RegisterStatus {
    Free,
    ToClobber,
    MustSave,
    InUse,
    Saved { loc: ValLocation, next: Box<Self> },
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum X86InstructionOrLabel {
    Label(String),
    Insn(X86Instruction),
    FunctionEpilogue,
}

#[derive(Debug, Clone)]
pub struct X86TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

pub struct X86CodegenState {
    insns: Vec<X86InstructionOrLabel>,
    mode: X86Mode,
    symbols: Vec<X86TempSymbol>,
    name: String,
    strings: Rc<RefCell<StringMap>>,
    callconv: std::boxed::Box<dyn X86CallConv>,
    fnty: FnType,
    frame_size: i32,
    properties: &'static TargetProperties,
    scratch_reg: Option<X86Register>,
    gpr_status: HashMap<u8, RegisterStatus>,
    _xmm_status: HashMap<u8, RegisterStatus>,
    trap_unreachable: bool,
}

impl FunctionRawCodegen for X86CodegenState {
    type Loc = ValLocation;

    type Writer = Section;

    type Error = std::io::Error;

    fn write_trap(&mut self, trap: xlang_backend::expr::Trap) {
        match trap {
            xlang_backend::expr::Trap::Unreachable if !self.trap_unreachable => {}
            xlang_backend::expr::Trap::Unreachable | xlang_backend::expr::Trap::Abort => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::Ud2)),
            xlang_backend::expr::Trap::Breakpoint => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::Int3)),

            xlang_backend::expr::Trap::Overflow => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Int,
                        vec![X86Operand::Immediate(4)],
                    )));
            }
        }
    }

    fn write_barrier(&mut self, acc: xlang_struct::AccessClass) {
        match acc & AccessClass::ATOMIC_MASK {
            val @ (AccessClass::Normal | AccessClass::AtomicRelaxed) => {
                panic!("Invalid access class {}", val);
            }
            AccessClass::AtomicAcquire | AccessClass::AtomicRelease | AccessClass::AtomicAcqRel => {
            }
            AccessClass::AtomicSeqCst => self
                .insns
                .push(X86InstructionOrLabel::Insn(X86Instruction::MFence)),
            _ => unreachable!(),
        }
    }

    fn store_val(
        &mut self,
        val: xlang_backend::expr::VStackValue<Self::Loc>,
        lvalue: xlang_backend::expr::LValue<Self::Loc>,
    ) {
        panic!("store_val {:?} {:?}", val, lvalue)
    }

    fn return_void(&mut self) {
        self.insns.push(X86InstructionOrLabel::FunctionEpilogue);
    }

    fn return_value(&mut self, val: xlang_backend::expr::VStackValue<Self::Loc>) {
        match val {
            VStackValue::Trapped => {}
            VStackValue::Constant(Value::Invalid(_)) => {
                self.write_trap(Trap::Unreachable);
            }
            val => {
                self.move_value(val, self.callconv.find_return_val(&self.fnty.ret).unwrap());
                self.insns.push(X86InstructionOrLabel::FunctionEpilogue);
            }
        }
    }

    fn write_intrinsic(
        &mut self,
        _name: xlang::abi::string::StringView,
        _params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang_backend::expr::VStackValue<Self::Loc> {
        todo!()
    }

    fn write_target(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._T{}",
            self.name, n
        )));
    }

    #[allow(clippy::match_wildcard_for_single_variants)]
    fn call_direct(
        &mut self,
        value: xlang::abi::string::StringView,
        ty: &FnType,
        params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang::prelude::v1::Option<xlang_backend::expr::VStackValue<Self::Loc>> {
        let sym = X86TempSymbol(
            value.to_string(),
            None,
            None,
            SymbolType::Null,
            SymbolKind::Global,
        );
        self.symbols.push(sym);
        let call_addr = Address::PltSym {
            name: value.to_string(),
        };
        let cc = self.callconv.with_tag(ty.tag).unwrap();
        for (i, val) in params.into_iter().enumerate() {
            self.move_value(val, cc.find_parameter(i.try_into().unwrap(), ty));
        }

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Call,
                vec![X86Operand::RelAddr(call_addr)],
            )));

        let retval = cc.find_return_val(&ty.ret);
        match &ty.ret {
            Type::Void | Type::Null => XLangOption::None,
            ty @ Type::Product(_) => {
                XLangOption::Some(VStackValue::OpaqueAggregate(ty.clone(), retval.unwrap()))
            }
            Type::Scalar(
                ty @ ScalarType {
                    header:
                        ScalarTypeHeader {
                            bitsize: 0,
                            validity,
                            ..
                        },
                    kind: ScalarTypeKind::Integer { .. },
                },
            ) if validity.contains(ScalarValidity::NONZERO) => {
                // Special case uint nonzero(0)
                XLangOption::Some(VStackValue::Constant(Value::Invalid(Type::Scalar(*ty))))
            }
            Type::Scalar(ty) => XLangOption::Some(VStackValue::OpaqueScalar(*ty, retval.unwrap())),
            Type::Pointer(pty) => XLangOption::Some(VStackValue::Pointer(
                pty.clone(),
                LValue::OpaquePointer(retval.unwrap()),
            )),
            ty => todo!("{:?}", ty),
        }
    }

    fn call_indirect(
        &mut self,
        _value: Self::Loc,
        _ty: &FnType,
        _params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang::prelude::v1::Option<xlang_backend::expr::VStackValue<Self::Loc>> {
        todo!()
    }

    fn branch(
        &mut self,
        _target: u32,
        _condition: BranchCondition,
        _val: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_compare(
        &mut self,
        _target: u32,
        _condition: BranchCondition,
        _v1: xlang_backend::expr::VStackValue<Self::Loc>,
        _v2: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_unconditional(&mut self, n: u32) {
        let addr = Address::Symbol {
            name: format!("{}._T{}", self.name, n),
            disp: 0,
        };
        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Jmp,
                vec![X86Operand::RelAddr(addr)],
            )));
    }

    fn branch_indirect(&mut self, target: Self::Loc) {
        match target {
            ValLocation::BpDisp(disp) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::IndirectDisp32 {
                            mode: ModRMRegOrSib::Reg(X86Register::Rbp),
                            disp32: disp,
                        })],
                    )));
            }
            ValLocation::SpDisp(disp) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::IndirectDisp32 {
                            mode: ModRMRegOrSib::Reg(X86Register::Rsp),
                            disp32: disp,
                        })],
                    )));
            }
            ValLocation::ImpliedPtr(p) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::ModRM(ModRM::Indirect {
                            mode: ModRMRegOrSib::Reg(p),
                        })],
                    )));
            }
            ValLocation::Register(r) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::JmpInd,
                        vec![X86Operand::Register(r)],
                    )));
            }
            ValLocation::Regs(r) => todo!("regs{:?}", r),
            ValLocation::Null => self.write_trap(Trap::Unreachable),
            ValLocation::Unassigned(_) => unreachable!("Unassigned Memory Location"),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn move_value(&mut self, val: xlang_backend::expr::VStackValue<Self::Loc>, loc: Self::Loc) {
        match val {
            VStackValue::Constant(val) => match val {
                xlang_struct::Value::Invalid(_) => {
                    self.write_trap(Trap::Unreachable);
                }
                xlang_struct::Value::Uninitialized(_) => {}
                xlang_struct::Value::GenericParameter(n) => todo!("param %{}", n),
                xlang_struct::Value::Integer { ty: _, val } => match loc {
                    ValLocation::BpDisp(disp) => todo!("[bp{:+}]", disp),
                    ValLocation::SpDisp(disp) => todo!("[sp{:+}]", disp),
                    ValLocation::Register(r) => {
                        if val == 0 {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::XorRM,
                                    vec![
                                        X86Operand::Register(r),
                                        X86Operand::ModRM(ModRM::Direct(r)),
                                    ],
                                )));
                        } else {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::MovImm,
                                    vec![
                                        X86Operand::Register(r),
                                        X86Operand::Immediate(u64::try_from(val).unwrap()),
                                    ],
                                )));
                        }
                    }
                    _ => todo!(),
                },
                xlang_struct::Value::GlobalAddress { ty: _, item: _ } => todo!(),
                xlang_struct::Value::ByteString { content } => {
                    let sym = self
                        .strings
                        .borrow_mut()
                        .get_string_symbol(content, Encoding::Byte);
                    let addr = Address::Symbol {
                        name: sym.to_string(),
                        disp: 0,
                    };
                    match loc {
                        ValLocation::Register(r) => {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::Lea,
                                    vec![
                                        X86Operand::Register(r),
                                        X86Operand::ModRM(ModRM::Indirect {
                                            mode: ModRMRegOrSib::RipRel(addr),
                                        }),
                                    ],
                                )));
                        }
                        _ => todo!(),
                    }
                }
                xlang_struct::Value::String {
                    encoding,
                    utf8,
                    ty: _,
                } => {
                    let sym = self
                        .strings
                        .borrow_mut()
                        .get_string_symbol(utf8.into_bytes(), Encoding::XLang(encoding));
                    let addr = Address::Symbol {
                        name: sym.to_string(),
                        disp: 0,
                    };
                    match loc {
                        ValLocation::Register(r) => {
                            self.insns
                                .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                    X86Opcode::Lea,
                                    vec![
                                        X86Operand::Register(r),
                                        X86Operand::ModRM(ModRM::Indirect {
                                            mode: ModRMRegOrSib::RipRel(addr),
                                        }),
                                    ],
                                )));
                        }
                        loc => todo!("move_val({:?})", loc),
                    }
                }
            },
            VStackValue::Pointer(_, LValue::StringLiteral(encoding, utf8)) => {
                let sym = self.strings.borrow_mut().get_string_symbol(utf8, encoding);
                let addr = Address::Symbol {
                    name: sym.to_string(),
                    disp: 0,
                };
                match loc {
                    ValLocation::Register(r) => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::Lea,
                                vec![
                                    X86Operand::Register(r),
                                    X86Operand::ModRM(ModRM::Indirect {
                                        mode: ModRMRegOrSib::RipRel(addr),
                                    }),
                                ],
                            )));
                    }
                    loc => todo!("move_val({:?})", loc),
                }
            }
            VStackValue::OpaqueScalar(_, l2) => match (loc, l2) {
                (_, ValLocation::Null) | (ValLocation::Null, _) => {}
                (ValLocation::Unassigned(_), _) | (_, ValLocation::Unassigned(_)) => {
                    panic!("Unassigned location");
                }
                (ValLocation::Register(r1), ValLocation::Register(r2)) => {
                    if !(r1 == r2) {
                        match r1.class().size(self.mode).cmp(&r2.class().size(self.mode)) {
                            Ordering::Equal => {
                                if r1.class().size(self.mode) == 1 {
                                    self.insns.push(X86InstructionOrLabel::Insn(
                                        X86Instruction::new(
                                            X86Opcode::MovRM8,
                                            vec![
                                                X86Operand::Register(r1),
                                                X86Operand::ModRM(ModRM::Direct(r2)),
                                            ],
                                        ),
                                    ));
                                } else {
                                    self.insns.push(X86InstructionOrLabel::Insn(
                                        X86Instruction::new(
                                            X86Opcode::MovRM,
                                            vec![
                                                X86Operand::Register(r1),
                                                X86Operand::ModRM(ModRM::Direct(r2)),
                                            ],
                                        ),
                                    ));
                                }
                            }
                            _ => todo!(),
                        }
                    }
                }
                (ValLocation::Register(r), x) if x.addressible() => {
                    let modrm = x.as_modrm(self.mode).unwrap();
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovMR,
                            vec![X86Operand::ModRM(modrm), X86Operand::Register(r)],
                        )));
                }
                (x, ValLocation::Register(r)) if x.addressible() => {
                    let modrm = x.as_modrm(self.mode).unwrap();
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovRM,
                            vec![X86Operand::Register(r), X86Operand::ModRM(modrm)],
                        )));
                }
                (x, y) if x.addressible() && y.addressible() => {
                    let modrm1 = x.as_modrm(self.mode).unwrap();
                    let modrm2 = y.as_modrm(self.mode).unwrap();
                    let r = self.get_or_allocate_scratch_reg();

                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovRM,
                            vec![X86Operand::Register(r), X86Operand::ModRM(modrm2)],
                        )));
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::MovMR,
                            vec![X86Operand::ModRM(modrm1), X86Operand::Register(r)],
                        )));
                }
                (l1, l2) => todo!("move_val({:?},{:?})", l1, l2),
            },
            VStackValue::Trapped => {}
            VStackValue::AggregatePieced(_ty, _pieces) => match loc {
                ValLocation::BpDisp(i) => todo!("[bp{:+#x}]", i),
                ValLocation::SpDisp(i) => todo!("[sp{:+#x}]", i),
                ValLocation::Register(r) => todo!("{}", r),
                ValLocation::Regs(regs) => todo!("{:?}", regs),
                ValLocation::ImpliedPtr(r) => todo!("[{}]", r),
                ValLocation::Null => {}
                ValLocation::Unassigned(n) => panic!("unassigned({})", n),
            },
            v @ (VStackValue::OpaqueAggregate(_, _)
            | VStackValue::CompareResult(_, _)
            | VStackValue::LValue(_, _)
            | VStackValue::Pointer(_, _)) => {
                todo!("{:?}", v)
            }
        }
    }

    fn compute_global_address(&mut self, path: &xlang_struct::Path, loc: Self::Loc) {
        let addr = self.get_global_address(path);
        self.load_address(addr, loc);
    }

    fn compute_label_address(&mut self, target: u32, loc: Self::Loc) {
        let addr = Address::Symbol {
            name: format!("{}._T{}", self.name, target),
            disp: 0,
        };
        self.load_address(addr, loc);
    }

    fn compute_parameter_address(&mut self, _param: u32, _loc: Self::Loc) {
        todo!()
    }

    fn compute_local_address(&mut self, _inloc: Self::Loc, _loc: Self::Loc) {
        todo!()
    }

    fn compute_string_address(
        &mut self,
        _enc: xlang_backend::str::Encoding,
        _bytes: xlang::vec::Vec<u8>,
        _loc: Self::Loc,
    ) {
        todo!()
    }

    fn free(&mut self, _loc: Self::Loc) {
        todo!()
    }

    fn clobber(&mut self, _loc: Self::Loc) {
        todo!()
    }

    fn allocate(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc {
        let size = type_size(ty, self.properties).unwrap();
        if !(size > 8 || needs_addr) {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class = X86RegisterClass::gpr_size(
                    size.next_power_of_two().try_into().unwrap(),
                    self.mode,
                )
                .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        return ValLocation::Register(reg);
                    }
                    _ => continue,
                }
            }
        }
        let offset = self.frame_size;

        self.frame_size += i32::try_from(size).unwrap(); // A pointer could be 16, 32, or 64 bits
                                                         // TODO: Alignment
        ValLocation::BpDisp(offset)
    }

    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc {
        if !needs_addr {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class =
                    X86RegisterClass::gpr_size((self.properties.ptrbits / 8) as usize, self.mode)
                        .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        return ValLocation::Register(reg);
                    }
                    _ => continue,
                }
            }
        }
        let offset = self.frame_size;

        self.frame_size += i32::from(self.properties.ptrbits / 8); // A pointer could be 16, 32, or 64 bits
                                                                   // TODO: Alignment
        ValLocation::BpDisp(offset)
    }

    fn write_binary_op(
        &mut self,
        _v1: xlang_backend::expr::VStackValue<Self::Loc>,
        _v2: xlang_backend::expr::VStackValue<Self::Loc>,
        _op: xlang_struct::BinaryOp,
        _out: Self::Loc,
    ) {
        todo!()
    }

    fn write_unary_op(
        &mut self,
        _v1: xlang_backend::expr::VStackValue<Self::Loc>,
        _op: UnaryOp,
        _out: Self::Loc,
    ) {
        todo!()
    }

    fn write_scalar_cast(
        &mut self,
        _v1: xlang_backend::expr::VStackValue<Self::Loc>,
        _ty: &ScalarType,
        _out: Self::Loc,
    ) {
        todo!()
    }

    fn write_block_entry_point(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._BE{}",
            self.name, n
        )));
    }

    fn write_block_exit_point(&mut self, n: u32) {
        self.insns.push(X86InstructionOrLabel::Label(format!(
            "{}._BX{}",
            self.name, n
        )));
    }

    fn write_block_exit(&mut self, n: u32) {
        let addr = Address::Symbol {
            name: format!("{}._BX{}", self.name, n),
            disp: 0,
        };

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Jmp,
                vec![X86Operand::RelAddr(addr)],
            )));
    }

    fn tailcall_direct(
        &mut self,
        value: xlang::abi::string::StringView,
        ty: &FnType,
        params: xlang::vec::Vec<VStackValue<Self::Loc>>,
    ) {
        let sym = X86TempSymbol(
            value.to_string(),
            None,
            None,
            SymbolType::Null,
            SymbolKind::Global,
        );
        self.symbols.push(sym);
        let call_addr = Address::PltSym {
            name: value.to_string(),
        };
        let cc = self.callconv.with_tag(ty.tag).unwrap();
        for (i, val) in params.into_iter().enumerate() {
            self.move_value(val, cc.find_parameter(i.try_into().unwrap(), ty));
        }

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::Leave));

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Jmp,
                vec![X86Operand::RelAddr(call_addr)],
            )));
    }

    fn tailcall_indirect(
        &mut self,
        _value: Self::Loc,
        _ty: &FnType,
        _params: xlang::vec::Vec<VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn load_val(&mut self, lvalue: LValue<Self::Loc>, loc: Self::Loc) {
        todo!("load_val {:?} {:?}", lvalue, loc)
    }
}

impl X86CodegenState {
    #[allow(clippy::unused_self)] // Will be needed for mangling when MSVC mangling is supported
    fn get_global_address(&self, path: &xlang::ir::Path) -> Address {
        match &*path.components {
            [PathComponent::Text(name)] | [PathComponent::Root, PathComponent::Text(name)] => {
                Address::PltSym {
                    name: name.to_string(),
                }
            }
            [..] => todo!(),
        }
    }

    fn load_address(&mut self, addr: Address, loc: ValLocation) {
        match loc {
            ValLocation::Register(r) => {
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Lea,
                        vec![
                            X86Operand::Register(r),
                            X86Operand::ModRM(ModRM::Indirect {
                                mode: ModRMRegOrSib::RipRel(addr),
                            }),
                        ],
                    )));
            }
            loc => {
                let reg = self.get_or_allocate_scratch_reg();
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::Lea,
                        vec![
                            X86Operand::Register(reg),
                            X86Operand::ModRM(ModRM::Indirect {
                                mode: ModRMRegOrSib::RipRel(addr),
                            }),
                        ],
                    )));
                self.insns
                    .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                        X86Opcode::MovRM,
                        vec![
                            X86Operand::ModRM(loc.as_modrm(self.mode).unwrap()),
                            X86Operand::Register(reg),
                        ],
                    )));
            }
        }
    }

    fn get_or_allocate_scratch_reg(&mut self) -> X86Register {
        if let Some(reg) = self.scratch_reg {
            reg
        } else {
            for i in 0..(if self.mode == X86Mode::Long { 16 } else { 8 }) {
                let class =
                    X86RegisterClass::gpr_size((self.properties.ptrbits / 8) as usize, self.mode)
                        .unwrap();
                let mode = self.gpr_status.get_or_insert_mut(i, RegisterStatus::Free);
                match mode {
                    RegisterStatus::Free => {
                        let reg = X86Register::from_class(class, i).unwrap();
                        *mode = RegisterStatus::InUse;
                        self.scratch_reg = Some(reg);
                        return reg;
                    }
                    _ => continue,
                }
            }
            todo!()
        }
    }

    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
    pub fn write_output(
        self,
        text: &mut Section,
        symbols: &mut Vec<X86TempSymbol>,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(text, self.mode);
        if self.frame_size > 0 {
            encoder.write_insn(X86Instruction::new(
                X86Opcode::Push,
                vec![X86Operand::Register(X86Register::Rbp)],
            ))?;
            encoder.write_insn(X86Instruction::new(
                X86Opcode::MovMR,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rbp)),
                    X86Operand::Register(X86Register::Rsp),
                ],
            ))?;
            encoder.write_insn(X86Instruction::new(
                X86Opcode::SubImm,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rsp)),
                    X86Operand::Immediate(self.frame_size.try_into().unwrap()),
                ],
            ))?;
        }
        for item in self.insns {
            match item {
                X86InstructionOrLabel::Label(num) => {
                    symbols.push(X86TempSymbol(
                        num.to_string(),
                        Some(".text"),
                        Some(encoder.offset()),
                        SymbolType::Function,
                        SymbolKind::Local,
                    ));
                }
                X86InstructionOrLabel::Insn(insn) => encoder.write_insn(insn)?,
                X86InstructionOrLabel::FunctionEpilogue => {
                    if self.frame_size > 0 {
                        encoder.write_insn(X86Instruction::Leave)?;
                    }
                    encoder.write_insn(X86Instruction::Retn)?;
                }
            }
        }

        Ok(())
    }
}

pub struct X86CodegenPlugin {
    target: Option<Target>,
    fns: Option<std::collections::HashMap<String, FunctionCodegen<X86CodegenState>>>,
    strings: Rc<RefCell<StringMap>>,
    properties: Option<&'static TargetProperties>,
}

impl X86CodegenPlugin {
    fn write_output_impl<W: std::io::Write>(&mut self, mut x: W) -> std::io::Result<()> {
        let fmt = binfmt::def_vec_for(self.target.as_ref().unwrap());
        let mut file = fmt.create_file(FileType::Relocatable);
        let mut text = Section {
            name: String::from(".text"),
            align: 1024,
            ty: SectionType::ProgBits,
            content: Vec::new(),
            relocs: Vec::new(),
            ..Section::default()
        };

        let mut rodata = Section {
            name: String::from(".rodata"),
            align: 1024,
            ty: SectionType::ProgBits,
            content: Vec::new(),
            relocs: Vec::new(),
            ..Section::default()
        };

        let mut syms = Vec::with_capacity(16);

        syms.push(X86TempSymbol(
            "_GLOBAL_OFFSET_TABLE_".into(),
            None,
            None,
            SymbolType::Null,
            SymbolKind::Global,
        ));

        for (enc, sym, str) in self.strings.borrow().symbols() {
            let sym = X86TempSymbol(
                sym.to_string(),
                Some(".rodata"),
                Some(rodata.content.len()),
                SymbolType::Object,
                SymbolKind::Local,
            );
            rodata.content.extend_from_slice(&enc.encode_utf8(str));
            syms.push(sym);
        }

        for (name, mut output) in self.fns.take().unwrap() {
            let sym = X86TempSymbol(
                name.clone(),
                Some(".text"),
                Some(text.content.len()),
                SymbolType::Function,
                SymbolKind::Global,
            ); // TODO: internal linkage is a thing
            syms.push(sym);

            syms.extend_from_slice(&output.raw_inner().symbols);
            output.into_inner().write_output(&mut text, &mut syms)?;
        }
        file.add_section(text).unwrap();
        file.add_section(rodata).unwrap();
        for sym in syms {
            let secno = sym
                .1
                .and_then(|v| file.sections().enumerate().find(|(_, s)| &*s.name == v))
                .map(|(s, _)| u32::try_from(s).unwrap());
            let fsym = file.get_or_create_symbol(&sym.0).unwrap();
            *fsym.kind_mut() = sym.4;
            if secno.is_some() {
                *fsym.section_mut() = secno;
                *fsym.value_mut() = sym.2.map(|v| v as u128);
                *fsym.symbol_type_mut() = sym.3;
            }
        }

        fmt.write_file(&mut x, &file)?;
        Ok(())
    }
}

impl XLangPlugin for X86CodegenPlugin {
    fn accept_ir(
        &mut self,
        ir: &mut xlang_struct::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        self.fns = Some(std::collections::HashMap::new());
        for Pair(path, member) in &ir.root.members {
            let name = &*path.components;
            let name = match name {
                [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)]
                | [xlang_struct::PathComponent::Text(t)] => t.to_string(),
                [xlang_struct::PathComponent::Root, v @ ..] | [v @ ..] => {
                    xlang_backend::mangle::mangle_itanium(v)
                }
            };

            match &member.member_decl {
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty,
                    body: xlang::abi::option::Some(body),
                }) => {
                    let properties = self.properties.unwrap();
                    let features =
                        get_features_from_properties(properties, properties.arch.default_machine);
                    let mut state = FunctionCodegen::new(
                        X86CodegenState {
                            insns: Vec::new(),
                            mode: X86Mode::default_mode_for(self.target.as_ref().unwrap()).unwrap(),
                            symbols: Vec::new(),
                            name: name.clone(),
                            strings: self.strings.clone(),
                            fnty: ty.clone(),
                            callconv: callconv::get_callconv(
                                ty.tag,
                                self.target.clone().unwrap(),
                                features.clone(),
                            )
                            .unwrap(),
                            properties,
                            _xmm_status: HashMap::new(),
                            gpr_status: HashMap::new(),
                            frame_size: 0,
                            scratch_reg: None,
                            trap_unreachable: true,
                        },
                        path.clone(),
                        ty.clone(),
                        xlang::targets::properties::get_properties(
                            self.target.as_ref().map(From::from).unwrap(),
                        )
                        .unwrap(),
                    );
                    state.write_function_body(body);
                    self.fns.as_mut().unwrap().insert(name.clone(), state);
                }
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty: _,
                    body: xlang::abi::option::None,
                })
                | xlang_struct::MemberDeclaration::Scope(_)
                | xlang_struct::MemberDeclaration::Empty
                | xlang_struct::MemberDeclaration::OpaqueAggregate(_)
                | xlang_struct::MemberDeclaration::AggregateDefinition(_) => {}
            }
        }

        xlang::abi::result::Ok(())
    }

    #[allow(clippy::needless_borrow)] // Incorrect lint
    fn set_target(&mut self, targ: xlang::targets::Target) {
        self.target = Some((&targ).into());
        self.properties = xlang::targets::properties::get_properties(targ);
    }
}

fn get_features_from_properties(
    properties: &'static TargetProperties,
    machine: &'static MachineProperties,
) -> HashSet<X86Feature> {
    let mut names = HashSet::new();
    for &f in machine.default_features {
        names.insert(f);
    }
    for &Pair(name, val) in properties.enabled_features {
        if val {
            names.insert(name);
        } else {
            names.remove(&name);
        }
    }

    names
        .into_iter()
        .map(xlang::abi::string::StringView::into_str)
        .map(X86Feature::from_str)
        .collect::<Result<_, _>>()
        .unwrap()
}

impl XLangCodegen for X86CodegenPlugin {
    fn target_matches(&self, x: &xlang::targets::Target) -> bool {
        let target: target_tuples::Target = x.into();

        matches!(
            target.arch(),
            target_tuples::Architecture::I86
                | target_tuples::Architecture::I8086
                | target_tuples::Architecture::I086
                | target_tuples::Architecture::I186
                | target_tuples::Architecture::I286
                | target_tuples::Architecture::I386
                | target_tuples::Architecture::I486
                | target_tuples::Architecture::I586
                | target_tuples::Architecture::I686
                | target_tuples::Architecture::X86_64
        )
    }

    fn write_output(
        &mut self,
        x: xlang::prelude::v1::DynMut<dyn xlang::abi::io::Write>,
    ) -> xlang::abi::io::Result<()> {
        let wrapper = xlang::abi::io::WriteAdapter::new(x);

        self.write_output_impl(wrapper).map_err(Into::into).into()
    }
}

xlang::host::rustcall! {
#[no_mangle]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(Box::new(X86CodegenPlugin {
        fns: Some(std::collections::HashMap::new()),
        target: None,
        strings: Rc::new(RefCell::new(StringMap::new())),
        properties: None
    }))
}}

#[cfg(test)] // miri doesn't work because of FFI calls. This should be fixed
mod test {
    use std::{cell::RefCell, rc::Rc};

    use arch_ops::x86::insn::X86Mode;
    use binfmt::fmt::Section;
    use target_tuples::Target;
    use xlang::{abi::vec, prelude::v1::HashMap};
    use xlang_backend::{str::StringMap, FunctionCodegen};
    use xlang_struct::{
        Abi, Block, BlockItem, Expr, FunctionBody, FunctionDeclaration, Path, ScalarType,
        ScalarTypeHeader, ScalarTypeKind, Type, Value,
    };

    use crate::{callconv, X86CodegenState};

    #[test]
    fn test_return_void() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        let properties = xlang::targets::properties::get_properties((&target).into()).unwrap();
        let features =
            super::get_features_from_properties(properties, &properties.arch.default_machine);
        let name = "foo";
        let FunctionDeclaration { ty, body } = FunctionDeclaration {
            ty: xlang_struct::FnType {
                ret: Type::Void,
                params: xlang::abi::vec::Vec::new(),
                tag: Abi::C,
            },
            body: xlang::abi::option::Some(FunctionBody {
                locals: vec![],
                block: Block { items: vec![] },
            }),
        };
        let mut state = FunctionCodegen::new(
            X86CodegenState {
                insns: Vec::new(),
                mode: X86Mode::default_mode_for(&target).unwrap(),
                symbols: Vec::new(),
                name: name.to_string(),
                strings: Rc::new(RefCell::new(StringMap::new())),
                fnty: ty.clone(),
                callconv: callconv::get_callconv(ty.tag, target.clone(), features.clone()).unwrap(),
                properties,
                _xmm_status: HashMap::new(),
                gpr_status: HashMap::new(),
                frame_size: 0,
                scratch_reg: None,
                trap_unreachable: true,
            },
            Path {
                components: vec![xlang_struct::PathComponent::Text(name.into())],
            },
            ty.clone(),
            properties,
        );
        state.write_function_body(&body.as_ref().unwrap());
        let mut sec = Section {
            align: 1024,
            ..Default::default()
        };
        state
            .into_inner()
            .write_output(&mut sec, &mut Vec::new())
            .unwrap();
        assert_eq!(*sec.content, [0xC3])
    }

    #[test]
    fn test_return_value() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        let properties = xlang::targets::properties::get_properties((&target).into()).unwrap();
        let features =
            super::get_features_from_properties(properties, &properties.arch.default_machine);
        let name = "foo";
        let intty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: i32::MIN.into(),
                max: i32::MAX.into(),
            },
        };
        let FunctionDeclaration { ty, body } = FunctionDeclaration {
            ty: xlang_struct::FnType {
                ret: Type::Scalar(intty),
                params: xlang::abi::vec::Vec::new(),
                tag: Abi::C,
            },
            body: xlang::abi::option::Some(FunctionBody {
                locals: vec![],
                block: Block {
                    items: vec![
                        BlockItem::Expr(Expr::Const(Value::Integer { ty: intty, val: 0 })),
                        BlockItem::Expr(Expr::ExitBlock { blk: 0, values: 1 }),
                    ],
                },
            }),
        };
        let mut state = FunctionCodegen::new(
            X86CodegenState {
                insns: Vec::new(),
                mode: X86Mode::default_mode_for(&target).unwrap(),
                symbols: Vec::new(),
                name: name.to_string(),
                strings: Rc::new(RefCell::new(StringMap::new())),
                fnty: ty.clone(),
                callconv: callconv::get_callconv(ty.tag, target.clone(), features.clone()).unwrap(),
                properties,
                _xmm_status: HashMap::new(),
                gpr_status: HashMap::new(),
                frame_size: 0,
                scratch_reg: None,
                trap_unreachable: true,
            },
            Path {
                components: vec![xlang_struct::PathComponent::Text(name.into())],
            },
            ty.clone(),
            properties,
        );
        state.write_function_body(&body.as_ref().unwrap());
        let mut sec = Section {
            align: 1024,
            ..Default::default()
        };
        state
            .into_inner()
            .write_output(&mut sec, &mut Vec::new())
            .unwrap();
        assert_eq!(*sec.content, [0x33, 0xC0, 0xC3]);
    }

    #[test]
    fn test_invalid() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        let properties = xlang::targets::properties::get_properties((&target).into()).unwrap();
        let features =
            super::get_features_from_properties(properties, &properties.arch.default_machine);
        let name = "foo";
        let intty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: i32::MIN.into(),
                max: i32::MAX.into(),
            },
        };
        let FunctionDeclaration { ty, body } = FunctionDeclaration {
            ty: xlang_struct::FnType {
                ret: Type::Scalar(intty),
                params: xlang::abi::vec::Vec::new(),
                tag: Abi::C,
            },
            body: xlang::abi::option::Some(FunctionBody {
                locals: vec![],
                block: Block {
                    items: vec![
                        BlockItem::Expr(Expr::Const(Value::Invalid(Type::Scalar(intty)))),
                        BlockItem::Expr(Expr::ExitBlock { blk: 0, values: 1 }),
                    ],
                },
            }),
        };
        let mut state = FunctionCodegen::new(
            X86CodegenState {
                insns: Vec::new(),
                mode: X86Mode::default_mode_for(&target).unwrap(),
                symbols: Vec::new(),
                name: name.to_string(),
                strings: Rc::new(RefCell::new(StringMap::new())),
                fnty: ty.clone(),
                callconv: callconv::get_callconv(ty.tag, target.clone(), features.clone()).unwrap(),
                properties,
                _xmm_status: HashMap::new(),
                gpr_status: HashMap::new(),
                frame_size: 0,
                scratch_reg: None,
                trap_unreachable: true,
            },
            Path {
                components: vec![xlang_struct::PathComponent::Text(name.into())],
            },
            ty.clone(),
            properties,
        );
        state.write_function_body(&body.as_ref().unwrap());
        let mut sec = Section {
            align: 1024,
            ..Default::default()
        };
        state
            .into_inner()
            .write_output(&mut sec, &mut Vec::new())
            .unwrap();
        assert_eq!(*sec.content, [0x0F, 0x0B])
    }

    #[test]
    fn test_invalid_notrap() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        let properties = xlang::targets::properties::get_properties((&target).into()).unwrap();
        let features =
            super::get_features_from_properties(properties, &properties.arch.default_machine);
        let name = "foo";
        let intty = ScalarType {
            header: ScalarTypeHeader {
                bitsize: 32,
                ..Default::default()
            },
            kind: ScalarTypeKind::Integer {
                signed: true,
                min: i32::MIN.into(),
                max: i32::MAX.into(),
            },
        };
        let FunctionDeclaration { ty, body } = FunctionDeclaration {
            ty: xlang_struct::FnType {
                ret: Type::Scalar(intty),
                params: xlang::abi::vec::Vec::new(),
                tag: Abi::C,
            },
            body: xlang::abi::option::Some(FunctionBody {
                locals: vec![],
                block: Block {
                    items: vec![
                        BlockItem::Expr(Expr::Const(Value::Invalid(Type::Scalar(intty)))),
                        BlockItem::Expr(Expr::ExitBlock { blk: 0, values: 1 }),
                    ],
                },
            }),
        };
        let mut state = FunctionCodegen::new(
            X86CodegenState {
                insns: Vec::new(),
                mode: X86Mode::default_mode_for(&target).unwrap(),
                symbols: Vec::new(),
                name: name.to_string(),
                strings: Rc::new(RefCell::new(StringMap::new())),
                fnty: ty.clone(),
                callconv: callconv::get_callconv(ty.tag, target.clone(), features.clone()).unwrap(),
                properties,
                _xmm_status: HashMap::new(),
                gpr_status: HashMap::new(),
                frame_size: 0,
                scratch_reg: None,
                trap_unreachable: false,
            },
            Path {
                components: vec![xlang_struct::PathComponent::Text(name.into())],
            },
            ty.clone(),
            properties,
        );
        state.write_function_body(&body.as_ref().unwrap());
        let mut sec = Section {
            align: 1024,
            ..Default::default()
        };
        state
            .into_inner()
            .write_output(&mut sec, &mut Vec::new())
            .unwrap();
        assert_eq!(*sec.content, [])
    }
}
