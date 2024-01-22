use std::{
    cell::RefCell, collections::HashSet, convert::TryFrom, ops::Deref, rc::Rc, str::FromStr,
};

use arch_ops::{
    clever::{
        CleverEncoder, CleverExtension, CleverImmediate, CleverIndex, CleverInstruction,
        CleverOpcode, CleverOperand, CleverRegister,
    },
    traits::{Address, InsnWrite},
};
use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use target_tuples::{Architecture, Target};
use xlang::{
    abi::{
        option::Option::{None as XLangNone, Some as XLangSome},
        span::Span,
        string::StringView,
    },
    plugin::{OutputMode, XLangCodegen, XLangPlugin},
    prelude::v1::{DynBox, HashMap, Pair},
    targets::properties::{MachineProperties, TargetProperties},
};
use xlang_backend::{
    callconv::CallingConvention, expr::ValLocation, mangle::mangle_itanium, str::StringMap,
    ty::TypeInformation, FunctionCodegen, FunctionRawCodegen,
};
use xlang_struct::{
    AccessClass, AggregateDefinition, BinaryOp, FloatFormat, FnType, FunctionDeclaration,
    PathComponent, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverValLocation {
    Register {
        size: u16,
        reg: CleverRegister,
    },
    Indirect {
        size: u16,
        reg: CleverRegister,
        offset: i64,
    },
    Regs {
        size: u64,
        regs: Vec<CleverRegister>,
    },
    Null,
    Unassigned(usize),
}

fn gcd(a: i64, b: i64) -> i64 {
    if a == 0 {
        b
    } else if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

impl ValLocation for CleverValLocation {
    fn addressible(&self) -> bool {
        matches!(self, Self::Indirect { .. })
    }

    fn unassigned(n: usize) -> Self {
        Self::Unassigned(n)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CleverInstructionOrLabel {
    Insn(CleverInstruction),
    Label(String),
    FunctionEpilogue,
}

impl From<CleverInstruction> for CleverInstructionOrLabel {
    fn from(insn: CleverInstruction) -> Self {
        Self::Insn(insn)
    }
}

#[derive(Debug, Clone)]
pub struct TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum RegisterStatus {
    Free,
    ToClobber,
    MustSave,
    InUse,
    Saved {
        loc: CleverValLocation,
        next: Box<Self>,
    },
}

pub struct CleverCallConv {
    features: HashSet<CleverExtension>,
    tys: Rc<TypeInformation>,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeClass {
    Float,
    Vec,
    Integer,
    Memory,
    Zero,
}

#[allow(clippy::missing_panics_doc)] // TODO: remove todo!()
#[must_use]
pub fn classify_type(ty: &Type) -> Option<TypeClass> {
    match ty {
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    vectorsize: XLangSome(1..=65535),
                    ..
                },
            ..
        }) => Some(TypeClass::Vec),
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::Float { .. } | ScalarTypeKind::Posit,
            ..
        }) => Some(TypeClass::Float),
        Type::Scalar(_) | Type::Pointer(_) => Some(TypeClass::Integer),
        Type::Void | Type::FnType(_) | Type::Null => None,
        Type::Array(ty) => classify_type(&ty.ty),
        Type::TaggedType(_, ty) => classify_type(ty),
        Type::Product(tys) => {
            let mut infected = TypeClass::Zero;
            for ty in tys {
                infected = match (classify_type(ty)?, infected) {
                    (a, TypeClass::Zero) => a,
                    (_, TypeClass::Memory) => TypeClass::Memory,
                    (a, b) if a == b => a,
                    _ => TypeClass::Memory,
                };
            }
            Some(infected)
        }
        Type::Aligned(_, _) => todo!(),
        Type::Aggregate(AggregateDefinition { fields, .. }) => {
            let mut infected = TypeClass::Zero;
            for ty in fields.iter().map(|field| &field.ty) {
                infected = match (classify_type(ty)?, infected) {
                    (a, TypeClass::Zero) => a,
                    (_, TypeClass::Memory) => TypeClass::Memory,
                    (a, b) if a == b => a,
                    _ => TypeClass::Memory,
                };
            }
            Some(infected)
        }
        Type::Named(path) => todo!("named type {:?}", path),
    }
}

impl CallingConvention for CleverCallConv {
    type Loc = CleverValLocation;

    fn pass_return_place(&self, ty: &xlang_struct::Type) -> Option<Self::Loc> {
        match (classify_type(ty)?, self.tys.type_size(ty)?) {
            (TypeClass::Memory, _) => todo!("memory"),
            (_, 0..=8) => None,
            _ => todo!("Oversized values"),
        }
    }

    fn find_param(
        &self,
        fnty: &xlang_struct::FnType,
        real: &xlang_struct::FnType,
        param: u32,
        infn: bool,
    ) -> Self::Loc {
        let mut int_regs = &[
            CleverRegister::r2,
            CleverRegister::r1,
            CleverRegister::r3,
            CleverRegister::r4,
            CleverRegister::r5,
            CleverRegister::r9,
            CleverRegister::r10,
            CleverRegister::r11,
        ][..];
        let mut float_regs = &[
            CleverRegister::f0,
            CleverRegister::f1,
            CleverRegister::f2,
            CleverRegister::f3,
        ][..];

        let mut last_loc = CleverValLocation::Unassigned(0);
        if self.pass_return_place(&fnty.ret).is_some() {
            int_regs = &int_regs[1..];
        }
        eprintln!("Finding param {} of {}", param, fnty);
        for (i, ty) in fnty
            .params
            .iter()
            .chain(core::iter::repeat(&Type::Void))
            .take((param as usize) + 1)
            .enumerate()
        {
            eprintln!("Classifying type {}", ty);
            match (classify_type(ty).unwrap(), self.tys.type_size(ty).unwrap()) {
                (TypeClass::Zero, _) => last_loc = CleverValLocation::Null,
                (TypeClass::Memory, _) => panic!("Memory value"),
                (TypeClass::Float, size @ (2 | 4 | 8)) => {
                    let reg = float_regs[0];
                    float_regs = &float_regs[1..];
                    last_loc = CleverValLocation::Register {
                        size: (size * 8) as u16,
                        reg,
                    };
                }
                (TypeClass::Integer, size @ 1..=8) | (TypeClass::Float, size @ 1) => {
                    let reg = int_regs[0];
                    int_regs = &int_regs[1..];
                    let size = size.next_power_of_two() * 8;
                    last_loc = CleverValLocation::Register {
                        size: size as u16,
                        reg,
                    };
                }
                (TypeClass::Integer | TypeClass::Float, size @ 9..=16) => {
                    let regs = int_regs[1..2].to_vec();
                    int_regs = &int_regs[2..];
                    last_loc = CleverValLocation::Regs { size, regs };
                }

                (_, _) => todo!("Memory value"),
            }

            if i == (param as usize) {
                break;
            }
        }

        last_loc
    }

    fn find_return_val(&self, fnty: &xlang_struct::FnType) -> Self::Loc {
        match (
            classify_type(&fnty.ret).unwrap(),
            self.tys.type_size(&fnty.ret).unwrap(),
        ) {
            (TypeClass::Memory, _) => todo!("memory value"),
            (_, 0) => CleverValLocation::Null,
            (TypeClass::Integer, size @ 1..=8) => {
                let size = size.next_power_of_two() * 8;
                CleverValLocation::Register {
                    size: size as u16,
                    reg: CleverRegister::r0,
                }
            }
            (TypeClass::Float, size @ (2 | 4 | 8)) => {
                let size = size * 8;
                CleverValLocation::Register {
                    size: size as u16,
                    reg: CleverRegister::f0,
                }
            }
            _ => todo!("memory value"),
        }
    }
}

pub struct CleverFunctionCodegen {
    insns: Vec<CleverInstructionOrLabel>,
    symbols: Vec<TempSymbol>,
    name: String,
    strings: Rc<RefCell<StringMap>>,
    frame_size: i32,
    properties: &'static TargetProperties<'static>,
    scratch_reg: Option<CleverRegister>,
    callconv: CleverCallConv,
    ptrreg: Option<CleverRegister>,
    gpr_status: HashMap<CleverRegister, RegisterStatus>,
    trap_unreachable: bool,
    features: HashSet<CleverExtension>,
    tys: Rc<TypeInformation>,
}

impl FunctionRawCodegen for CleverFunctionCodegen {
    type Loc = CleverValLocation;

    type CallConv = CleverCallConv;

    fn write_trap(&mut self, trap: xlang_backend::expr::Trap) {
        match trap {
            xlang_backend::expr::Trap::Unreachable if !self.trap_unreachable => {}
            xlang_backend::expr::Trap::Breakpoint => {
                self.insns
                    .push(CleverInstructionOrLabel::Insn(CleverInstruction::new(
                        CleverOpcode::Int { i: 0 },
                        vec![],
                    )))
            }
            _ => self
                .insns
                .push(CleverInstructionOrLabel::Insn(CleverInstruction::new(
                    CleverOpcode::Und0,
                    vec![],
                ))),
        }
    }

    fn write_barrier(&mut self, acc: xlang_struct::AccessClass) {
        match acc & AccessClass::ATOMIC_MASK {
            AccessClass::AtomicRelaxed => {}
            AccessClass::AtomicAcquire => {
                let scratch = self.get_or_allocate_scratch_reg();
                self.insns
                    .push(CleverInstructionOrLabel::Insn(CleverInstruction::new(
                        CleverOpcode::MovRS { r: scratch },
                        vec![CleverOperand::Indirect {
                            size: 8,
                            base: CleverRegister::r7,
                            scale: 1,
                            index: CleverIndex::Abs(0),
                        }],
                    )));
            }
            AccessClass::AtomicRelease | AccessClass::AtomicAcqRel => {
                let scratch = self.get_or_allocate_scratch_reg();
                self.insns
                    .push(CleverInstructionOrLabel::Insn(CleverInstruction::new(
                        CleverOpcode::Or {
                            lock: true,
                            flags: false,
                        },
                        vec![
                            CleverOperand::Indirect {
                                size: 8,
                                base: CleverRegister::r7,
                                scale: 1,
                                index: CleverIndex::Abs(0),
                            },
                            CleverOperand::Register {
                                size: 8,
                                reg: scratch,
                            },
                        ],
                    )));
            }
            _ => self
                .insns
                .push(CleverInstruction::new(CleverOpcode::Fence, vec![]).into()),
        }
    }

    fn write_int_binary_imm(
        &mut self,
        dest: Self::Loc,
        a: Self::Loc,
        b: u128,
        ty: &xlang_struct::Type,
        op: xlang_struct::BinaryOp,
    ) {
        todo!()
    }

    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc) {
        match (src, dest) {
            (CleverValLocation::Unassigned(_), _) | (_, CleverValLocation::Unassigned(_)) => {
                panic!("unassigned memory location")
            }
            (CleverValLocation::Regs { .. }, _) | (_, CleverValLocation::Regs { .. }) => {
                todo!("regs")
            }
            (CleverValLocation::Null, _) | (_, CleverValLocation::Null) => {}
            (
                a,
                CleverValLocation::Register {
                    size: _,
                    reg: r @ CleverRegister(0..=15),
                },
            ) => {
                let ptrreg = self.get_or_allocate_pointer_reg();
                let op1 = self.loc_to_operand(a, ptrreg).unwrap();

                self.insns
                    .push(CleverInstruction::new(CleverOpcode::MovRD { r }, vec![op1]).into());
            }
            (
                CleverValLocation::Register {
                    size: _,
                    reg: r @ CleverRegister(0..=15),
                },
                a,
            ) => {
                let ptrreg = self.get_or_allocate_pointer_reg();
                let op1 = self.loc_to_operand(a, ptrreg).unwrap();

                self.insns
                    .push(CleverInstruction::new(CleverOpcode::MovRS { r }, vec![op1]).into());
            }
            (a, b) => {
                let ptrreg = self.get_or_allocate_pointer_reg();
                let scratch = self.get_or_allocate_scratch_reg();

                let op1 = self.loc_to_operand(a, ptrreg).unwrap();

                self.insns.push(
                    CleverInstruction::new(CleverOpcode::MovRD { r: scratch }, vec![op1]).into(),
                );

                let op2 = self.loc_to_operand(b, ptrreg).unwrap();

                self.insns.push(
                    CleverInstruction::new(CleverOpcode::MovRS { r: scratch }, vec![op2]).into(),
                );
            }
        }
    }

    fn move_imm(&mut self, src: u128, dest: Self::Loc, ty: &xlang_struct::Type) {
        match (dest, self.tys.type_size(ty).unwrap()) {
            (CleverValLocation::Register { size, reg }, tysize @ 1..=8) => {
                let imm_size = (128 - (src.leading_zeros())).min((tysize * 8) as u32);
                let imm_val = if imm_size <= 12 {
                    CleverImmediate::Short(src as u16)
                } else {
                    CleverImmediate::Long((((imm_size + 15) / 16) * 16) as u16, src as u64)
                };

                if reg.0 < 16 {
                    self.insns.push(
                        CleverInstruction::new(
                            CleverOpcode::MovRD { r: reg },
                            vec![CleverOperand::Immediate(imm_val)],
                        )
                        .into(),
                    );
                } else {
                    self.insns.push(
                        CleverInstruction::new(
                            CleverOpcode::Mov,
                            vec![
                                CleverOperand::Register { size, reg },
                                CleverOperand::Immediate(imm_val),
                            ],
                        )
                        .into(),
                    );
                }
            }
            (loc, size) => todo!("move of size {} into {:?}", size, loc),
        }
    }

    fn store_indirect_imm(&mut self, src: xlang_struct::Value, ptr: Self::Loc) {
        todo!()
    }

    fn load_val(&mut self, lvalue: Self::Loc, loc: Self::Loc) {
        todo!()
    }

    fn store_indirect(&mut self, lvalue: Self::Loc, loc: Self::Loc, ty: &xlang_struct::Type) {
        todo!()
    }

    fn get_callconv(&self) -> &Self::CallConv {
        &self.callconv
    }

    fn native_int_size(&self) -> u16 {
        64
    }

    fn native_float_size(&self) -> xlang::prelude::v1::Option<u16> {
        if self.features.contains(&CleverExtension::Float) {
            XLangSome(64)
        } else {
            XLangNone
        }
    }

    fn native_vec_size(&self) -> xlang::prelude::v1::Option<u64> {
        if self.features.contains(&CleverExtension::Vec) {
            XLangSome(16)
        } else {
            XLangNone
        }
    }

    fn preferred_vec_size(&self) -> xlang::abi::option::Option<u64> {
        self.native_vec_size()
    }

    fn write_intrinsic(
        &mut self,
        name: xlang::abi::string::StringView,
        params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang_backend::expr::VStackValue<Self::Loc> {
        todo!()
    }

    fn write_target(&mut self, target: u32) {
        self.insns.push(CleverInstructionOrLabel::Label(format!(
            "{}._T{}",
            self.name, target
        )))
    }

    fn call_direct(&mut self, path: &xlang_struct::Path, _realty: &FnType) {
        let sym = match &*path.components {
            [PathComponent::Text(n)] | [PathComponent::Root, PathComponent::Text(n)] => {
                n.to_string()
            }
            [PathComponent::Root, rest @ ..] | [rest @ ..] => mangle_itanium(rest),
        };

        let addr = Address::PltSym { name: sym };

        self.insns.push(
            CleverInstruction::new(
                CleverOpcode::CallR { ss: 2 },
                vec![CleverOperand::Immediate(CleverImmediate::LongAddrRel(
                    64, addr,
                ))],
            )
            .into(),
        );
    }

    fn call_indirect(&mut self, value: Self::Loc) {
        todo!()
    }

    fn tailcall_direct(&mut self, path: &xlang_struct::Path, realty: &xlang_struct::FnType) {
        todo!()
    }

    fn tailcall_indirect(&mut self, value: Self::Loc, realty: &xlang_struct::FnType) {
        todo!()
    }

    fn leave_function(&mut self) {
        self.insns.push(CleverInstructionOrLabel::FunctionEpilogue);
    }

    fn branch(&mut self, target: u32, condition: xlang_struct::BranchCondition, val: Self::Loc) {
        todo!()
    }

    fn branch_compare(
        &mut self,
        target: u32,
        condition: xlang_struct::BranchCondition,
        v1: xlang_backend::expr::VStackValue<Self::Loc>,
        v2: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_unconditional(&mut self, target: u32) {
        todo!()
    }

    fn branch_indirect(&mut self, target: Self::Loc) {
        todo!()
    }

    fn compute_global_address(&mut self, path: &xlang_struct::Path, loc: Self::Loc) {
        todo!()
    }

    fn compute_label_address(&mut self, target: u32, loc: Self::Loc) {
        todo!()
    }

    fn compute_parameter_address(&mut self, param: u32, loc: Self::Loc) {
        todo!()
    }

    fn compute_local_address(&mut self, inloc: Self::Loc, loc: Self::Loc) {
        todo!()
    }

    fn compute_string_address(
        &mut self,
        enc: xlang_backend::str::Encoding,
        bytes: xlang::vec::Vec<u8>,
        loc: Self::Loc,
    ) {
        let addr = self.strings.borrow_mut().get_string_symbol(bytes, enc);
        let addr = Address::Symbol {
            name: addr.to_string(),
            disp: 0,
        };
        match loc {
            CleverValLocation::Register { size, reg } => match (size, reg) {
                (64, CleverRegister(0..=15)) => self.insns.push(
                    CleverInstruction::new(
                        CleverOpcode::LeaRD { r: reg },
                        vec![CleverOperand::Immediate(CleverImmediate::LongMemRel(
                            64, addr, 64,
                        ))],
                    )
                    .into(),
                ),
                (size, reg) => self.insns.push(
                    CleverInstruction::new(
                        CleverOpcode::Lea,
                        vec![
                            CleverOperand::Register { size, reg },
                            CleverOperand::Immediate(CleverImmediate::LongMemRel(64, addr, 64)),
                        ],
                    )
                    .into(),
                ),
            },
            CleverValLocation::Indirect { size, reg, offset } => todo!("indirect"),
            CleverValLocation::Regs { size, regs } => todo!("registers"),
            CleverValLocation::Null => {}
            CleverValLocation::Unassigned(_) => panic!("Unassigned"),
        }
    }

    fn free(&mut self, loc: Self::Loc) {
        todo!()
    }

    fn clobber(&mut self, loc: Self::Loc) {
        todo!()
    }

    fn allocate(&mut self, ty: &xlang_struct::Type, needs_addr: bool) -> Self::Loc {
        todo!()
    }

    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc {
        todo!()
    }

    fn prepare_call_frame(&mut self, _: &xlang_struct::FnType, _: &xlang_struct::FnType) {
        if self.frame_size & 15 != 0 {
            self.frame_size = ((self.frame_size + 15) / 16) * 16;
        }
        /* TODO */
    }

    fn lockfree_use_libatomic(&mut self, _: u64) -> bool {
        false
    }

    fn lockfree_cmpxchg_use_libatomic(&mut self, _: u64) -> bool {
        false
    }

    fn has_wait_free_compound(&mut self, op: BinaryOp, size: u64) -> bool {
        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Lsh
            | BinaryOp::Rsh => size < 128,
            _ => false,
        }
    }

    fn has_wait_free_compound_fetch(&mut self, op: BinaryOp, size: u64) -> bool {
        false
    }

    fn compare_exchange(
        &mut self,
        dest: Self::Loc,
        ctrl: Self::Loc,
        val: Self::Loc,
        ty: &Type,
        ord: AccessClass,
    ) {
        todo!()
    }

    fn weak_compare_exchange(
        &mut self,
        dest: Self::Loc,
        ctrl: Self::Loc,
        val: Self::Loc,
        ty: &Type,
        ord: AccessClass,
    ) {
        todo!()
    }

    fn call_absolute(&mut self, addr: u128, _: &FnType) {
        let addr = Address::Abs(addr);

        self.insns.push(
            CleverInstruction::new(
                CleverOpcode::CallR { ss: 2 },
                vec![CleverOperand::Immediate(CleverImmediate::LongAddrRel(
                    64, addr,
                ))],
            )
            .into(),
        );
    }

    fn write_int_binary(
        &mut self,
        _dest: Self::Loc,
        _a: Self::Loc,
        _b: Self::Loc,
        _ty: &Type,
        _op: BinaryOp,
    ) {
        todo!()
    }

    fn write_unary(&mut self, _val: Self::Loc, _ty: &Type, _op: xlang_struct::UnaryOp) {
        todo!()
    }

    fn write_asm(
        &mut self,
        asm: &xlang_struct::AsmExpr,
        inputs: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang::vec::Vec<Self::Loc> {
        todo!()
    }

    fn write_scalar_convert(
        &mut self,
        target_ty: ScalarType,
        incoming_ty: ScalarType,
        new_loc: Self::Loc,
        old_loc: Self::Loc,
    ) {
        todo!()
    }
}

impl CleverFunctionCodegen {
    fn get_or_allocate_scratch_reg(&mut self) -> CleverRegister {
        let Self {
            scratch_reg,
            gpr_status,
            ..
        } = self;
        *scratch_reg.get_or_insert_with(|| {
            for i in 0..16 {
                match gpr_status.get_or_insert_mut(CleverRegister(i), RegisterStatus::Free) {
                    RegisterStatus::Free | RegisterStatus::ToClobber => {
                        gpr_status.insert(CleverRegister(i), RegisterStatus::InUse);
                        return CleverRegister(i);
                    }
                    RegisterStatus::MustSave => {}
                    RegisterStatus::InUse => {}
                    RegisterStatus::Saved { .. } => {}
                }
            }

            todo!("Save registers")
        })
    }

    fn get_or_allocate_pointer_reg(&mut self) -> CleverRegister {
        let Self {
            ptrreg, gpr_status, ..
        } = self;
        *ptrreg.get_or_insert_with(|| {
            for i in 0..16 {
                match gpr_status.get_or_insert_mut(CleverRegister(i), RegisterStatus::Free) {
                    RegisterStatus::Free | RegisterStatus::ToClobber => {
                        gpr_status.insert(CleverRegister(i), RegisterStatus::InUse);
                        return CleverRegister(i);
                    }
                    RegisterStatus::MustSave => {}
                    RegisterStatus::InUse => {}
                    RegisterStatus::Saved { .. } => {}
                }
            }

            todo!("Save registers")
        })
    }

    fn loc_to_operand(
        &mut self,
        loc: CleverValLocation,
        sreg: CleverRegister,
    ) -> Option<CleverOperand> {
        match loc {
            CleverValLocation::Register { size, reg } => {
                Some(CleverOperand::Register { size, reg })
            }
            CleverValLocation::Indirect { size, reg, offset } => {
                let scale = gcd(offset.abs(), 128);
                let offset = offset / scale;
                let idx = if (-8..8).contains(&offset) {
                    CleverIndex::Abs(offset as i16)
                } else {
                    let size = ((((64 - (offset.leading_ones() + offset.leading_zeros())) + 15)
                        / 16)
                        * 16) as u16;

                    self.insns.push(
                        CleverInstruction::new(
                            CleverOpcode::Movsx { flags: true },
                            vec![
                                CleverOperand::Register {
                                    size: 64,
                                    reg: sreg,
                                },
                                CleverOperand::Immediate(CleverImmediate::Long(
                                    size,
                                    offset as u64,
                                )),
                            ],
                        )
                        .into(),
                    );
                    CleverIndex::Register(sreg)
                };
                Some(CleverOperand::Indirect {
                    size,
                    base: reg,
                    scale: scale as u8,
                    index: idx,
                })
            }
            CleverValLocation::Regs { .. } => None,
            CleverValLocation::Null => None,
            CleverValLocation::Unassigned(_) => panic!("unassigned location"),
        }
    }

    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
    pub fn write_output(
        self,
        text: &mut Section,
        symbols: &mut Vec<TempSymbol>,
    ) -> std::io::Result<()> {
        let mut encoder = CleverEncoder::new(text);
        if self.frame_size > 0 {
            let size = 64 - self.frame_size.leading_zeros();
            encoder.write_instruction(CleverInstruction::new(
                CleverOpcode::PushR {
                    r: CleverRegister::fbase,
                },
                vec![],
            ))?;
            encoder.write_instruction(CleverInstruction::new(
                CleverOpcode::MovRD {
                    r: CleverRegister::fbase,
                },
                vec![CleverOperand::Register {
                    size: 64,
                    reg: CleverRegister::sptr,
                }],
            ))?;
            let val = if size <= 12 {
                CleverImmediate::Short(self.frame_size as u16)
            } else {
                CleverImmediate::Long((((size + 15) / 16) * 16) as u16, self.frame_size as u64)
            };
            encoder.write_instruction(CleverInstruction::new(
                CleverOpcode::SubRD {
                    r: CleverRegister::sptr,
                },
                vec![CleverOperand::Immediate(val)],
            ))?;
        }
        for item in self.insns {
            match item {
                CleverInstructionOrLabel::Label(num) => {
                    symbols.push(TempSymbol(
                        num.to_string(),
                        Some(".text"),
                        Some(encoder.offset()),
                        SymbolType::Function,
                        SymbolKind::Local,
                    ));
                }
                CleverInstructionOrLabel::Insn(insn) => encoder.write_instruction(insn)?,
                CleverInstructionOrLabel::FunctionEpilogue => {
                    if self.frame_size > 0 {
                        encoder.write_instruction(CleverInstruction::new(
                            CleverOpcode::MovRD {
                                r: CleverRegister::sptr,
                            },
                            vec![CleverOperand::Register {
                                size: 64,
                                reg: CleverRegister::fbase,
                            }],
                        ))?;
                        encoder.write_instruction(CleverInstruction::new(
                            CleverOpcode::PopR {
                                r: CleverRegister::sptr,
                            },
                            Vec::new(),
                        ))?;
                    }
                    encoder.write_instruction(CleverInstruction::new(CleverOpcode::Ret, vec![]))?;
                }
            }
        }

        Ok(())
    }
}

pub struct CleverCodegenPlugin {
    fns: Option<std::collections::HashMap<String, FunctionCodegen<CleverFunctionCodegen>>>,
    strings: Rc<RefCell<StringMap>>,
    properties: Option<&'static TargetProperties<'static>>,
    features: HashSet<CleverExtension>,
}

impl CleverCodegenPlugin {
    fn write_output_impl<W: std::io::Write>(&mut self, mut x: W) -> std::io::Result<()> {
        let fmt = binfmt::format_by_name(&self.properties.unwrap().link.obj_binfmt).unwrap();
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

        syms.push(TempSymbol(
            "_GLOBAL_OFFSET_TABLE_".into(),
            None,
            None,
            SymbolType::Null,
            SymbolKind::Global,
        ));

        for (enc, sym, str) in self.strings.borrow().symbols() {
            let sym = TempSymbol(
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
            let sym = TempSymbol(
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

impl XLangPlugin for CleverCodegenPlugin {
    fn accept_ir(
        &mut self,
        ir: &mut xlang_struct::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        self.fns = Some(std::collections::HashMap::new());
        let properties = self.properties.unwrap();

        let mut tys = TypeInformation::from_properties(properties);

        for Pair(path, member) in &ir.root.members {
            match &member.member_decl {
                xlang_struct::MemberDeclaration::AggregateDefinition(defn) => {
                    tys.add_aggregate(path.clone(), defn.clone());
                }
                xlang_struct::MemberDeclaration::OpaqueAggregate(_) => {
                    tys.add_opaque_aggregate(path.clone());
                }
                _ => {}
            }
        }

        let tys = Rc::new(tys);

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
                    ..
                }) => {
                    let features = self.features.clone();
                    let mut state = FunctionCodegen::new(
                        CleverFunctionCodegen {
                            insns: Vec::new(),
                            symbols: Vec::new(),
                            name: name.clone(),
                            strings: self.strings.clone(),
                            properties,
                            gpr_status: HashMap::new(),
                            frame_size: 0,
                            scratch_reg: None,
                            ptrreg: None,
                            trap_unreachable: true,
                            features: features.clone(),
                            tys: tys.clone(),
                            callconv: CleverCallConv {
                                features,
                                tys: tys.clone(),
                            },
                        },
                        path.clone(),
                        ty.clone(),
                        properties,
                        tys.clone(),
                    );
                    state.write_function_body(body);
                    self.fns.as_mut().unwrap().insert(name.clone(), state);
                }
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    body: xlang::abi::option::None,
                    ..
                })
                | xlang_struct::MemberDeclaration::Scope(_)
                | xlang_struct::MemberDeclaration::Empty
                | xlang_struct::MemberDeclaration::OpaqueAggregate(_)
                | xlang_struct::MemberDeclaration::AggregateDefinition(_) => {}
                xlang_struct::MemberDeclaration::Static(_) => todo!(),
            }
        }

        xlang::abi::result::Ok(())
    }

    #[allow(clippy::needless_borrow)] // Incorrect lint
    fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
        self.properties = Some(targ);
        self.features = get_features_from_properties(targ, targ.arch.default_machine);
    }
}

fn get_features_from_properties(
    properties: &'static TargetProperties,
    machine: &'static MachineProperties,
) -> HashSet<CleverExtension> {
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
        .map(CleverExtension::from_str)
        .collect::<Result<_, _>>()
        .unwrap()
}

impl XLangCodegen for CleverCodegenPlugin {
    fn target_matches(&self, x: StringView) -> bool {
        let target: target_tuples::Target = x.parse().unwrap();

        matches!(target.arch(), Architecture::Clever)
    }

    fn write_output(
        &mut self,
        x: xlang::prelude::v1::DynMut<dyn xlang::abi::io::Write>,
        mode: OutputMode,
    ) -> xlang::abi::io::Result<()> {
        let wrapper = xlang::abi::io::WriteAdapter::new(x);
        if mode != OutputMode::Obj {
            todo!("asm output")
        }
        self.write_output_impl(wrapper).map_err(Into::into).into()
    }

    fn set_features(&mut self, features: Span<StringView>) {
        self.features = features
            .iter()
            .map(Deref::deref)
            .map(CleverExtension::from_str)
            .collect::<Result<_, _>>()
            .unwrap();
    }
}

xlang::host::rustcall! {
#[no_mangle]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::prelude::v1::Box::new(CleverCodegenPlugin {
        fns: Some(std::collections::HashMap::new()),
        strings: Rc::new(RefCell::new(StringMap::new())),
        properties: None,
        features: HashSet::new()
    }))
}}

xlang::plugin_abi_version!("0.1");
