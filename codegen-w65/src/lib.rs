use std::{
    cell::RefCell, collections::HashSet, convert::TryFrom, ops::Deref, rc::Rc, str::FromStr,
};

use arch_ops::{
    traits::{Address, InsnWrite},
    w65::{W65Address, W65Encoder, W65Instruction, W65Mode, W65Opcode, W65Operand, W65Register},
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
    AccessClass, AggregateDefinition, BinaryOp, FnType, FunctionDeclaration, PathComponent,
    ScalarType, ScalarTypeHeader, ScalarTypeKind, Type,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum W65ValLocation {
    Register {
        size: u16,
        reg: W65Register,
    },
    Indirect {
        size: u16,
        reg: W65Register,
        offset: i64,
    },
    Regs {
        size: u64,
        regs: Vec<W65Register>,
    },
    SyntheticRegister {
        size: u64,
        base: u32,
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

impl ValLocation for W65ValLocation {
    fn addressible(&self) -> bool {
        matches!(self, Self::Indirect { .. })
    }

    fn unassigned(n: usize) -> Self {
        Self::Unassigned(n)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum W65InstructionOrLabel {
    Insn(W65Instruction),
    Label(String),
    FunctionEpilogue,
    ClobberMode,
    ClearModeFlags(W65Mode),
    SetModeFlags(W65Mode),
    ResetMode(W65Mode),
    AssumeMode(W65Mode),
}

impl From<W65Instruction> for W65InstructionOrLabel {
    fn from(insn: W65Instruction) -> Self {
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
        loc: W65ValLocation,
        next: Box<Self>,
    },
}

pub struct W65CallConv {
    tys: Rc<TypeInformation>,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeClass {
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
        }) => Some(TypeClass::Memory),
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::Float { .. },
            ..
        }) => Some(TypeClass::Integer),
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
            for ty in fields.iter().map(|Pair(_, ty)| ty) {
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

impl CallingConvention for W65CallConv {
    type Loc = W65ValLocation;

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
        let mut ismallregs: &[W65Register] = &[W65Register::X, W65Register::Y];
        let mut iregs: &[u32] = &[1, 2, 3, 4, 5, 6];

        let mut last_loc = W65ValLocation::Unassigned(0);

        let param = param as usize;

        if param > real.params.len() && real.variadic {
            todo!("varargs are passed on the stack")
        }

        for (i, ty) in fnty.params.iter().enumerate() {
            match (classify_type(ty).unwrap(), self.tys.type_size(ty).unwrap()) {
                (TypeClass::Memory, _) => todo!("memory"),
                (TypeClass::Zero, _) => last_loc = W65ValLocation::Null,
                (TypeClass::Integer, size @ (1 | 2)) => {
                    if ismallregs.is_empty() {
                        if let Some((first, rest)) = iregs.split_first() {
                            iregs = rest;
                            last_loc = W65ValLocation::SyntheticRegister { size, base: *first }
                        } else {
                            todo!("stack")
                        }
                    } else {
                        let reg = ismallregs[0];
                        ismallregs = &ismallregs[1..];
                        last_loc = W65ValLocation::Register {
                            size: size as u16,
                            reg,
                        }
                    }
                }
                (TypeClass::Integer, size @ 3..=4) => {
                    if let Some((first, rest)) = iregs.split_first() {
                        iregs = rest;
                        last_loc = W65ValLocation::SyntheticRegister { size, base: *first }
                    } else {
                        todo!("stack")
                    }
                }
                (TypeClass::Integer, size @ 5..=16) => {
                    let nregs = ((size as usize) & !3) >> 2;

                    if iregs.len() < nregs {
                        todo!("stack")
                    }

                    let (l, r) = iregs.split_at(nregs);

                    iregs = r;

                    last_loc = W65ValLocation::SyntheticRegister { size, base: l[0] }
                }
                _ => todo!("oversized values"),
            }
        }

        last_loc
    }

    fn find_return_val(&self, fnty: &xlang_struct::FnType) -> Self::Loc {
        match (
            classify_type(&fnty.ret).unwrap(),
            self.tys.type_size(&fnty.ret),
        ) {
            (TypeClass::Memory, _) => todo!("memory"),
            (TypeClass::Zero, _) => W65ValLocation::Null,
            (TypeClass::Integer, Some(size @ (1 | 2))) => W65ValLocation::Register {
                size: size as u16,
                reg: W65Register::A,
            },
            (TypeClass::Integer, Some(size @ (3..=8))) => {
                W65ValLocation::SyntheticRegister { size, base: 0 }
            }
            _ => todo!("oversized values"),
        }
    }
}

impl W65CallConv {
    pub fn return_mode(&self, fnty: &xlang_struct::FnType) -> W65Mode {
        let mut mode = W65Mode::NONE;

        match (classify_type(&fnty.ret), self.tys.type_size(&fnty.ret)) {
            (Some(TypeClass::Integer), Some(1)) => mode |= W65Mode::M,
            _ => {}
        }

        mode
    }

    pub fn call_mode(&self, fnty: &xlang_struct::FnType) -> W65Mode {
        let mut mode = W65Mode::M;
        let mut hassize2 = false;
        let mut nsize12vals = 0;

        for param in &fnty.params {
            match (classify_type(param), self.tys.type_size(param)) {
                (Some(TypeClass::Integer), Some(1)) if nsize12vals != 2 => {
                    nsize12vals += 1;
                }
                (Some(TypeClass::Integer), Some(1)) if nsize12vals != 2 => {
                    nsize12vals += 1;
                }
                _ if nsize12vals == 2 => break,
                _ => continue,
            }
        }

        if !hassize2 && nsize12vals != 0 {
            mode | W65Mode::X;
        }

        mode
    }
}

pub struct W65FunctionCodegen {
    insns: Vec<W65InstructionOrLabel>,
    symbols: Vec<TempSymbol>,
    name: String,
    strings: Rc<RefCell<StringMap>>,
    frame_size: i32,
    properties: &'static TargetProperties<'static>,
    scratch_reg: Option<W65Register>,
    callconv: W65CallConv,
    ptrreg: Option<W65Register>,
    gpr_status: HashMap<W65Register, RegisterStatus>,
    trap_unreachable: bool,
    tys: Rc<TypeInformation>,
    fnty: FnType,
}

impl FunctionRawCodegen for W65FunctionCodegen {
    type Loc = W65ValLocation;

    type CallConv = W65CallConv;

    fn write_trap(&mut self, trap: xlang_backend::expr::Trap) {
        match trap {
            xlang_backend::expr::Trap::Unreachable => {}
            xlang_backend::expr::Trap::Breakpoint => {
                self.insns
                    .push(W65InstructionOrLabel::Insn(W65Instruction::new(
                        W65Opcode::Wdm,
                        W65Operand::Immediate(0x80),
                    )))
            }
            _ => {
                self.insns.push(W65Instruction::Brk.into());
                self.insns.push(W65Instruction::Stp.into());
            }
        }
    }

    fn write_barrier(&mut self, acc: xlang_struct::AccessClass) {}

    fn write_int_binary_imm(
        &mut self,
        a: Self::Loc,
        b: u128,
        ty: &xlang_struct::Type,
        op: xlang_struct::BinaryOp,
    ) {
        todo!()
    }

    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc) {
        todo!()
    }

    fn move_imm(&mut self, src: u128, dest: Self::Loc, ty: &xlang_struct::Type) {
        match dest {
            W65ValLocation::Register { size, reg } => todo!(),
            W65ValLocation::Indirect { size, reg, offset } => todo!(),
            W65ValLocation::Regs { size, regs } => todo!(),
            W65ValLocation::SyntheticRegister { size, base } => {
                if src == 0 {
                    // Special case with src=0

                    if size != 1 {
                        // we don't care about the size of the register if we're doing a 1-byte write, since the entire 4 bytes is reserved anyways, so save 3 cycles on a REP/SEP
                        self.insns
                            .push(W65InstructionOrLabel::ClearModeFlags(W65Mode::M));
                    }
                    let symname = format!("__r{}", base);
                    for i in 0..(size >> 1) {
                        let addr = Address::Symbol {
                            name: symname.clone(),
                            disp: i as i64,
                        };

                        // ABI Specifies that we can do a direct-page access to the reserved space region, so always do so to save 1 byte and 1 cycle
                        let op = W65Operand::Address(arch_ops::w65::W65Address::Direct(addr));

                        self.insns
                            .push(W65Instruction::new(W65Opcode::Stz, op).into());
                    }
                } else {
                    if size != 1 {
                        // we don't care about the size of the register if we're doing a 1-byte write, since the entire 4 bytes is reserved anyways, so save 3 cycles on a REP/SEP
                        self.insns
                            .push(W65InstructionOrLabel::ClearModeFlags(W65Mode::M));
                    }
                    let symname = format!("__r{}", base);
                    let mut val = src;
                    for i in 0..(size >> 1) {
                        let addr = Address::Symbol {
                            name: symname.clone(),
                            disp: i as i64,
                        };

                        // ABI Specifies that we can do a direct-page access to the reserved space region, so always do so to save 1 byte and 1 cycle
                        let op = W65Operand::Address(arch_ops::w65::W65Address::Direct(addr));
                        if (val & 0xFFFF) != 0 {
                            self.insns.push(
                                W65Instruction::new(
                                    W65Opcode::Lda,
                                    W65Operand::Immediate(val as u16),
                                )
                                .into(),
                            );
                            self.insns
                                .push(W65Instruction::new(W65Opcode::Sta, op).into());
                        } else {
                            self.insns
                                .push(W65Instruction::new(W65Opcode::Stz, op).into());
                        }
                    }
                }
            }
            W65ValLocation::Null => todo!(),
            W65ValLocation::Unassigned(_) => todo!(),
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
        16
    }

    fn native_float_size(&self) -> xlang::prelude::v1::Option<u16> {
        XLangNone
    }

    fn native_vec_size(&self) -> xlang::prelude::v1::Option<u64> {
        XLangNone
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
        self.insns.push(W65InstructionOrLabel::Label(format!(
            "{}._T{}",
            self.name, target
        )))
    }

    fn call_direct(&mut self, path: &xlang_struct::Path, realty: &FnType) {
        let sym = match &*path.components {
            [PathComponent::Text(n)] | [PathComponent::Root, PathComponent::Text(n)] => {
                n.to_string()
            }
            [PathComponent::Root, rest @ ..] | [rest @ ..] => mangle_itanium(rest),
        };

        let addr = Address::Symbol { name: sym, disp: 0 };

        // TODO: We can generate a JSR relaxation later

        self.insns.push(W65InstructionOrLabel::ResetMode(
            self.callconv.call_mode(realty),
        ));
        self.insns.push(
            W65Instruction::new(W65Opcode::Jsr, W65Operand::Address(W65Address::Long(addr))).into(),
        );
        self.insns.push(W65InstructionOrLabel::AssumeMode(
            self.callconv.call_mode(realty),
        ));
    }

    fn call_indirect(&mut self, value: Self::Loc) {
        todo!()
    }

    fn tailcall_direct(
        &mut self,
        value: xlang::abi::string::StringView,
        ty: &xlang_struct::FnType,
        params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn tailcall_indirect(
        &mut self,
        value: Self::Loc,
        ty: &xlang_struct::FnType,
        params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn leave_function(&mut self) {
        self.insns.push(W65InstructionOrLabel::FunctionEpilogue);
    }

    fn branch(
        &mut self,
        target: u32,
        condition: xlang_struct::BranchCondition,
        val: xlang_backend::expr::VStackValue<Self::Loc>,
    ) {
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

        todo!()
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

    fn write_block_entry_point(&mut self, n: u32) {
        todo!()
    }

    fn write_block_exit_point(&mut self, n: u32) {
        todo!()
    }

    fn write_block_exit(&mut self, n: u32) {
        todo!()
    }

    fn prepare_call_frame(&mut self, _: &xlang_struct::FnType, _: &xlang_struct::FnType) {
        if self.frame_size & 3 != 0 {
            self.frame_size = ((self.frame_size + 3) / 4) * 4;
        }
        /* TODO */
    }

    fn lockfree_use_libatomic(&mut self, _: u64) -> bool {
        false
    }

    fn lockfree_cmpxchg_use_libatomic(&mut self, _: u64) -> bool {
        true
    }

    fn has_wait_free_compound(&mut self, op: BinaryOp, size: u64) -> bool {
        false
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

        todo!()
    }

    fn write_int_binary(&mut self, _a: Self::Loc, _b: Self::Loc, _ty: &Type, _op: BinaryOp) {
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
        todo!("w65 asm")
    }
}

impl W65FunctionCodegen {
    fn loc_to_operand(&mut self, loc: W65ValLocation, sreg: W65Register) -> Option<W65Operand> {
        match loc {
            W65ValLocation::Register { size, reg } => todo!(),
            W65ValLocation::Indirect { size, reg, offset } => todo!(),
            W65ValLocation::Regs { size, regs } => todo!(),
            W65ValLocation::SyntheticRegister { size, base } => todo!(),
            W65ValLocation::Null => todo!(),
            W65ValLocation::Unassigned(_) => todo!(),
        }
    }

    #[allow(clippy::missing_errors_doc)]
    #[allow(clippy::missing_panics_doc)]
    pub fn write_output(
        self,
        text: &mut Section,
        symbols: &mut Vec<TempSymbol>,
    ) -> std::io::Result<()> {
        let mut encoder = W65Encoder::new(text);
        let mut known_mode = Some(self.callconv.call_mode(&self.fnty));
        if self.frame_size > 0 {
            todo!()
        }
        for item in self.insns {
            match item {
                W65InstructionOrLabel::ClobberMode => {
                    known_mode = None;
                }
                W65InstructionOrLabel::AssumeMode(md) => {
                    known_mode = Some(md);
                    *encoder.mode_mut() = md;
                }
                W65InstructionOrLabel::Label(num) => {
                    symbols.push(TempSymbol(
                        num.to_string(),
                        Some(".text"),
                        Some(encoder.offset()),
                        SymbolType::Function,
                        SymbolKind::Local,
                    ));
                    known_mode = None; // TODO: Be smarter about clobbering mode after a label
                }
                W65InstructionOrLabel::ResetMode(nmode) => {
                    encoder.write_insn(W65Instruction::new(
                        W65Opcode::Sep,
                        W65Operand::Immediate(nmode.bits()),
                    ))?;
                    encoder.write_insn(W65Instruction::new(
                        W65Opcode::Rep,
                        W65Operand::Immediate(((W65Mode::X | W65Mode::M) & !nmode).bits()),
                    ))?;
                    known_mode = Some(nmode);
                    *encoder.mode_mut() = nmode;
                }
                W65InstructionOrLabel::SetModeFlags(mode) => {
                    if let Some(&mut mut known_mode) = known_mode.as_mut() {
                        if (known_mode & mode) != mode {
                            encoder.write_insn(W65Instruction::new(
                                W65Opcode::Sep,
                                W65Operand::Immediate((mode & !known_mode).bits()),
                            ))?;
                        }
                        encoder.set_mode_flags(mode);
                        known_mode |= mode;
                    } else {
                        // TODO: Be smarter
                        encoder.write_insn(W65Instruction::new(
                            W65Opcode::Sep,
                            W65Operand::Immediate((W65Mode::X | W65Mode::M).bits()),
                        ))?;
                        known_mode = Some(W65Mode::X | W65Mode::M);
                        *encoder.mode_mut() = W65Mode::X | W65Mode::M;
                    }
                }
                W65InstructionOrLabel::ClearModeFlags(mode) => {
                    if let Some(&mut mut known_mode) = known_mode.as_mut() {
                        if (known_mode & !mode) != W65Mode::NONE {
                            encoder.write_insn(W65Instruction::new(
                                W65Opcode::Sep,
                                W65Operand::Immediate((mode & !known_mode).bits()),
                            ))?;
                        }
                        encoder.set_mode_flags(mode);
                        known_mode &= !mode;
                    } else {
                        // TODO: Be smarter
                        encoder.write_insn(W65Instruction::new(
                            W65Opcode::Rep,
                            W65Operand::Immediate((W65Mode::X | W65Mode::M).bits()),
                        ))?;
                        known_mode = Some(W65Mode::NONE);
                        *encoder.mode_mut() = W65Mode::NONE;
                    }
                }
                W65InstructionOrLabel::Insn(insn) => {
                    eprintln!("Codegening instruction {:?}", insn);
                    let insn = insn.into_real();
                    encoder.write_insn(insn)?
                }
                W65InstructionOrLabel::FunctionEpilogue => {
                    // TODO: Handle interrupt handlers
                    if self.frame_size > 0 {
                        todo!()
                    }
                    let mode = self.callconv.return_mode(&self.fnty);
                    if let Some(known_mode) = known_mode {
                        if (known_mode & mode) != mode {
                            encoder.write_insn(W65Instruction::new(
                                W65Opcode::Sep,
                                W65Operand::Immediate((mode & !known_mode).bits()),
                            ))?;
                        }
                        if (known_mode & !mode) != W65Mode::NONE {
                            encoder.write_insn(W65Instruction::new(
                                W65Opcode::Sep,
                                W65Operand::Immediate((mode & !known_mode).bits()),
                            ))?;
                        }
                    } else {
                        encoder.write_insn(W65Instruction::new(
                            W65Opcode::Sep,
                            W65Operand::Immediate(mode.bits()),
                        ))?;
                        encoder.write_insn(W65Instruction::new(
                            W65Opcode::Rep,
                            W65Operand::Immediate(((W65Mode::X | W65Mode::M) & !mode).bits()),
                        ))?;
                    }

                    encoder.write_insn(W65Instruction::Rtl)?;
                }
            }
        }

        Ok(())
    }
}

pub struct W65CodegenPlugin {
    fns: Option<std::collections::HashMap<String, FunctionCodegen<W65FunctionCodegen>>>,
    strings: Rc<RefCell<StringMap>>,
    properties: Option<&'static TargetProperties<'static>>,
}

impl W65CodegenPlugin {
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

impl XLangPlugin for W65CodegenPlugin {
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
                }) => {
                    let mut state = FunctionCodegen::new(
                        W65FunctionCodegen {
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
                            tys: tys.clone(),
                            callconv: W65CallConv { tys: tys.clone() },
                            fnty: ty.clone(),
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
                    ty: _,
                    body: xlang::abi::option::None,
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
    }
}

impl XLangCodegen for W65CodegenPlugin {
    fn target_matches(&self, x: StringView) -> bool {
        let target: target_tuples::Target = x.parse().unwrap();

        matches!(target.arch(), Architecture::Wc65c816)
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

    fn set_features(&mut self, _features: Span<StringView>) {}
}

xlang::host::rustcall! {
#[no_mangle]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::prelude::v1::Box::new(W65CodegenPlugin {
        fns: Some(std::collections::HashMap::new()),
        strings: Rc::new(RefCell::new(StringMap::new())),
        properties: None,
    }))
}}

xlang::plugin_abi_version!("0.1");
