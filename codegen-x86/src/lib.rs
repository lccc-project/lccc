#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

// mod callconv;

use std::{cell::RefCell, convert::TryFrom, hash::Hash, rc::Rc};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        insn::{ModRM, ModRMRegOrSib, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register,
    },
};

use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use target_tuples::Target;
use xlang::{
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox, Option as XLangOption, Pair},
    targets::properties::TargetProperties,
};
use xlang_backend::{
    expr::{LValue, VStackValue},
    str::{Encoding, StringMap},
    FunctionCodegen, FunctionRawCodegen,
};
use xlang_struct::{
    AccessClass, BranchCondition, FnType, FunctionDeclaration, ScalarType, ScalarTypeHeader, Type,
    UnaryOp,
};

#[must_use]
pub const fn get_type_size(ty: &Type, properties: &'static TargetProperties) -> Option<usize> {
    match ty {
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    bitsize,
                    vectorsize,
                    ..
                },
            ..
        }) => {
            let bits = (*bitsize).next_power_of_two();
            let bits = bits + (8 - (bits % 8)) % 8;
            let bytes = (bits / 8) as usize;
            let vec = if *vectorsize == 0 {
                1
            } else {
                *vectorsize as usize
            };
            Some(bytes * vec)
        }
        Type::Void | Type::FnType(_) => None,
        Type::Pointer(_) => Some((properties.ptrbits / 8) as usize),
    }
}

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
    StackVal { off: usize, hash: u64 },
    LocalVariable(usize),
    Saved { loc: ValLocation, next: Box<Self> },
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum X86InstructionOrLabel {
    Label(u32),
    Insn(X86Instruction),
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
}

impl FunctionRawCodegen for X86CodegenState {
    type Loc = ValLocation;

    type Writer = Section;

    type Error = std::io::Error;

    fn write_trap(&mut self, trap: xlang_backend::expr::Trap) {
        match trap {
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
        _val: xlang_backend::expr::VStackValue<Self::Loc>,
        _lvalue: xlang_backend::expr::LValue<Self::Loc>,
    ) {
        todo!()
    }

    fn return_void(&mut self) {
        todo!()
    }

    fn return_value(&mut self, val: xlang_backend::expr::VStackValue<Self::Loc>) {
        // TODO: Adjust for size
        self.move_value(val, ValLocation::Register(X86Register::Eax));
        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::Leave));
        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::Retn));
    }

    fn write_intrinsic(
        &mut self,
        _name: xlang::abi::string::StringView,
        _params: xlang::vec::Vec<xlang_backend::expr::VStackValue<Self::Loc>>,
    ) -> xlang_backend::expr::VStackValue<Self::Loc> {
        todo!()
    }

    fn write_target(&mut self, _target: u32) {
        todo!()
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
        for (i, val) in params.into_iter().enumerate() {
            match i {
                0 => self.move_value(val, ValLocation::Register(X86Register::Rdi)),
                n => todo!("cannot handle parameter {}", n),
            }
        }

        self.insns
            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                X86Opcode::Call,
                vec![X86Operand::RelAddr(call_addr)],
            )));

        match &ty.ret {
            Type::Void => XLangOption::None,
            Type::Scalar(ty) => XLangOption::Some(VStackValue::OpaqueScalar(
                *ty,
                ValLocation::Register(X86Register::Rax),
            )),
            Type::Pointer(_) => XLangOption::Some(VStackValue::Pointer(LValue::OpaquePointer(
                ValLocation::Register(X86Register::Rax),
            ))),
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

    fn branch_unconditional(&mut self, _target: u32) {
        todo!()
    }

    fn branch_indirect(&mut self, _target: Self::Loc) {
        todo!()
    }

    fn move_value(&mut self, val: xlang_backend::expr::VStackValue<Self::Loc>, loc: Self::Loc) {
        match val {
            VStackValue::Constant(val) => match val {
                xlang_struct::Value::Invalid(_) | xlang_struct::Value::Uninitialized(_) => {}
                xlang_struct::Value::GenericParameter(n) => todo!("param %{}", n),
                xlang_struct::Value::Integer { ty: _, val } => match loc {
                    ValLocation::BpDisp(disp) => todo!("[bp{:+}]", disp),
                    ValLocation::SpDisp(disp) => todo!("[sp{:+}]", disp),
                    ValLocation::Register(r) => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovImm,
                                vec![
                                    X86Operand::Register(r),
                                    X86Operand::Immediate(u64::from(val)),
                                ],
                            )));
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
                        _ => todo!(),
                    }
                }
            },
            VStackValue::LValue(_) | VStackValue::Pointer(_) | VStackValue::OpaqueScalar(_, _) => {
                todo!()
            }
            VStackValue::Trapped => {}
        }
    }

    fn compute_global_address(&mut self, _path: &xlang_struct::Path, _loc: Self::Loc) {
        todo!()
    }

    fn compute_label_address(&mut self, _target: u32, _loc: Self::Loc) {
        todo!()
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

    fn assign_value(&mut self, _ty: &Type, _needs_addr: bool) -> Self::Loc {
        todo!()
    }

    fn prepare_stack_frame(&mut self, _to_assign: &[Type]) -> xlang::prelude::v1::Option<usize> {
        todo!()
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
}

impl X86CodegenState {
    #[allow(clippy::missing_errors_doc)]
    pub fn write_output(
        self,
        text: &mut Section,
        symbols: &mut Vec<X86TempSymbol>,
    ) -> std::io::Result<()> {
        let mut encoder = X86Encoder::new(text, self.mode);
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
        for item in self.insns {
            match item {
                X86InstructionOrLabel::Label(num) => {
                    symbols.push(X86TempSymbol(
                        format!("{}._T{}", self.name, num),
                        Some(".text"),
                        Some(encoder.offset()),
                        SymbolType::Function,
                        SymbolKind::Local,
                    ));
                }
                X86InstructionOrLabel::Insn(insn) => encoder.write_insn(insn)?,
            }
        }

        Ok(())
    }
}

pub struct X86CodegenPlugin {
    target: Option<Target>,
    fns: Option<std::collections::HashMap<String, FunctionCodegen<X86CodegenState>>>,
    strings: Rc<RefCell<StringMap>>,
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
        for Pair(path, member) in &ir.root.members {
            let name = &*path.components;
            let name = match name {
                [xlang_struct::PathComponent::Text(t)]
                | [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)] => &**t,
                _ => panic!("Cannot access name component"),
            }
            .to_string();

            match &member.member_decl {
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty,
                    body: xlang::abi::option::Some(body),
                }) => {
                    let mut state = FunctionCodegen::new(
                        X86CodegenState {
                            insns: Vec::new(),
                            mode: X86Mode::default_mode_for(self.target.as_ref().unwrap()).unwrap(),
                            symbols: Vec::new(),
                            name: name.clone(),
                            strings: self.strings.clone(),
                        },
                        path.clone(),
                        ty.clone(),
                        xlang::targets::properties::get_properties(
                            self.target.as_ref().map(From::from).unwrap(),
                        )
                        .unwrap(),
                    );
                    state.write_block(body);
                    self.fns.as_mut().unwrap().insert(name.clone(), state);
                }
                xlang_struct::MemberDeclaration::Function(FunctionDeclaration {
                    ty: _,
                    body: xlang::abi::option::None,
                })
                | xlang_struct::MemberDeclaration::Scope(_)
                | xlang_struct::MemberDeclaration::Empty => {}
            }
        }

        xlang::abi::result::Ok(())
    }

    fn set_target(&mut self, targ: xlang::targets::Target) {
        self.target = Some(targ.into());
    }
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
    }))
}}
