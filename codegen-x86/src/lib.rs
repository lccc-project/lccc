#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

mod callconv;

use std::{
    cell::RefCell,
    collections::VecDeque,
    convert::TryFrom,
    hash::{Hash, Hasher},
    io::Write,
    rc::Rc,
};

use arch_ops::{
    traits::{Address, InsnWrite},
    x86::{
        features::X86Feature,
        // features::X86Feature,
        insn::{ModRM, ModRMRegOrSib, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register,
        X86RegisterClass,
    },
};

use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use xlang::{
    abi::{
        collection::{HashMap, HashSet},
        hash::XLangHasher,
    },
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox, Pair},
    targets::properties::TargetProperties,
};
use xlang_struct::{
    Block, BranchCondition, Expr, FnType, FunctionDeclaration, PointerType, ScalarType,
    ScalarTypeHeader, ScalarTypeKind, StackItem, StringEncoding, Type, UnaryOp, Value,
};

// pub fn feature_name_to_archops(name: &str) -> Option<X86Feature> {
//     match name {
//     "x87" => Some(X86Feature::X87),
//     "mmx" => Some(X86Feature::Mmx),
//     "sse" => Some(X86Feature::Sse),
//     "sse2" => Some(X86Feature::Sse2),
//     "sse3" => Some(X86Feature::Sse3),
//     "ssse3"),
//     "sse4"),
//     "sse4a"),
//     "sse4.1"),
//     "sse4.2"),
//     "avx"),
//     "avx2"),
//     "avx512f"),
//     "avx512pf"),
//     "avx512er"),
//     "avx512cd"),
//     "avx512vl"),
//     "avx512bw"),
//     "avx512dq"),
//     "avx512ifma"),
//     "avx512vbmi"),
//     "aes"),
//     "sha"),
//     "pclmul"),
//     "clflushopt"),
//     "clwb"),
//     "fsgsbase"),
//     "ptwrite"),
//     "rdrand"),
//     "f16c"),
//     "fma"),
//     "pconfig"),
//     "wbnoinvd"),
//     "fma4"),
//     "prfchw"),
//     "rdpid"),
//     "prefetchwcl"),
//     "rdseed"),
//     "sgx"),
//     "xop"),
//     "lwp"),
//     "3dnow"),
//     "3dnowa"),
//     "popcnt"),
//     "abm"),
//     "adx"),
//     "bmi"),
//     "bmi2"),
//     "lzcnt"),
//     "xsave"),
//     "xsaveopt"),
//     "xsavec"),
//     "xsaves"),
//     "rtm"),
//     "hle"),
//     "tbm"),
//     "mwaitx"),
//     "clzero"),
//     "pku"),
//     "avx512vbmi2"),
//     "avx512bf16"),
//     "avx512fp16"),
//     "gfni"),
//     "vaes"),
//     "waitpkg"),
//     "vpclmulqdq"),
//     "avx512bitalg"),
//     "movdiri"),
//     "movdir64b"),
//     "enqcmd"),
//     "uintr"),
//     "tsxldtrk"),
//     "avx512vpopcntdq"),
//     "avx512vp2intersect"),
//     "avx5124fmaps"),
//     "avx512vnni"),
//     "avxvnni"),
//     "avx5124vnniw"),
//     "cldemote"),
//     "serialize"),
//     "amx-tile"),
//     "amx-int8"),
//     "amx-bf16"),
//     "hreset"),
//     "kl"),
//     "widekl"),
//     "sahf"),
//     "cx16"),
//     "movbe"),
//     "crc32"),
//     "mwait"),
//     }
// }

pub fn get_type_size(ty: &Type, properties: &'static TargetProperties) -> Option<usize> {
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
        Type::Void => None,
        Type::FnType(_) => None,
        Type::Pointer(_) => Some((properties.ptrbits / 8) as usize),
    }
}

#[derive(Clone, Debug)]
pub struct StringInterner {
    syms: HashMap<xlang::abi::string::String, String>,
    inuse: std::collections::HashMap<u64, usize>,
}

impl StringInterner {
    #[must_use]
    pub fn new() -> Self {
        Self {
            syms: HashMap::new(),
            inuse: std::collections::HashMap::new(),
        }
    }

    pub fn get_or_insert_string(&mut self, st: xlang::abi::string::String) -> String {
        let StringInterner { syms, inuse } = self;
        syms.get_or_insert_with_mut(st, |s| {
            let mut hasher = XLangHasher::default();
            s.hash(&mut hasher);
            let val = hasher.finish();
            let suffix = match inuse.entry(val) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(0);
                    String::new()
                }
                std::collections::hash_map::Entry::Occupied(entry) => {
                    let entry = entry.into_mut();
                    *entry += 1;
                    format!(".{}", val)
                }
            };

            let str = format!("__xlang_string.{}.{:016x}{}", xlang::version(), val, suffix);
            str
        })
        .clone()
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValLocation {
    BpDisp(i32),
    SpDisp(i32),
    Register(X86Register),
    Regs(Vec<X86Register>),
    /// Value in no location (for ZSTs)
    Null,
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LValue {
    Local(usize),
    PointerConstant(Value),
    Temporary(Box<VStackValue>),
    OpaquePointer(ValLocation),
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum VStackValue {
    Constant(Value),
    LValue(LValue),
    Pointer(LValue, PointerType),
    OpaqueInt(ValLocation, ScalarType),
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
struct X86TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

#[allow(dead_code)]
struct X86CodegenState {
    vstack: VecDeque<VStackValue>,
    strmap: Rc<RefCell<StringInterner>>,
    insns: Vec<X86InstructionOrLabel>,
    rodata: Vec<u8>,
    frame_size: i32,
    symbols: Vec<X86TempSymbol>,
    locals: Vec<VStackValue>,
    signature: FnType,
    mode: X86Mode,
    symname: String,
    targets: HashMap<u32, Vec<StackItem>>,
    regs: HashMap<X86Register, RegisterStatus>,
    properties: &'static TargetProperties,
    features: HashSet<X86Feature>,
}

impl X86CodegenState {
    pub fn init(
        sig: FnType,
        mode: X86Mode,
        strmap: Rc<RefCell<StringInterner>>,
        symname: String,
        properties: &'static TargetProperties,
        features: HashSet<X86Feature>,
    ) -> Self {
        Self {
            vstack: VecDeque::new(),
            strmap,
            insns: Vec::new(),
            rodata: Vec::new(),
            frame_size: 0,
            symbols: Vec::new(),
            locals: Vec::new(),
            signature: sig,
            mode,
            symname,
            targets: HashMap::new(),
            regs: HashMap::new(),
            properties,
            features,
        }
    }

    #[allow(dead_code)]
    fn restore_register(&mut self, reg: X86Register) {
        match self.regs.get(&reg) {
            Some(RegisterStatus::Saved { loc, .. }) => match loc {
                ValLocation::Null => {}
                ValLocation::Register(r) => match reg.class() {
                    X86RegisterClass::Byte | X86RegisterClass::ByteRex => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovMR8,
                                vec![
                                    X86Operand::ModRM(ModRM::Direct(reg)),
                                    X86Operand::Register(*r),
                                ],
                            )));
                    }
                    X86RegisterClass::Word | X86RegisterClass::Double | X86RegisterClass::Quad => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovMR,
                                vec![
                                    X86Operand::ModRM(ModRM::Direct(reg)),
                                    X86Operand::Register(*r),
                                ],
                            )));
                    }
                    X86RegisterClass::Xmm | X86RegisterClass::Ymm | X86RegisterClass::Zmm => {
                        todo!("movups {}, {}", reg, r)
                    }
                    X86RegisterClass::Sreg => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovSRM,
                                vec![
                                    X86Operand::Register(reg),
                                    X86Operand::ModRM(ModRM::Direct(reg)),
                                ],
                            )))
                    }
                    _ => panic!("Cannot restore reg {}", reg),
                },
                ValLocation::BpDisp(off) => todo!("[bp{:-}]", off),
                ValLocation::SpDisp(off) => todo!("[sp{:-}]", off),
                _ => unreachable!("Not a save location for {}", reg),
            },
            _ => {}
        }
    }

    #[allow(dead_code)]
    fn save_reg(&mut self, reg: X86Register, rs: RegisterStatus) {
        match rs {
            RegisterStatus::Free => {}
            RegisterStatus::ToClobber => {} // Can already clobber this register, since we've saved it to *be* clobbered
            RegisterStatus::MustSave => {
                self.frame_size += reg.class().size(self.mode) as i32;
                let off = -self.frame_size;
                self.regs.insert(
                    reg,
                    RegisterStatus::Saved {
                        loc: ValLocation::BpDisp(off as i32),
                        next: Box::new(RegisterStatus::Free),
                    },
                );
            }
            RegisterStatus::StackVal { off, hash } => {
                if self.vstack.len() > off {
                    // Check to make sure that the thing that is claimed to be using the register actually exists
                    // Save the first n values of the vstack, we need to readd them later.
                    // This is why the vstack is a deque despite being used primarily as a stack
                    let vals = self.vstack.drain(..off).collect::<Vec<_>>();

                    let mut val = self.vstack.pop_front().unwrap();

                    let mut hasher = XLangHasher::default();
                    val.hash(&mut hasher);
                    let comp = hasher.finish();
                    if comp == hash {
                        // Slow check, to make sure the thing we need to store is actually in the register
                        match val {
                            VStackValue::OpaqueInt(ValLocation::Register(r), ty) if r == reg => {
                                self.frame_size +=
                                    get_type_size(&Type::Scalar(ty.clone()), self.properties)
                                        .unwrap() as i32;
                                let off = -self.frame_size;
                                val = self.move_value(val, ValLocation::BpDisp(off));
                            }
                            VStackValue::LValue(LValue::OpaquePointer(ValLocation::Register(
                                r,
                            )))
                            | VStackValue::Pointer(
                                LValue::OpaquePointer(ValLocation::Register(r)),
                                _,
                            ) if r == reg => {
                                self.frame_size += (self.properties.ptrbits / 8) as i32;
                                let off = -self.frame_size;
                                val = self.move_value(val, ValLocation::BpDisp(off));
                            }
                            VStackValue::LValue(LValue::Temporary(ref _inner))
                            | VStackValue::Pointer(LValue::Temporary(ref _inner), _) => {
                                todo!("temporary value: {:?}", val)
                            }

                            _ => {}
                        }
                    }
                    self.vstack.push_front(val);
                    vals.into_iter()
                        .rev()
                        .for_each(|val| self.vstack.push_front(val));
                }

                self.regs.insert(reg, RegisterStatus::Free);
            }
            RegisterStatus::LocalVariable(_) => todo!(),
            RegisterStatus::Saved { loc: _, next } => self.save_reg(reg, Box::into_inner(next)),
        }
    }

    #[allow(dead_code)]
    fn clobber_register(&mut self, reg: X86Register) {
        if let Some(status) = self.regs.get(&reg).cloned() {
            self.save_reg(reg, status);
        }
    }

    #[allow(dead_code)]
    fn allocate_register(&mut self, _class: X86RegisterClass) -> X86Register {
        todo!()
    }

    fn move_value(&mut self, val: VStackValue, target_loc: ValLocation) -> VStackValue {
        match val {
            VStackValue::Constant(Value::Integer { ty, val }) => match target_loc {
                ValLocation::BpDisp(_) | ValLocation::SpDisp(_) | ValLocation::Regs(_) => {
                    todo!()
                }
                ValLocation::Null => VStackValue::OpaqueInt(ValLocation::Null, ty),
                ValLocation::Register(r) => {
                    if val == 0 {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::XorMR,
                                vec![X86Operand::ModRM(ModRM::Direct(r)), X86Operand::Register(r)],
                            )));
                    } else {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::MovImm,
                                vec![
                                    X86Operand::Register(r),
                                    X86Operand::Immediate(u64::from(val)),
                                ],
                            )));
                    }

                    VStackValue::OpaqueInt(target_loc, ty)
                }
            },
            VStackValue::Constant(Value::String {
                encoding: StringEncoding::Utf8,
                utf8,
                ty: Type::Pointer(ty),
            }) => match target_loc {
                ValLocation::BpDisp(_) | ValLocation::SpDisp(_) | ValLocation::Regs(_) => {
                    todo!()
                }
                ValLocation::Null => panic!("Cannot store a pointer in a zero-sized location"),
                ValLocation::Register(r) => {
                    let symname = self.strmap.borrow_mut().get_or_insert_string(utf8);
                    self.insns
                        .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                            X86Opcode::Lea,
                            vec![
                                X86Operand::Register(r),
                                X86Operand::ModRM(ModRM::Indirect {
                                    mode: ModRMRegOrSib::RipRel(Address::Symbol {
                                        name: symname.clone(),
                                        disp: 0,
                                    }),
                                }),
                            ],
                        )));
                    self.symbols.push(X86TempSymbol(
                        symname,
                        None,
                        None,
                        SymbolType::Null,
                        SymbolKind::Local,
                    ));
                    VStackValue::Pointer(LValue::OpaquePointer(target_loc), ty)
                }
            },
            VStackValue::Constant(Value::Uninitialized(ty)) => {
                VStackValue::Constant(Value::Uninitialized(ty))
            }
            VStackValue::Constant(Value::Invalid(ty)) => VStackValue::Constant(Value::Invalid(ty)),
            VStackValue::OpaqueInt(loc, ty) => todo!("Opaque Int in {:?}: {:?}", loc, ty),

            v => todo!("Other value {:?}", v),
        }
    }

    pub fn encode<W: InsnWrite>(
        mut self,
        x: &mut X86Encoder<W>,
        rod: &mut Section,
    ) -> std::io::Result<()> {
        x.write_insn(X86Instruction::new(
            X86Opcode::Push,
            vec![X86Operand::Register(X86Register::Rbp)],
        ))?;
        x.write_insn(X86Instruction::new(
            X86Opcode::MovMR,
            vec![
                X86Operand::ModRM(ModRM::Direct(X86Register::Rbp)),
                X86Operand::Register(X86Register::Rsp),
            ],
        ))?;
        if self.frame_size > 0 {
            x.write_insn(X86Instruction::new(
                X86Opcode::SubImm,
                vec![
                    X86Operand::ModRM(ModRM::Direct(X86Register::Rsp)),
                    X86Operand::Immediate(self.frame_size as u64),
                ],
            ))?;
        }
        for insn in self.insns {
            match insn {
                X86InstructionOrLabel::Label(lbl) => self.symbols.push(X86TempSymbol(
                    format!("{}._T{}", self.symname, lbl),
                    Some(".text"),
                    Some(x.offset()),
                    SymbolType::Null,
                    SymbolKind::Local,
                )),
                X86InstructionOrLabel::Insn(insn) => x.write_insn(insn)?,
            }
        }

        rod.write_all(&self.rodata)?;

        Ok(())
    }

    #[allow(clippy::needless_collect)] // Because it isn't needless
    pub fn write_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null => (),
            Expr::Const(v) => self.vstack.push_back(VStackValue::Constant(v.clone())),
            Expr::CallFunction(ty) => {
                let vcount = ty.params.len();
                let vslen = self.vstack.len();

                let params = self.vstack.drain(vslen - vcount..).collect::<Vec<_>>();
                let f = self.vstack.pop_back().unwrap();
                match f {
                    VStackValue::Constant(Value::GlobalAddress { ty: _, item }) => {
                        let name = match &*item.components {
                            [xlang_struct::PathComponent::Text(t)] | [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)] => &**t,
                            _ => panic!("Cannot access name component"),
                        }
                        .to_string();
                        for (i, val) in params.into_iter().enumerate() {
                            match i {
                                0 => self.move_value(val, ValLocation::Register(X86Register::Rdi)),
                                1 => self.move_value(val, ValLocation::Register(X86Register::Rsi)),
                                2 => self.move_value(val, ValLocation::Register(X86Register::Rdx)),
                                _ => todo!(),
                            };
                        }

                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::new(
                                X86Opcode::Call,
                                vec![X86Operand::RelAddr(Address::PltSym { name })],
                            )));
                    }
                    VStackValue::Constant(Value::Invalid(_) | Value::Uninitialized(_)) => {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::Ud2));
                    }
                    VStackValue::Constant(_)
                    | VStackValue::LValue(_)
                    | VStackValue::Pointer(_, _)
                    | VStackValue::OpaqueInt(_, _) => todo!(),
                }
            }
            Expr::ExitBlock { blk, values } => match (blk, values) {
                (0, 1) => {
                    let val = self.vstack.pop_back().unwrap();
                    if let VStackValue::Constant(Value::Invalid(_)) = val {
                        self.insns
                            .push(X86InstructionOrLabel::Insn(X86Instruction::Ud2));
                        return;
                    }
                    let ty = self.signature.ret.clone();
                    match ty {
                        Type::Scalar(s) => match s {
                            ScalarType {
                                header:
                                    ScalarTypeHeader {
                                        bitsize: size @ 1..=64,
                                        vectorsize: 0,
                                        validity: _,
                                    },
                                kind: ScalarTypeKind::Integer { .. },
                            } => {
                                let bytesize = ((7 + size) / 8).next_power_of_two();
                                let class = match bytesize {
                                    1 => X86RegisterClass::Byte,
                                    2 => X86RegisterClass::Word,
                                    4 => X86RegisterClass::Double,
                                    8 => X86RegisterClass::Quad,
                                    _ => unreachable!(),
                                };
                                drop(self.move_value(
                                    val,
                                    ValLocation::Register(
                                        X86Register::from_class(class, 0).unwrap(),
                                    ),
                                ));
                                self.insns
                                    .push(X86InstructionOrLabel::Insn(X86Instruction::Leave));
                                self.insns
                                    .push(X86InstructionOrLabel::Insn(X86Instruction::Retn));
                            }
                            _ => todo!("return scalar type"),
                        },
                        Type::Void => panic!("Cannot have a value of type Void"),
                        Type::FnType(_) | Type::Pointer(_) => todo!(),
                    }
                }
                (0, _) => panic!("Cannot return multiple values from a function"),
                (_, _) => todo!("exit block"),
            },
            Expr::UnaryOp(op) => {
                let val = self.vstack.pop_back().unwrap();

                match val {
                    VStackValue::Constant(Value::Integer {
                        ty:
                            ty @ ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            },
                        val,
                    }) => match op {
                        UnaryOp::Minus => {
                            self.vstack.push_back(VStackValue::Constant(Value::Integer {
                                ty,
                                val: (-(val as i32)) as u32,
                            }))
                        }
                        UnaryOp::BitNot => self
                            .vstack
                            .push_back(VStackValue::Constant(Value::Integer { ty, val: !val })),
                        UnaryOp::LogicNot => {
                            if val == 0 {
                                self.vstack
                                    .push_back(VStackValue::Constant(Value::Integer { ty, val: 1 }))
                            } else {
                                self.vstack
                                    .push_back(VStackValue::Constant(Value::Integer { ty, val: 0 }))
                            }
                        }
                    },
                    VStackValue::LValue(_) => panic!("typecheck fail"),
                    VStackValue::Pointer(_, _) => panic!("typecheck fail"),
                    _ => todo!(),
                }
            }
            Expr::BinaryOp(_) => todo!(),
            Expr::Branch { cond, target } => {
                let tsym = format!("{}.__T{}", self.symname, target);
                match cond {
                    BranchCondition::Never => {
                        drop(tsym);
                    }
                    BranchCondition::Always => {
                        assert!(self.targets.get(target).unwrap().len() == 0); // TODO: Handle parameterized branches
                        self.vstack.clear();
                    }
                    BranchCondition::Less => todo!(),
                    BranchCondition::LessEqual => todo!(),
                    BranchCondition::Equal => todo!(),
                    BranchCondition::NotEqual => todo!(),
                    BranchCondition::Greater => todo!(),
                    BranchCondition::GreaterEqual => todo!(),
                }
            }
        }
    }

    pub fn write_block(&mut self, block: &Block) {
        for item in &block.items {
            match item {
                xlang_struct::BlockItem::Expr(e) => self.write_expr(e),
                xlang_struct::BlockItem::Target { num, stack } => {
                    todo!("target @{} {:?}", num, &**stack)
                }
            }
        }
    }
}

pub struct X86CodegenPlugin {
    fns: Option<std::collections::HashMap<String, X86CodegenState>>,
    target: Option<target_tuples::Target>,
    strings: Rc<RefCell<StringInterner>>,
}

impl X86CodegenPlugin {
    fn write_output_impl<W: std::io::Write>(&mut self, mut x: W) -> std::io::Result<()> {
        let fmt = binfmt::def_vec_for(self.target.as_ref().unwrap());
        let mut file = fmt.create_file(FileType::Relocatable);
        let mut text = Section {
            name: String::from(".text"),
            align: 1204,
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

        let mut syms = Vec::new();

        for Pair(str, sym) in &self.strings.borrow_mut().syms {
            let sym = X86TempSymbol(
                sym.clone(),
                Some(".rodata"),
                Some(rodata.content.len()),
                SymbolType::Object,
                SymbolKind::Local,
            );
            rodata.content.extend_from_slice(str.as_ref());
            syms.push(sym);
        }

        for (name, output) in self.fns.take().unwrap() {
            let sym = X86TempSymbol(
                name.clone(),
                Some(".text"),
                Some(text.content.len()),
                SymbolType::Function,
                SymbolKind::Global,
            ); // TODO: internal linkage is a thing
            syms.push(sym);
            let mut encoder = X86Encoder::new(&mut text, output.mode);
            syms.extend_from_slice(&output.symbols);
            output.encode(&mut encoder, &mut rodata)?;
        }
        file.add_section(text).unwrap();
        file.add_section(rodata).unwrap();
        for sym in syms {
            let secno = sym
                .1
                .and_then(|v| file.sections().enumerate().find(|(_, s)| &*s.name == v))
                .map(|(s, _)| u32::try_from(s).unwrap());
            let fsym = file.get_or_create_symbol(&sym.0).unwrap();
            if secno.is_some() {
                *fsym.section_mut() = secno;
                *fsym.value_mut() = sym.2.map(|v| v as u128);
                *fsym.symbol_type_mut() = sym.3;
                *fsym.kind_mut() = sym.4;
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
                    let mut state = X86CodegenState::init(
                        ty.clone(),
                        X86Mode::default_mode_for(self.target.as_ref().unwrap()).unwrap(),
                        self.strings.clone(),
                        name.clone(),
                        xlang::targets::properties::get_properties(
                            self.target.as_ref().unwrap().into(),
                        )
                        .unwrap(),
                        HashSet::new(),
                    );
                    state.write_block(body);
                    self.fns.as_mut().unwrap().insert(name, state);
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
        strings: Rc::new(RefCell::new(StringInterner::new())),
    }))
}}
