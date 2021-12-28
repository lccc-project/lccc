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
        insn::{ModRM, ModRMRegOrSib, X86Encoder, X86Instruction, X86Mode, X86Opcode, X86Operand},
        X86Register, X86RegisterClass,
    },
};

use binfmt::{
    fmt::{FileType, Section, SectionType},
    sym::{SymbolKind, SymbolType},
};
use xlang::{
    abi::{collection::HashMap, hash::XLangHasher},
    plugin::{XLangCodegen, XLangPlugin},
    prelude::v1::{Box, DynBox, Pair},
};
use xlang_struct::{
    Block, Expr, FnType, FunctionDeclaration, PointerType, ScalarType, ScalarTypeHeader,
    ScalarTypeKind, StringEncoding, Type, Value,
};

#[derive(Clone, Debug, Hash)]
pub struct StringInterner {
    syms: HashMap<xlang::abi::string::String, String>,
    inuse: HashMap<u64, usize>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            syms: HashMap::new(),
            inuse: HashMap::new(),
        }
    }

    pub fn get_or_insert_string(&mut self, st: xlang::abi::string::String) -> String {
        let StringInterner { syms, inuse } = self;
        syms.get_or_insert_with_mut(st, |s| {
            let mut hasher = XLangHasher::default();
            s.hash(&mut hasher);
            let val = hasher.finish();
            let suffix = if let Some(x) = inuse.get_mut(&val) {
                let val = *x;
                *x += 1;
                format!(".{}", val)
            } else {
                inuse.insert(val, 0);
                format!("")
            };

            let str = format!("__xlang_string.{}.{:016x}{}", xlang::version(), val, suffix);
            str
        })
        .clone()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum ValLocation {
    BpDisp(i32),
    SpDisp(i32),
    Register(X86Register),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum LValue {
    Local(usize),
    PointerConstant(Value),
    Temporary(Box<VStackValue>),
    OpaquePointer(ValLocation),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum VStackValue {
    Constant(Value),
    LValue(LValue),
    Pointer(LValue, PointerType),
    OpaqueInt(ValLocation, ScalarType),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum RegisterStatus {
    Allocated,
    Free,
    CalleeSave,
    Saved { loc: ValLocation, next: Box<Self> },
}

use RegisterStatus::{Allocated, CalleeSave, Free};

#[derive(Debug, Clone)]
struct X86TempSymbol(
    String,
    Option<&'static str>,
    Option<usize>,
    SymbolType,
    SymbolKind,
);

struct X86CodegenState {
    vstack: VecDeque<VStackValue>,
    strmap: Rc<RefCell<StringInterner>>,
    insns: Vec<X86Instruction>,
    rodata: Vec<u8>,
    frame_size: u64,
    symbols: Vec<X86TempSymbol>,
    locals: Vec<VStackValue>,
    regtab: [RegisterStatus; 16],
    signature: FnType,
    mode: X86Mode,
}

impl X86CodegenState {
    pub fn init(sig: FnType, mode: X86Mode, strmap: Rc<RefCell<StringInterner>>) -> Self {
        Self {
            vstack: VecDeque::new(),
            strmap,
            insns: Vec::new(),
            rodata: Vec::new(),
            frame_size: 0,
            symbols: Vec::new(),
            locals: Vec::new(),
            regtab: [
                Free, Free, Free, CalleeSave, CalleeSave, CalleeSave, Free, Free, Free, Free, Free,
                Free, CalleeSave, CalleeSave, CalleeSave, CalleeSave,
            ],
            signature: sig,
            mode,
        }
    }

    fn allocate_register(&mut self, class: X86RegisterClass) -> X86Register {
        for (r, i) in self.regtab.iter_mut().enumerate() {
            if let Free = i {
                *i = Allocated;
                return X86Register::from_class(class, u8::try_from(r).unwrap()).unwrap();
                // TODO, this allocates the Xmm registers at the same time as the other registers.
            }
        }
        todo!("allocate_register")
    }

    fn move_value(&mut self, val: VStackValue, tloc: ValLocation) -> VStackValue {
        match val {
            VStackValue::Constant(Value::Integer { ty, val }) => match tloc {
                ValLocation::BpDisp(_) => todo!(),
                ValLocation::SpDisp(_) => todo!(),
                ValLocation::Register(r) => {
                    if val == 0 {
                        self.insns.push(X86Instruction::new(
                            X86Opcode::XorMR,
                            vec![X86Operand::ModRM(ModRM::Direct(r)), X86Operand::Register(r)],
                        ))
                    } else {
                        self.insns.push(X86Instruction::new(
                            X86Opcode::MovImm,
                            vec![X86Operand::Register(r), X86Operand::Immediate(val as u64)],
                        ))
                    }

                    VStackValue::OpaqueInt(tloc, ty)
                }
            },
            VStackValue::Constant(Value::String {
                encoding: StringEncoding::Utf8,
                utf8,
                ty: Type::Pointer(ty),
            }) => match tloc {
                ValLocation::BpDisp(_) => todo!(),
                ValLocation::SpDisp(_) => todo!(),
                ValLocation::Register(r) => {
                    let symname = self.strmap.borrow_mut().get_or_insert_string(utf8);
                    self.insns.push(X86Instruction::new(
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
                    ));
                    self.symbols.push(X86TempSymbol(
                        symname,
                        None,
                        None,
                        SymbolType::Null,
                        SymbolKind::Local,
                    ));
                    VStackValue::Pointer(LValue::OpaquePointer(tloc), ty)
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
        self,
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
                    X86Operand::Immediate(self.frame_size),
                ],
            ))?;
        }
        for insn in self.insns {
            x.write_insn(insn)?;
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
                            [xlang_struct::PathComponent::Text(t)] => &**t,
                            [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)] => &**t,
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

                        self.insns.push(X86Instruction::new(
                            X86Opcode::Call,
                            vec![X86Operand::RelAddr(Address::PltSym { name })],
                        ))
                    }
                    VStackValue::Constant(Value::Invalid(_))
                    | VStackValue::Constant(Value::Uninitialized(_)) => {
                        self.insns.push(X86Instruction::Ud2)
                    }
                    VStackValue::Constant(_) => todo!(),
                    VStackValue::LValue(_) => todo!(),
                    VStackValue::Pointer(_, _) => todo!(),
                    VStackValue::OpaqueInt(_, _) => todo!(),
                }
            }
            Expr::ExitBlock { blk, values } => match (blk, values) {
                (0, 1) => {
                    let val = self.vstack.pop_back().unwrap();
                    if let VStackValue::Constant(Value::Invalid(_)) = val {
                        self.insns.push(X86Instruction::Ud2);
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
                                let bsize = ((7 + size) / 8).next_power_of_two();
                                let class = match bsize {
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
                                self.insns.push(X86Instruction::Leave);
                                self.insns.push(X86Instruction::Retn);
                            }
                            _ => todo!("return scalar type"),
                        },
                        Type::Void => panic!("Cannot have a value of type Void"),
                        Type::FnType(_) => todo!(),
                        Type::Pointer(_) => todo!(),
                    }
                }
                (0, _) => panic!("Cannot return multiple values from a function"),
                (_, _) => todo!("exit block"),
            },
            Expr::BinaryOp(_) => todo!(),
            Expr::UnaryOp(_) => todo!(),
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
            ..Default::default()
        };

        let mut rodata = Section {
            name: String::from(".rodata"),
            align: 1024,
            ty: SectionType::ProgBits,
            content: Vec::new(),
            relocs: Vec::new(),
            ..Default::default()
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
            syms.push(sym)
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
                .map(|(s, _)| s as u32);
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
                [xlang_struct::PathComponent::Text(t)] => &**t,
                [xlang_struct::PathComponent::Root, xlang_struct::PathComponent::Text(t)] => &**t,
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

#[no_mangle]
pub extern "C" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(Box::new(X86CodegenPlugin {
        fns: Some(std::collections::HashMap::new()),
        target: None,
        strings: Rc::new(RefCell::new(StringInterner::new())),
    }))
}
