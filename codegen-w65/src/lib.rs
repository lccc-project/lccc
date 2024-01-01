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
    callconv::CallingConvention,
    expr::ValLocation,
    mangle::mangle_itanium,
    mc::{MCBackend, MCWriter, MachineFeatures},
    str::StringMap,
    ty::TypeInformation,
    FunctionCodegen, FunctionRawCodegen,
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

pub struct W65MachineFeatures {
    properties: &'static TargetProperties<'static>,
}

impl MachineFeatures for W65MachineFeatures {
    type Loc = W65ValLocation;

    type CallConv = W65CallConv;

    fn native_int_size(&self) -> u16 {
        todo!()
    }

    fn native_float_size(&self) -> Option<u16> {
        todo!()
    }

    fn lockfree_use_libatomic(&self, size: u64) -> bool {
        todo!()
    }

    fn lockfree_cmpxchg_use_libatomic(&self, size: u64) -> bool {
        todo!()
    }

    fn has_wait_free_compound(&self, op: BinaryOp, size: u64) -> bool {
        todo!()
    }

    fn has_wait_free_compound_fetch(&self, op: BinaryOp, size: u64) -> bool {
        todo!()
    }

    fn mangle(&self, path: &[PathComponent]) -> String {
        todo!()
    }
}

pub struct W65MCWriter {}

impl MCWriter for W65MCWriter {
    fn get_features(
        &self,
        properties: &'static TargetProperties<'static>,
        features: Span<StringView>,
    ) -> Self::Features {
        W65MachineFeatures { properties }
    }

    type Features = W65MachineFeatures;

    type Clobbers = ();

    fn resolve_locations(
        &self,
        insns: &mut [xlang_backend::mc::MCInsn<<Self::Features as MachineFeatures>::Loc>],
        callconv: &<Self::Features as MachineFeatures>::CallConv,
    ) -> Self::Clobbers {
        todo!()
    }

    fn write_machine_code<I: InsnWrite, F: FnMut(String, u64)>(
        &self,
        insns: &[xlang_backend::mc::MCInsn<<Self::Features as MachineFeatures>::Loc>],
        clobbers: Self::Clobbers,
        tys: Rc<TypeInformation>,
        out: &mut I,
        sym_accepter: F,
    ) -> std::io::Result<()> {
        todo!()
    }

    fn get_call_conv(
        &self,
        realty: &FnType,
        targ: &'static TargetProperties<'static>,
        features: Span<StringView>,
        ty_info: Rc<TypeInformation>,
    ) -> <Self::Features as MachineFeatures>::CallConv {
        todo!()
    }

    fn target_matches(&self, name: &str) -> bool {
        Target::parse(name).arch() == Architecture::Wc65c816
    }
}

xlang::host::rustcall! {
#[no_mangle]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::abi::boxed::Box::new(MCBackend::new(W65MCWriter{})))
}}

xlang::plugin_abi_version!("0.1");
