use xlang::{abi::collection::HashSet, prelude::v1::HashMap};

pub use crate::ast::{Mutability, Safety, Spanned};
use crate::{
    ast::{self, PathSegment},
    helpers::nzu16,
    interning::Symbol,
    lang::LangItem,
    span::Pos,
};

pub use super::DefId;
use super::{cx::ConstExpr, generics::GenericArgs, mir::RegionId, tyck::InferId, Definitions};

use core::num::NonZeroU16;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AsyncType {
    Normal,
    Async,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AbiTag {
    Rust,
    RustCall,
    RustIntrinsic,
    LCRust(Option<u16>),
    C { unwind: bool },
    System { unwind: bool },
    Cdecl { unwind: bool },
    Stdcall { unwind: bool },
    Fastcall { unwind: bool },
    Thiscall { unwind: bool },
    Vectorcall { unwind: bool },
    Win64 { unwind: bool },
    SysV64 { unwind: bool },
    Aapcs { unwind: bool },
    Efiabi { unwind: bool },
    X86Interrupt,
    W65Interrupt,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntWidth {
    Bits(NonZeroU16),
    Size,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatFormat {
    IeeeBinary,
    IeeeExtRange,
    IeeeExtPrecision,
    Dfloat,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FloatType {
    pub width: NonZeroU16,
    pub format: FloatFormat,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntType {
    pub signed: bool,
    pub width: IntWidth,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FnType {
    pub safety: Spanned<Safety>,
    pub constness: Spanned<Mutability>,
    pub asyncness: Spanned<AsyncType>,
    pub tag: Spanned<AbiTag>,
    pub retty: Box<Spanned<Type>>,
    pub paramtys: Vec<Spanned<Type>>,
    pub iscvarargs: Spanned<bool>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SemaLifetime {
    Bound(Spanned<u32>),
    Region(RegionId),
    Static,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int(IntType),
    Float(FloatType),
    Char,
    Str,
    Never,
    Tuple(Vec<Spanned<Type>>),
    FnPtr(Box<FnType>),
    FnItem(Box<FnType>, DefId, GenericArgs),
    UserType(DefId, GenericArgs),
    UnresolvedLangItem(LangItem, GenericArgs),
    IncompleteAlias(DefId),
    Pointer(Spanned<Mutability>, Box<Spanned<Type>>),
    Array(Box<Spanned<Type>>, Spanned<ConstExpr>),
    Inferable(Option<InferId>),
    InferableInt(InferId),
    Reference(
        Option<Box<Spanned<SemaLifetime>>>,
        Spanned<Mutability>,
        Box<Spanned<Type>>,
    ),
    Param(u32),
    TraitSelf(DefId),
    DropFlags(Box<Type>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FatPtrPart {
    Payload,
    Metadata,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldName {
    EnumDiscrim,
    Field(Symbol),
    VariantSubfield(DefId, Symbol),
    FatPtrPart(FatPtrPart),
}

impl core::fmt::Display for AbiTag {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("extern \"")?;
        match self {
            Self::Rust => f.write_str("Rust")?,
            Self::RustCall => f.write_str("rust-call")?,
            Self::RustIntrinsic => f.write_str("rust-intrinsic")?,
            Self::LCRust(None) => f.write_str("lcrust")?,
            Self::LCRust(Some(ver)) => f.write_fmt(format_args!("lcrust-v{}", ver))?,
            Self::C { unwind } => {
                f.write_str("C")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::System { unwind } => {
                f.write_str("system")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Cdecl { unwind } => {
                f.write_str("cdecl")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Stdcall { unwind } => {
                f.write_str("stdcall")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Fastcall { unwind } => {
                f.write_str("fastcall")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Thiscall { unwind } => {
                f.write_str("thiscall")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Vectorcall { unwind } => {
                f.write_str("vectorcall")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Win64 { unwind } => {
                f.write_str("win64")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::SysV64 { unwind } => {
                f.write_str("sysv64")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Aapcs { unwind } => {
                f.write_str("aapcs")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::Efiabi { unwind } => {
                f.write_str("efiabi")?;
                if *unwind {
                    f.write_str("-unwind")?;
                }
            }
            Self::X86Interrupt => f.write_str("x86-interrupt")?,
            Self::W65Interrupt => f.write_str("w65-interrupt")?,
        }

        f.write_str("\"")
    }
}

#[doc(hidden)]
pub use super::intrin::__parse_tag;

pub fn convert_tag(tag: Spanned<Symbol>, curmod: DefId, at_item: DefId) -> super::Result<AbiTag> {
    let span = tag.span;
    let mut tag = &**tag;

    let unwind = if tag.ends_with("-unwind") {
        tag = &tag[..(tag.len() - 7)];
        true
    } else {
        false
    };

    match (tag, unwind) {
        ("Rust", false) => Ok(AbiTag::Rust),
        ("rust-call", false) => Ok(AbiTag::RustCall),
        ("rust-intrinsic", false) => Ok(AbiTag::RustIntrinsic),
        ("lcrust", false) => Ok(AbiTag::LCRust(None)),
        (x, false) if x.starts_with("lcrust-v") => {
            let x = &x[8..];

            let ver = x.parse::<u16>().map_err(|_| {
                let new_span = span.with_start(Pos::new(span.start.row(), span.start.col() + 8));
                super::Error {
                    span,
                    text: format!("Unknown abi {}", tag),
                    category: super::ErrorCategory::InvalidAbi,
                    at_item,
                    containing_item: curmod,
                    relevant_item: at_item,
                    hints: vec![super::SemaHint {
                        text: format!("Couldn't parse {} as a version of the lcrust abi", x),
                        itemref: at_item,
                        refspan: new_span,
                    }],
                }
            })?;

            Ok(AbiTag::LCRust(Some(ver)))
        }
        (x @ ("Rust" | "rust-call" | "rust-intrinsic" | "lcrust"), true) => Err(super::Error {
            span,
            text: format!("Unkown abi {}", tag),
            category: super::ErrorCategory::InvalidAbi,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![super::SemaHint {
                text: format!("extern \"{}\" supports unwinding by default", x),
                itemref: at_item,
                refspan: span,
            }],
        }),
        (x, true) if x.starts_with("lcrust-v") => Err(super::Error {
            span,
            text: format!("Unkown abi {}", tag),
            category: super::ErrorCategory::InvalidAbi,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![super::SemaHint {
                text: format!("extern \"{}\" supports unwinding by default", x),
                itemref: at_item,
                refspan: span,
            }],
        }),
        ("x86-interrupt", false) => Ok(AbiTag::X86Interrupt),
        ("w65-interrupt", false) => Ok(AbiTag::W65Interrupt),
        (x @ ("x86-interrupt" | "w65-interrupt"), true) => Err(super::Error {
            span,
            text: format!("Unkown abi {}", tag),
            category: super::ErrorCategory::InvalidAbi,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![super::SemaHint {
                text: format!(
                    "extern \"{}\" has special handing and cannot supprt unwinding",
                    x
                ),
                itemref: at_item,
                refspan: span,
            }],
        }),
        ("C", unwind) => Ok(AbiTag::C { unwind }),
        ("system", unwind) => Ok(AbiTag::System { unwind }),
        ("cdecl", unwind) => Ok(AbiTag::Cdecl { unwind }),
        ("stdcall", unwind) => Ok(AbiTag::Stdcall { unwind }),
        ("fastcall", unwind) => Ok(AbiTag::Fastcall { unwind }),
        ("thiscall", unwind) => Ok(AbiTag::Thiscall { unwind }),
        ("vectorcall", unwind) => Ok(AbiTag::Vectorcall { unwind }),
        ("win64", unwind) => Ok(AbiTag::Win64 { unwind }),
        ("sysv64", unwind) => Ok(AbiTag::SysV64 { unwind }),
        ("aapcs", unwind) => Ok(AbiTag::Aapcs { unwind }),
        ("efiabi", unwind) => Ok(AbiTag::Efiabi { unwind }),
        (_, _) => Err(super::Error {
            span,
            text: format!("Unkown abi {}", tag),
            category: super::ErrorCategory::InvalidAbi,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![],
        }),
    }
}

// Write the Rust type here
#[allow(non_upper_case_globals)]
impl FloatType {
    pub const f16: FloatType = FloatType {
        width: nzu16!(16),
        format: FloatFormat::IeeeBinary,
    };
    pub const f32: FloatType = FloatType {
        width: nzu16!(32),
        format: FloatFormat::IeeeBinary,
    };
    pub const f64: FloatType = FloatType {
        width: nzu16!(64),
        format: FloatFormat::IeeeBinary,
    };
    pub const f128: FloatType = FloatType {
        width: nzu16!(128),
        format: FloatFormat::IeeeBinary,
    };
    pub const bf16: FloatType = FloatType {
        width: nzu16!(16),
        format: FloatFormat::IeeeExtRange,
    };
    pub const fx80: FloatType = FloatType {
        width: nzu16!(80),
        format: FloatFormat::IeeeExtPrecision,
    };
    pub const fd128: FloatType = FloatType {
        width: nzu16!(128),
        format: FloatFormat::Dfloat,
    };
}

#[allow(non_upper_case_globals)] // Write the Rust type here
impl IntType {
    pub const isize: IntType = IntType {
        signed: true,
        width: IntWidth::Size,
    };
    pub const usize: IntType = IntType {
        signed: false,
        width: IntWidth::Size,
    };

    pub const u8: IntType = IntType {
        signed: false,
        width: IntWidth::Bits(nzu16!(8)),
    };
    pub const u16: IntType = IntType {
        signed: false,
        width: IntWidth::Bits(nzu16!(16)),
    };
    pub const u32: IntType = IntType {
        signed: false,
        width: IntWidth::Bits(nzu16!(32)),
    };
    pub const u64: IntType = IntType {
        signed: false,
        width: IntWidth::Bits(nzu16!(64)),
    };
    pub const u128: IntType = IntType {
        signed: false,
        width: IntWidth::Bits(nzu16!(128)),
    };
    pub const i8: IntType = IntType {
        signed: true,
        width: IntWidth::Bits(nzu16!(8)),
    };
    pub const i16: IntType = IntType {
        signed: true,
        width: IntWidth::Bits(nzu16!(16)),
    };
    pub const i32: IntType = IntType {
        signed: true,
        width: IntWidth::Bits(nzu16!(32)),
    };
    pub const i64: IntType = IntType {
        signed: true,
        width: IntWidth::Bits(nzu16!(64)),
    };
    pub const i128: IntType = IntType {
        signed: true,
        width: IntWidth::Bits(nzu16!(128)),
    };
}

impl core::fmt::Display for IntType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.signed {
            f.write_str("i")?;
        } else {
            f.write_str("u")?;
        }

        match self.width {
            IntWidth::Size => f.write_str("size"),
            IntWidth::Bits(bits) => f.write_fmt(format_args!("{}", bits)),
        }
    }
}

pub fn parse_int_type(
    sym: Spanned<Symbol>,
    at_item: DefId,
    curmod: DefId,
) -> super::Result<Spanned<IntType>> {
    let span = sym.span;
    let (first, size) = sym.split_at(1);

    let signed = match first {
        "i" => true,
        "u" => false,
        _ => {
            return Err(super::Error {
                span,
                text: format!("Expected a integer type, got {}", sym.body),
                category: super::ErrorCategory::Other,
                at_item,
                containing_item: curmod,
                relevant_item: at_item,
                hints: vec![],
            });
        }
    };
    let width = if size == "size" {
        IntWidth::Size
    } else {
        let size = size.parse::<NonZeroU16>().map_err(|_| super::Error {
            span,
            text: format!("Invalid integer type {}", sym.body),
            category: super::ErrorCategory::Other,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![],
        })?;
        match size.get() {
            8 | 16 | 32 | 64 | 128 => {}
            _ => Err(super::Error {
                span,
                text: format!("Invalid integer type {}", sym.body),
                category: super::ErrorCategory::Other,
                at_item,
                containing_item: curmod,
                relevant_item: at_item,
                hints: vec![],
            })?,
        }
        IntWidth::Bits(size)
    };

    Ok(Spanned {
        body: IntType { signed, width },
        span,
    })
}

pub fn parse_int_suffix(
    sym: Spanned<Symbol>,
    at_item: DefId,
    curmod: DefId,
) -> super::Result<Spanned<IntType>> {
    let span = sym.span;
    let sym = sym.strip_prefix('_').unwrap_or(&sym);

    let (first, size) = sym.split_at(1);

    let signed = match first {
        "i" => true,
        "u" => false,
        _ => {
            return Err(super::Error {
                span,
                text: format!("Expected a integer suffix, got {}", sym),
                category: super::ErrorCategory::Other,
                at_item,
                containing_item: curmod,
                relevant_item: at_item,
                hints: vec![],
            });
        }
    };
    let width = if size == "size" {
        IntWidth::Size
    } else {
        let size = size.parse::<NonZeroU16>().map_err(|_| super::Error {
            span,
            text: format!("Invalid integer suffix {}", sym),
            category: super::ErrorCategory::Other,
            at_item,
            containing_item: curmod,
            relevant_item: at_item,
            hints: vec![],
        })?;
        match size.get() {
            8 | 16 | 32 | 64 | 128 => {}
            _ => Err(super::Error {
                span,
                text: format!("Invalid integer suffix {}", sym),
                category: super::ErrorCategory::Other,
                at_item,
                containing_item: curmod,
                relevant_item: at_item,
                hints: vec![],
            })?,
        }
        IntWidth::Bits(size)
    };

    Ok(Spanned {
        body: IntType { signed, width },
        span,
    })
}

impl FnType {
    pub fn matches_ignore_bounds(&self, other: &Self) -> bool {
        self.safety.body == other.safety.body
            && self.tag.body == other.tag.body
            && self.iscvarargs.body == other.iscvarargs.body
            && self.asyncness.body == other.asyncness.body
            && self.constness.body == other.constness.body
            && self.retty.matches_ignore_bounds(&other.retty)
            && self
                .paramtys
                .iter()
                .zip(&other.paramtys)
                .all(|(a, b)| a.matches_ignore_bounds(b))
    }
}

impl core::fmt::Display for FnType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.constness.body == Mutability::Const {
            f.write_str("const ")?;
        }

        if self.asyncness.body == AsyncType::Async {
            f.write_str("async ")?;
        }

        if self.safety.body == Safety::Unsafe {
            f.write_str("unsafe ")?;
        }

        self.tag.body.fmt(f)?;
        f.write_str(" fn(")?;
        let mut sep = "";
        for param in &self.paramtys {
            f.write_str(sep)?;
            sep = ", ";
            param.body.fmt(f)?;
        }

        if self.iscvarargs.body {
            f.write_str(sep)?;
            f.write_str("...")?;
        }
        f.write_str(") -> ")?;

        self.retty.body.fmt(f)
    }
}

impl core::fmt::Display for SemaLifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bound(sym) => f.write_fmt(format_args!("'%{}", sym.body)),
            Self::Region(reg) => reg.fmt(f),
            Self::Static => f.write_str("'static"),
        }
    }
}

impl Type {
    pub const UNIT: Self = Self::Tuple(Vec::new());

    pub fn matches_ignore_bounds(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Type::Param(_)) | (_, Type::TraitSelf(_)) => true,
            (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::Str, Type::Str)
            | (Type::Never, Type::Never) => true,
            (Type::Tuple(t), Type::Tuple(u)) => {
                t.len() == u.len() && t.iter().zip(u).all(|(a, b)| a.matches_ignore_bounds(b))
            }
            (Type::UserType(def1, _), Type::UserType(def2, _)) => def1 == def2,
            (Type::FnPtr(p1), Type::FnPtr(p2)) => p1.matches_ignore_bounds(p2),
            (Type::FnItem(fnty1, defid1, _), Type::FnItem(fnty2, defid2, _)) => {
                fnty1.matches_ignore_bounds(fnty2) && defid1 == defid2
            }
            (Type::Array(g1, expr1), Type::Array(g2, expr2)) => {
                g1.matches_ignore_bounds(g2) && expr1 == expr2
            }
            (Type::Reference(_, mt1, bx1), Type::Reference(_, mt2, bx2)) => {
                mt1 == mt2 && bx1.matches_ignore_bounds(bx2)
            }
            (Type::Pointer(mt1, bx1), Type::Pointer(mt2, bx2)) => mt1 == mt2 && bx1 == bx2,
            _ => false,
        }
    }

    pub fn is_inference(&self) -> bool {
        matches!(self, Self::Inferable(_) | Self::InferableInt(_))
    }

    pub fn rt_impl_lang(&self) -> Option<LangItem> {
        match self {
            Self::Float(x) => match *x {
                FloatType::f32 => Some(LangItem::F32Rt),
                FloatType::f64 => Some(LangItem::F64Rt),
                FloatType::f16 => Some(LangItem::F16Rt),
                FloatType::f128 => Some(LangItem::F128Rt),
                FloatType::bf16 => Some(LangItem::Bf16Rt),
                FloatType::fd128 => Some(LangItem::Fd128Rt),
                FloatType::fx80 => Some(LangItem::Fx80Rt),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_lang_item(&self) -> Option<LangItem> {
        match self {
            Self::Bool => Some(LangItem::Bool),
            Self::Char => Some(LangItem::Char),
            Self::Str => Some(LangItem::Str),
            Self::Array(_, _) => Some(LangItem::Array),
            Self::Tuple(tuples) if tuples.len() == 0 => Some(LangItem::Unit),
            Self::Tuple(_) => None,
            Self::Int(x) => match *x {
                IntType::u8 => Some(LangItem::U8),
                IntType::u16 => Some(LangItem::U16),
                IntType::u32 => Some(LangItem::U32),
                IntType::u64 => Some(LangItem::U64),
                IntType::u128 => Some(LangItem::U128),
                IntType::usize => Some(LangItem::USize),
                IntType::i8 => Some(LangItem::I8),
                IntType::i16 => Some(LangItem::I16),
                IntType::i32 => Some(LangItem::I32),
                IntType::i64 => Some(LangItem::I64),
                IntType::i128 => Some(LangItem::I128),
                IntType::isize => Some(LangItem::ISize),
                _ => None,
            },
            Self::Float(x) => match *x {
                FloatType::f32 => Some(LangItem::F32),
                FloatType::f64 => Some(LangItem::F64),
                FloatType::f16 => Some(LangItem::F16),
                FloatType::f128 => Some(LangItem::F128),
                FloatType::bf16 => Some(LangItem::Bf16),
                FloatType::fd128 => Some(LangItem::Fd128),
                FloatType::fx80 => Some(LangItem::Fx80),
                _ => None,
            },
            Self::Pointer(
                Spanned {
                    body: Mutability::Const,
                    ..
                },
                _,
            ) => Some(LangItem::ConstPtr),
            Self::Pointer(
                Spanned {
                    body: Mutability::Mut,
                    ..
                },
                _,
            ) => Some(LangItem::MutPtr),
            Self::Never => Some(LangItem::Never),
            _ => None,
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Bool => f.write_str("bool"),
            Self::Int(intty) => intty.fmt(f),
            Self::Float(ty) => {
                match ty.format {
                    FloatFormat::IeeeBinary => f.write_str("f")?,
                    FloatFormat::IeeeExtPrecision => f.write_str("fx")?,
                    FloatFormat::IeeeExtRange => f.write_str("bf")?,
                    FloatFormat::Dfloat => f.write_str("fd")?,
                }

                ty.width.fmt(f)
            }
            Self::Char => f.write_str("char"),
            Self::Str => f.write_str("str"),
            Self::Never => f.write_str("!"),
            Self::Tuple(tys) => {
                f.write_str("(")?;
                let mut sep = "";
                for ty in tys {
                    f.write_str(sep)?;
                    sep = ", ";
                    ty.body.fmt(f)?;
                }
                f.write_str(")")
            }
            Self::FnPtr(fnty) => fnty.fmt(f),
            Self::FnItem(fnty, fndef, generics) => {
                fnty.fmt(f)?;
                f.write_str(" {")?;
                fndef.fmt(f)?;
                generics.fmt(f)?;
                f.write_str("}")
            }
            Self::UserType(defid, generics) => f.write_fmt(format_args!("{}{}", defid, generics)),
            Self::IncompleteAlias(defid) => {
                defid.fmt(f)?;
                f.write_str(" /* unresolved type alias */")
            }
            Self::Pointer(mt, inner) => {
                f.write_str("*")?;
                mt.body.fmt(f)?;
                f.write_str(" ")?;
                inner.body.fmt(f)
            }
            Self::Inferable(x) => write!(f, "{{{{type {:?}}}}}", x),
            Self::InferableInt(x) => write!(f, "{{{{int {:?}}}}}", x),
            Self::Array(elem, len) => {
                f.write_str("[")?;
                elem.body.fmt(f)?;
                f.write_str("; ")?;
                len.body.fmt(f)?;
                f.write_str("]")
            }
            Self::Reference(life, mt, ty) => {
                f.write_str("&")?;
                if let Some(life) = life {
                    life.body.fmt(f)?;
                    f.write_str(" ")?;
                }

                if mt.body == Mutability::Mut {
                    f.write_str("mut ")?;
                }
                ty.body.fmt(f)
            }
            Self::Param(var) => f.write_fmt(format_args!("%{}", var)),
            Self::TraitSelf(tr) => f.write_fmt(format_args!("Self(impl #{})", tr)),
            Self::DropFlags(ty) => f.write_fmt(format_args!("DropFlags({})", ty)),
            Self::UnresolvedLangItem(lang, args) => {
                f.write_fmt(format_args!("{} {}", lang.name(), args))
            }
        }
    }
}

pub fn convert_builtin_type(name: &str) -> Option<Type> {
    match name {
        "_" => Some(Type::Inferable(None)),
        "char" => Some(Type::Char),
        "str" => Some(Type::Str),
        x if x.starts_with('i') || x.starts_with('u') => {
            let (s, size) = x.split_at(1);

            let width = if size == "size" {
                IntWidth::Size
            } else {
                let size = size.parse::<NonZeroU16>().ok()?;
                match size.get() {
                    8 | 16 | 32 | 64 | 128 => {}
                    _ => None?,
                }
                IntWidth::Bits(size)
            };

            Some(Type::Int(IntType {
                signed: s == "i",
                width,
            }))
        }
        x if x.starts_with('f') => {
            let size = x[1..].parse::<NonZeroU16>().ok()?;
            match size.get() {
                16 | 32 | 64 | 32 => {}
                _ => None?,
            }

            Some(Type::Float(FloatType {
                format: FloatFormat::IeeeBinary,
                width: size,
            }))
        }
        _ => None,
    }
}

pub fn convert_type(
    defs: &Definitions,
    curmod: DefId,
    at_item: DefId,
    ty: &ast::Type,
    self_ty: Option<&Type>,
) -> super::Result<Type> {
    match ty {
        ast::Type::Path(path) => match defs.find_type(curmod, &path.body, at_item, self_ty) {
            Ok((defid, generics)) => Ok(Type::UserType(defid, generics)),
            Err(e) => match (&*path.segments, &path.root) {
                (
                    [Spanned {
                        body:
                            PathSegment {
                                ident:
                                    Spanned {
                                        body: ast::SimplePathSegment::Identifier(id),
                                        ..
                                    },
                                generics: None,
                            },
                        ..
                    }],
                    None
                    | Some(Spanned {
                        body: ast::PathRoot::Root,
                        ..
                    }),
                ) => convert_builtin_type(id).ok_or(e),
                _ => Err(e),
            },
        },
        ast::Type::Reference(_, _) => todo!("reference"),
        ast::Type::Pointer(mt, ty) => {
            let ty = ty.try_copy_span(|ty| convert_type(defs, curmod, at_item, ty, self_ty))?;

            Ok(Type::Pointer(*mt, Box::new(ty)))
        }
        ast::Type::Array(_, _) => todo!("array"),
        ast::Type::FnType(_) => todo!("fn-ptr"),
        ast::Type::Never => Ok(Type::Never),
        ast::Type::Tuple(tys) => {
            let mut tys = tys
                .iter()
                .map(|ty| ty.try_copy_span(|ty| convert_type(defs, curmod, at_item, ty, self_ty)))
                .collect::<super::Result<Vec<_>>>()?;

            Ok(Type::Tuple(tys))
        }
    }
}

impl core::fmt::Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EnumDiscrim => f.write_str("{{discriminant}}"),
            Self::Field(sym) => f.write_str(sym),
            Self::VariantSubfield(var, field) => {
                var.fmt(f)?;
                f.write_str("::")?;
                f.write_str(field)
            }
            Self::FatPtrPart(part) => part.fmt(f),
        }
    }
}

impl core::fmt::Display for FatPtrPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Payload => f.write_str("{{data}}"),
            Self::Metadata => f.write_str("{{metadata}}"),
        }
    }
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ScalarNiches {
    pub is_nonzero: bool,
    pub max_value: u128,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Niches {
    Uninhabited,
    NonNullPointer,
    Scalar(ScalarNiches),
    Aggregate(Vec<(FieldName, Niches)>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NicheOptEnum {
    /// The DefId of the construct for which niche optimization applies
    pub niche_ctor: DefId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DiscrimPlacement {
    /// Offset into the type - in lcrust v0, this is always `0`
    Offset(u64),
    /// The Discrimant is elided into niche-opt
    FillNiche(NicheOptEnum),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumDiscriminant {
    pub discrim_type: IntType,
    pub discrim_placement: DiscrimPlacement,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct EnumLayout {
    pub discrim: EnumDiscriminant,
    pub variant_layouts: HashMap<DefId, VariantLayout>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VariantLayout {
    /// The value of the discriminant for this variant
    pub discrim_val: u128,
    /// The layout of the variant, including the discriminant (if not elided)
    pub variant_layout: TypeLayout,
    /// If the variant is a niche-ellision variant, indicate how the niche is filled
    pub variant_niche_fill: Option<NicheFill>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NicheFill {
    pub niche_offset: u64,
    pub niche_size: u64,
    /// The value to put in the niche.
    /// `None` means the niched field is uninhabited
    pub niche_value: Option<NicheValue>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NicheValue {
    IntValue(u128),
    NullPointer,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeLayout {
    pub size: Option<u64>,
    pub align: Option<u64>,
    pub enum_layout: Option<EnumLayout>,
    pub wide_ptr_metadata: Option<Type>,
    pub field_offsets: HashMap<FieldName, u64>,
    pub mutable_fields: HashSet<FieldName>,
    pub niches: Option<Niches>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeField {
    pub name: FieldName,
    pub ty: Type,
    pub vis: DefId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SemaTypeBound {
    Trait(DefId),
    Life(SemaLifetime),
}

impl core::fmt::Display for SemaTypeBound {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Trait(tr) => tr.fmt(f),
            Self::Life(life) => life.fmt(f),
        }
    }
}
