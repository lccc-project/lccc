pub use crate::ast::{Mutability, Safety, Spanned};
use crate::{
    ast::{self, PathSegment},
    helpers::nzu16,
    interning::Symbol,
    lang::LangItem,
    span::Pos,
};

pub use super::DefId;
use super::{cx::ConstExpr, mir::RegionId, tyck::InferId, Definitions};

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
                let new_span = span.with_start(Pos {
                    row: span.start.row + 8,
                    ..span.start
                });
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum IntWidth {
    Bits(NonZeroU16),
    Size,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FloatWidth {
    Bits(NonZeroU16),
    Long,
}

// Write the Rust type here
#[allow(non_upper_case_globals)]
impl FloatWidth {
    pub const f32: FloatWidth = FloatWidth::Bits(nzu16!(32));
    pub const f64: FloatWidth = FloatWidth::Bits(nzu16!(64));
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct IntType {
    pub signed: bool,
    pub width: IntWidth,
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SemaLifetime {
    Bound(Spanned<Symbol>),
    Region(RegionId),
    Static,
}

impl core::fmt::Display for SemaLifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bound(sym) => {
                f.write_str("'")?;
                f.write_str(sym)
            }
            Self::Region(reg) => reg.fmt(f),
            Self::Static => f.write_str("'static"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Bool,
    Int(IntType),
    Float(FloatWidth),
    Char,
    Str,
    Never,
    Tuple(Vec<Spanned<Type>>),
    FnPtr(FnType),
    FnItem(FnType, DefId),
    UserType(DefId),
    IncompleteAlias(DefId),
    Pointer(Spanned<Mutability>, Box<Spanned<Type>>),
    Array(Box<Spanned<Type>>, Spanned<ConstExpr>),
    Inferable(InferId),
    InferableInt(InferId),
    Reference(
        Option<Spanned<SemaLifetime>>,
        Spanned<Mutability>,
        Box<Spanned<Type>>,
    ),
    Param(u32),
}

impl Type {
    pub const UNIT: Self = Self::Tuple(Vec::new());

    pub fn is_inference(&self) -> bool {
        matches!(self, Self::Inferable(_) | Self::InferableInt(_))
    }

    pub fn as_lang_item(&self) -> Option<LangItem> {
        match self {
            Self::Bool => Some(LangItem::Bool),
            Self::Char => Some(LangItem::Char),
            Self::Str => Some(LangItem::Str),
            Self::Array(_, _) => Some(LangItem::Array),
            Self::Tuple(tuples) if tuples.len() == 0 => Some(LangItem::Unit),
            Self::Tuple(_) => Some(LangItem::Tuple),
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
            Self::Float(width) => {
                f.write_str("f")?;
                match width {
                    FloatWidth::Bits(bits) => f.write_fmt(format_args!("{}", bits)),
                    FloatWidth::Long => f.write_str("long"),
                }
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
            Self::FnItem(fnty, fndef) => {
                fnty.fmt(f)?;
                f.write_str(" {")?;
                fndef.fmt(f)?;
                f.write_str("}")
            }
            Self::UserType(defid) => defid.fmt(f),
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
            Self::Inferable(_) => f.write_str("{{type}}"),
            Self::InferableInt(_) => f.write_str("{{int}}"),
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
        }
    }
}

pub fn convert_builtin_type(name: &str) -> Option<Type> {
    match name {
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
                32 | 64 => {}
                _ => None?,
            }

            Some(Type::Float(FloatWidth::Bits(size)))
        }
        _ => None,
    }
}

pub fn convert_type(
    defs: &Definitions,
    curmod: DefId,
    at_item: DefId,
    ty: &ast::Type,
) -> super::Result<Type> {
    match ty {
        ast::Type::Path(path) => match defs.find_type(curmod, &path.body, at_item) {
            Ok(defid) => Ok(Type::UserType(defid)),
            Err(e) => match &*path.segments {
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
                }] => convert_builtin_type(id).ok_or(e),
                _ => Err(e),
            },
        },
        ast::Type::Reference(_, _) => todo!("reference"),
        ast::Type::Pointer(mt, ty) => {
            let ty = ty.try_copy_span(|ty| convert_type(defs, curmod, at_item, ty))?;

            Ok(Type::Pointer(*mt, Box::new(ty)))
        }
        ast::Type::Array(_, _) => todo!("array"),
        ast::Type::FnType(_) => todo!(),
        ast::Type::Never => Ok(Type::Never),
        ast::Type::Tuple(tys) => {
            let mut tys = tys
                .iter()
                .map(|ty| ty.try_copy_span(|ty| convert_type(defs, curmod, at_item, ty)))
                .collect::<super::Result<Vec<_>>>()?;

            Ok(Type::Tuple(tys))
        }
    }
}
