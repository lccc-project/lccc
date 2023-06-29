pub use crate::ast::{Mutability, Safety, Spanned};
use crate::{
    ast::{self, PathPrefix},
    interning::Symbol,
};

pub use super::DefId;
use super::Definitions;
use crate::feature::Features;

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct IntType {
    pub signed: bool,
    pub width: IntWidth,
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
        if self.safety.body == Safety::Unsafe {
            f.write_str("unsafe ")?;
        }

        if self.constness.body == Mutability::Const {
            f.write_str("const ")?;
        }

        if self.asyncness.body == AsyncType::Async {
            f.write_str("async ")?;
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
        f.write_str(")")?;

        self.retty.body.fmt(f)
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
        }
    }
}

fn convert_builtin_type(name: &str) -> Option<Type> {
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
            Err(e) => match (&path.prefix, &*path.segments) {
                (
                    None
                    | Some(Spanned {
                        body: PathPrefix::SimplePrefix(None),
                        ..
                    }),
                    [seg],
                ) => {
                    if seg.generics.is_none() {
                        convert_builtin_type(&seg.ident).ok_or(e)
                    } else {
                        Err(e)
                    }
                }
                _ => Err(e),
            },
        },
        ast::Type::Reference(_, _) => todo!("reference"),
        ast::Type::Pointer(_, _) => todo!("pointer"),
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
