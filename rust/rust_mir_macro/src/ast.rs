use std::hash::Hash;

use xlang_frontend::symbol::Symbol;

use proc_macro::Span;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum InferId {} // No inference vars in `mir!{}`

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct DefId(pub(crate) u32);

impl core::fmt::Display for DefId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("#{}", self.0))
    }
}

impl core::fmt::Debug for DefId {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("#{}", self.0))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Spanned<T> {
    pub span: Span,
    pub body: T,
}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.body.hash(state)
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

impl<T: Eq> Eq for Spanned<T> {}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
    Less,
    Greater,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BoolAndAssign,
    BoolOrAssign,
    LeftShift,
    RightShift,
    LeftShiftAssign,
    RightShiftAssign,
    Range,
    RangeInclusive,
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    RawAddrOf(Spanned<Mutability>),
    AddrOf(Option<Spanned<Mutability>>),
    Deref,
    Neg,
    Not,
    RangeFrom,
    RangeTo,
    RangeToInclusive,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Safety {
    Unsafe,
    Safe,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstExpr {
    IntConst(IntType, u128),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum StringType {
    Default,
    Raw(u8), // number of #s
    Byte,
    RawByte(u8), // see above
}

macro_rules! def_intrinsics {
    {
        $($(unsafe $(@$_vol:tt)?)? intrin $name:ident $(<$($gen_param:ident),* $(,)?>)?($($param:ty),* $(,)?) -> $retty:ty;)*
    } => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum IntrinsicDef {
            $($name),*
        }

        impl IntrinsicDef {
            pub fn from_name(s: &str) -> Option<IntrinsicDef> {
                match s{
                    $(::core::stringify!($name) => Some(Self::$name),)*
                    _ => None
                }
            }

            pub fn name(&self) -> &'static str {
                match self{
                    $(Self::$name => ::core::stringify!($name)),*
                }
            }

        }
    }
}

include!("../../src/sema/intrin_defs.rs");

include!("../../src/sema/ty_defs.rs");
include!("../../src/sema/mir_defs.rs");
