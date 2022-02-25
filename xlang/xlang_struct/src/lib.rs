#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use std::convert::TryFrom;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

use xlang_abi::prelude::v1::*;
use xlang_targets::Target;

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    Root,
    Text(String),
    SpecialComponent(String),
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Path {
    pub components: Vec<PathComponent>,
}

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnnotationItem {
    Meta(Box<Annotation>),
    Identifier(Path),
    Value(Value),
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Annotation {
    pub items: Vec<AnnotationItem>,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct AnnotatedElement {
    pub annotations: Vec<Annotation>,
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Visibility {
    Public = 0,
    Origin = 1,
    Module = 2,
    Private = 3,
    None = 4,
}

impl Default for Visibility {
    fn default() -> Self {
        Self::Public
    }
}

#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct ScopeMember {
    pub annotations: AnnotatedElement,
    pub vis: Visibility,
    pub member_decl: MemberDeclaration,
}

#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct Scope {
    pub annotations: AnnotatedElement,
    pub members: HashMap<Path, ScopeMember>,
}

#[repr(u32)]
#[derive(Clone, Debug)]
pub enum MemberDeclaration {
    Empty,
    Scope(Scope),
    Function(FunctionDeclaration),
    OpaqueAggregate(AggregateKind),
    AggregateDefinition(AggregateDefinition),
}

impl Default for MemberDeclaration {
    fn default() -> Self {
        Self::Empty
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default)]
    pub struct ScalarValidity : u8{
        const NONZERO = 0x01;
        const FINITE = 0x02;
        const NONNAN = 0x04;
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ScalarTypeHeader {
    pub bitsize: u16,
    pub vectorsize: u16,
    pub validity: ScalarValidity,
}
bitflags::bitflags! {
    #[repr(transparent)]
    pub struct CharFlags: u8{
        const SIGNED = 1;
        const UNICODE = 2;
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ScalarTypeKind {
    Empty,
    Integer { signed: bool, min: i128, max: i128 },
    Float { decimal: bool },
    // long double
    LongFloat,
    Fixed { fractbits: u16 },
    Char { flags: CharFlags },
}

impl Default for ScalarTypeKind {
    fn default() -> Self {
        Self::Empty
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ScalarType {
    pub header: ScalarTypeHeader,
    pub kind: ScalarTypeKind,
}

impl TryFrom<Type> for ScalarType {
    type Error = ();
    fn try_from(other: Type) -> std::result::Result<Self, ()> {
        if let Type::Scalar(sty) = other {
            Ok(sty)
        } else {
            Err(())
        }
    }
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Null,
    Scalar(ScalarType),
    Void,
    FnType(Box<FnType>),
    Pointer(PointerType),
    Array(Box<ArrayType>),
    TaggedType(u16, Box<Self>),
    Product(Vec<Self>),
    Aligned(Box<Value>, Box<Self>),
    Aggregate(AggregateDefinition),
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AggregateKind {
    Struct,
    Union,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateDefinition {
    pub anotations: AnnotatedElement,
    pub kind: AggregateKind,
    pub fields: Vec<Pair<String, Type>>,
}

impl Default for Type {
    fn default() -> Self {
        Self::Null
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType {
    pub ty: Type,
    pub len: Value,
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default)]
    pub struct PointerAliasingRule : u32{
        const UNIQUE = 2;
        const READ_ONLY = 4;
        const READ_SHALLOW = 8;
        const INVALID = 16;
        const NONNULL = 32;
        const VOLATILE = 64;
        const VOLATILE_WRITE = 128;
        const NULL_OR_INVALID = 256;
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Default,Hash)]
    pub enum struct ValidRangeType{
        None = 0,
        Dereference = 1,
        DereferenceWrite = 2,
        WriteOnly = 3,
        NullOrDereference = 4,
        NullOrDereferenceWrite = 5,
        NullOrWriteOnly = 6,
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default)]
    pub struct PointerDeclarationType : u16{
        const REF = 1;
        const CONST = 2;
        const VOLATILE = 4;
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct PointerType {
    pub alias: PointerAliasingRule,
    pub valid_range: Pair<ValidRangeType, u64>,
    pub decl: PointerDeclarationType,
    pub inner: Box<Type>,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FnType {
    pub ret: Type,
    pub params: Vec<Type>,
    pub tag: Abi,
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StringEncoding {
    Utf8,
    Utf16BE,
    Utf32BE,
    Utf16LE,
    Utf32LE,
    Wtf8,
    Wtf16BE,
    Wtf16LE,
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    Invalid(Type),
    Uninitialized(Type),
    GenericParameter(u32),
    Integer {
        ty: ScalarType,
        val: u128,
    },
    GlobalAddress {
        ty: Type,
        item: Path,
    },
    ByteString {
        content: Vec<u8>,
    },
    String {
        encoding: StringEncoding,
        utf8: String,
        ty: Type,
    },
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Rsh,
    Lsh,

    CmpInt,
    CmpLt,
    CmpGt,
    CmpEq,
    CmpNe,
    CmpGe,
    CmpLe,
    Cmp,
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    BitNot,
    LogicNot,
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BranchCondition {
    Always,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Never,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConversionStrength {
    Strong,
    Weak,
    Reinterpret,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateCtor {
    pub ty: Type,
    pub fields: Vec<String>,
}

///
/// An xir expression/instruction
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    /// No operation
    ///
    /// # Stack
    /// Type checking: [..]=>[..]
    ///
    Null,
    /// Pushes a constant value.
    ///
    /// # Stack
    ///
    /// Type checking: [..]=>[..,T]
    ///
    /// Operands: [..]=>[..,Value]
    Const(Value),

    /// Exits the `blk`th nested block with the given number of values
    ///
    /// # Stack
    ///
    /// Type Checking: [..,T1,T2,...,Tn]=>diverged
    ///
    /// Operands: [..,v1,v2,...,vn]=>divereged
    ExitBlock {
        blk: u32,
        values: u16,
    },

    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    CallFunction(FnType),
    Branch {
        cond: BranchCondition,
        target: u32,
    },
    Convert(ConversionStrength, Type),
    Derive(PointerType, Box<Self>),
    Local(u32),
    Pop(u32),
    Dup(u32),
    Pivot(u32, u32),
    Aggregate(AggregateCtor),
    Member(String),
    MemberIndirect(String),
    Block {
        n: u32,
        block: Block,
    },
    Assign(AccessClass),
    AsRValue(AccessClass),
}

fake_enum::fake_enum! {
    #[repr(pub u8)]
    #[derive(Hash)]
    pub enum struct AccessClass{
        Normal = 0,
        AtomicRelaxed = 1,
        AtomicRelease = 2,
        AtomicAcquire = 3,
        AtomicAcqRel = 4,
        AtomicSeqCst = 5,
    }
}

#[allow(non_upper_case_globals)]
impl AccessClass {
    pub const Volatile: Self = Self(0x10);
    pub const Nontemporal: Self = Self(0x20);
    pub const Freeze: Self = Self(0x40);
    pub const AtomicFailRelaxed: Self = Self(0x80);

    pub const ATOMIC_MASK: Self = Self(0xf);
}

impl std::fmt::Display for AccessClass {
    #[allow(clippy::useless_let_if_seq)]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let atomic = self.0 & 0xf;
        let bits = self.0 & 0xf0;
        let mut sep = "";
        if (bits & 0x10) != 0 {
            f.write_str("volatile")?;
            sep = " ";
        }
        if (bits & 0x20) != 0 {
            f.write_str(sep)?;
            f.write_str("nontemporal")?;
            sep = " ";
        }
        if (bits & 0x40) != 0 {
            f.write_str(sep)?;
            f.write_str("freeze")?;
            sep = " ";
        }
        if (bits & 0x80) != 0 {
            f.write_str(sep)?;
            f.write_str("fail relaxed")?;
            sep = " ";
        }
        match atomic {
            0 => Ok(()),
            1 => f.write_fmt(format_args!("{}atomic relaxed", sep)),
            2 => f.write_fmt(format_args!("{}atomic release", sep)),
            3 => f.write_fmt(format_args!("{}atomic acquire", sep)),
            4 => f.write_fmt(format_args!("{}atomic acq_rel", sep)),
            5 => f.write_fmt(format_args!("{}atomic seq_cst", sep)),
            6..=16 => panic!("Invalid access class {:?}", self),
            _ => unreachable!(),
        }
    }
}

impl BitOr for AccessClass {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitAnd for AccessClass {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

impl BitOrAssign for AccessClass {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0
    }
}
impl BitAndAssign for AccessClass {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StackValueKind {
    LValue,
    RValue,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StackItem {
    pub ty: Type,
    pub kind: StackValueKind,
}

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BlockItem {
    Expr(Expr),
    Target { num: u32, stack: Vec<StackItem> },
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Abi {
    C,
    Cdecl,
    Fastcall,
    Stdcall,
    Vectorcall,
    Thiscall,
    SysV,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionBody {
    pub locals: Vec<Type>,
    pub block: Block,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub ty: FnType,
    pub body: Option<FunctionBody>,
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct File {
    pub target: Target,
    pub root: Scope,
}
