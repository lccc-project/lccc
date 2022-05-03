#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use std::convert::TryFrom;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

use xlang_abi::prelude::v1::*;
use xlang_targets::Target;

#[doc(hidden)]
pub mod macros;

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    Root,
    Text(String),
    SpecialComponent(String),
}

impl core::fmt::Display for PathComponent {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Root => Ok(()),
            Self::Text(st) => f.write_str(st),
            Self::SpecialComponent(comp) => f.write_fmt(format_args!("#{:?}", comp)),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Path {
    pub components: Vec<PathComponent>,
}

impl core::fmt::Display for Path {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let mut iter = self.components.iter();
        iter.next().unwrap_or(&PathComponent::Root).fmt(f)?;

        for i in iter {
            f.write_str("::")?;
            i.fmt(f)?;
        }
        Ok(())
    }
}

#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnnotationItem {
    Identifier(Path),
    Value(Path, Value),
    Meta(Path, Vec<Self>),
}

impl core::fmt::Display for AnnotationItem {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            AnnotationItem::Identifier(id) => id.fmt(f),
            AnnotationItem::Value(id, val) => f.write_fmt(format_args!("{} = {}", id, val)),
            AnnotationItem::Meta(id, rest) => {
                id.fmt(f)?;
                f.write_str("(")?;
                let mut sep = "";
                for a in rest {
                    f.write_str(sep)?;
                    a.fmt(f)?;
                    sep = ", ";
                }
                f.write_str(")")
            }
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation {
    pub inner: AnnotationItem,
}

impl core::fmt::Display for Annotation {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("#[")?;
        self.inner.fmt(f)?;
        f.write_str("]")
    }
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
    Static(StaticDefinition),
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Linkage {
    External,
    Internal,
    Constant,
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct StaticDefinition {
    pub ty: Type,
    pub init: Value,
    pub linkage: Linkage,
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
    pub vectorsize: Option<u16>,
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
    Integer {
        signed: bool,
        min: Option<i128>,
        max: Option<i128>,
    },
    Float {
        decimal: bool,
    },
    // long double
    LongFloat,
    Fixed {
        fractbits: u16,
    },
    Char {
        flags: CharFlags,
    },
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

impl core::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self.kind {
            ScalarTypeKind::Empty => panic!(),
            ScalarTypeKind::Integer { signed, min, max } => {
                if !signed {
                    f.write_str("u")?;
                }

                f.write_str("int")?;

                if let Some(min) = min {
                    f.write_str(" min")?;
                    f.write_str("(")?;
                    min.fmt(f)?;
                    f.write_str(")")?;
                }
                if let Some(max) = max {
                    f.write_str(" max")?;
                    f.write_str("(")?;
                    max.fmt(f)?;
                    f.write_str(")")?;
                }
            }
            ScalarTypeKind::Float { decimal } => {
                if decimal {
                    f.write_str("dec")?;
                }
                f.write_str("float")?;
            }
            ScalarTypeKind::LongFloat => f.write_str("lfloat")?,
            ScalarTypeKind::Fixed { fractbits } => {
                f.write_str("fixed")?;
                f.write_str("(")?;
                fractbits.fmt(f)?;
                f.write_str(")")?;
            }
            ScalarTypeKind::Char { flags } => {
                if flags.contains(CharFlags::SIGNED) {
                    f.write_str("s")?;
                }
                f.write_str("char")?;

                if flags.contains(CharFlags::UNICODE) {
                    f.write_str(" utf")?;
                }
            }
        }

        if self.header.validity.contains(ScalarValidity::NONZERO) {
            f.write_str(" nonzero")?;
        }

        if self.header.validity.contains(ScalarValidity::FINITE) {
            f.write_str(" finite")?;
        } else if self.header.validity.contains(ScalarValidity::NONNAN) {
            f.write_str(" nonnan")?;
        }

        if let Some(vectorsize) = self.header.vectorsize {
            f.write_str(" vectorsize")?;
            f.write_str("(")?;
            vectorsize.fmt(f)?;
            f.write_str(")")?;
        }

        f.write_str("(")?;
        self.header.bitsize.fmt(f)?;
        f.write_str(")")
    }
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
    Named(Path),
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Null => Ok(()),
            Self::Scalar(st) => st.fmt(f),
            Self::Void => f.write_str("void()"),
            Self::TaggedType(tag, ty) => {
                f.write_str("tagged(")?;
                tag.fmt(f)?;
                f.write_str(") ")?;
                ty.fmt(f)
            }
            Self::Product(tys) => {
                f.write_str("product(")?;
                let mut iter = tys.iter();
                if let core::option::Option::Some(first) = iter.next() {
                    first.fmt(f)?;
                }
                for ty in iter {
                    f.write_str(", ")?;
                    ty.fmt(f)?;
                }

                f.write_str(")")
            }
            Self::FnType(fnty) => fnty.fmt(f),
            Self::Pointer(pty) => pty.fmt(f),
            Self::Array(arrty) => {
                f.write_str("[")?;
                arrty.ty.fmt(f)?;
                f.write_str(";")?;
                arrty.len.fmt(f)?;
                f.write_str("]")
            }
            Self::Named(name) => f.write_fmt(format_args!("({})", name)),
            Self::Aggregate(defn) => defn.fmt(f),
            Self::Aligned(alignment, base) => {
                f.write_fmt(format_args!("aligned({}) {}", alignment, base))
            }
        }
    }
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
    pub annotations: AnnotatedElement,
    pub kind: AggregateKind,
    pub fields: Vec<Pair<String, Type>>,
}

impl core::fmt::Display for AggregateDefinition {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self.kind {
            AggregateKind::Struct => f.write_str("struct")?,
            AggregateKind::Union => f.write_str("union")?,
        }

        for a in &self.annotations.annotations {
            f.write_str(" ")?;
            a.fmt(f)?;
        }

        f.write_str("{")?;
        let mut sep = " ";
        for Pair(name, ty) in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            f.write_str(name)?;
            f.write_str(": ")?;
            ty.fmt(f)?;
        }
        f.write_str(" }")
    }
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

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Default, Hash)]
    pub enum struct PointerKind{
        Default = 0,
        Near = 1,
        Far = 2,
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct PointerType {
    pub alias: PointerAliasingRule,
    pub valid_range: Pair<ValidRangeType, u64>,
    pub decl: PointerDeclarationType,
    pub kind: PointerKind,
    pub addr_space: u32,
    pub inner: Box<Type>,
}

impl core::fmt::Display for PointerType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("*")?;

        if self.decl.contains(PointerDeclarationType::REF) {
            f.write_str("ref ")?;
        }
        if self.decl.contains(PointerDeclarationType::VOLATILE) {
            f.write_str("volatile ")?;
        }
        if self.decl.contains(PointerDeclarationType::CONST) {
            f.write_str("const ")?;
        }

        if self.alias.contains(PointerAliasingRule::UNIQUE) {
            f.write_str("unique ")?;
        }
        if self.alias.contains(PointerAliasingRule::READ_ONLY) {
            f.write_str("readonly ")?;
        }
        if self.alias.contains(PointerAliasingRule::READ_SHALLOW) {
            f.write_str("read_shallow ")?;
        }
        if self.alias.contains(PointerAliasingRule::INVALID) {
            f.write_str("invalid ")?;
        }
        if self.alias.contains(PointerAliasingRule::NONNULL) {
            f.write_str("nonnull ")?;
        }
        if self.alias.contains(PointerAliasingRule::NULL_OR_INVALID) {
            f.write_str("null_or_invalid ")?;
        }

        match self.valid_range {
            Pair(ValidRangeType::None, _) => {}
            Pair(ValidRangeType::Dereference, n) => {
                f.write_fmt(format_args!("dereferenceable({}) ", n))?;
            }
            Pair(ValidRangeType::DereferenceWrite, n) => {
                f.write_fmt(format_args!("dereference_write({}) ", n))?;
            }
            Pair(ValidRangeType::WriteOnly, n) => {
                f.write_fmt(format_args!("write_only({}) ", n))?;
            }
            Pair(ValidRangeType::NullOrDereference, n) => {
                f.write_fmt(format_args!("null_or_dereferenceable({}) ", n))?;
            }
            Pair(ValidRangeType::NullOrDereferenceWrite, n) => {
                f.write_fmt(format_args!("null_or_dereference_write({}) ", n))?;
            }
            Pair(ValidRangeType::NullOrWriteOnly, n) => {
                f.write_fmt(format_args!("null_or_write_only({}) ", n))?;
            }
            Pair(ty, n) => panic!("{:?}({}) is invalid", ty, n),
        }

        if self.addr_space != 0 {
            f.write_fmt(format_args!("addr_space({}) ", self.addr_space))?;
        }

        match self.kind {
            PointerKind::Default => {}
            PointerKind::Near => f.write_str("near ")?,
            PointerKind::Far => f.write_str("far ")?,
            kind => panic!("{:?} is invalid", kind),
        }

        self.inner.fmt(f)
    }
}

#[repr(C)]
#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct FnType {
    pub ret: Type,
    pub params: Vec<Type>,
    pub variadic: bool,
    pub tag: Abi,
}

impl core::fmt::Display for FnType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("function(")?;

        let mut params = self.params.iter();

        if let core::option::Option::Some(first) = params.next() {
            first.fmt(f)?;
            for param in params {
                f.write_str(", ")?;
                param.fmt(f)?;
            }

            if self.variadic {
                f.write_str(",...")?;
            }
        } else if self.variadic {
            f.write_str("...")?;
        }
        f.write_str(")")?;
        f.write_str(" -> ")?;
        self.ret.fmt(f)
    }
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

impl core::fmt::Display for StringEncoding {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Utf8 => f.write_str("utf8"),
            Self::Utf16LE => f.write_str("utf16le"),
            Self::Utf16BE => f.write_str("utf16be"),
            Self::Wtf8 => f.write_str("wtf8"),
            Self::Wtf16LE => f.write_str("wtf16le"),
            Self::Wtf16BE => f.write_str("wtf16be"),
            Self::Utf32LE => f.write_str("utf32le"),
            Self::Utf32BE => f.write_str("utf32be"),
        }
    }
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
    LabelAddress(u32),
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Value::Invalid(ty) => f.write_fmt(format_args!("invalid {}", ty)),
            Value::Uninitialized(ty) => f.write_fmt(format_args!("uninit {}", ty)),
            Value::GenericParameter(n) => f.write_fmt(format_args!("%{}", n)),
            Value::Integer { ty, val } => f.write_fmt(format_args!("{} {}", ty, val)),
            Value::GlobalAddress { ty, item } => {
                f.write_fmt(format_args!("global_address {} ({})", item, ty))
            }
            Value::ByteString { content } => match core::str::from_utf8(content) {
                Ok(s) => f.write_fmt(format_args!(" \"{}\"", s.escape_default())),
                Err(mut err) => {
                    let mut bytes = &content[..];
                    f.write_str(" \"")?;
                    while !bytes.is_empty() {
                        let (left, right) = bytes.split_at(err.valid_up_to());
                        core::str::from_utf8(left)
                            .unwrap()
                            .escape_default()
                            .fmt(f)?;
                        if let core::option::Option::Some(len) = err.error_len() {
                            let (err_bytes, rest) = right.split_at(len);
                            bytes = rest;
                            for b in err_bytes {
                                f.write_fmt(format_args!("\\x{:02x}", b))?;
                            }
                        } else {
                            let err_bytes = core::mem::take(&mut bytes);
                            for b in err_bytes {
                                f.write_fmt(format_args!("\\x{:02x}", b))?;
                            }
                        }
                        match core::str::from_utf8(bytes) {
                            Ok(s) => {
                                s.escape_default().fmt(f)?;
                                break;
                            }
                            Err(next_err) => err = next_err,
                        }
                    }
                    f.write_str("\"")
                }
            },
            Value::String { encoding, utf8, ty } => f.write_fmt(format_args!(
                "{} {} {}",
                ty,
                encoding,
                utf8.escape_default()
            )),
            Value::LabelAddress(n) => f.write_fmt(format_args!("label_address @{}", n)),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct BinaryOp {
        Add = 0,
        Sub = 1,
        Mul = 2,
        Div = 3,
        Mod = 4,
        BitAnd = 5,
        BitOr = 6,
        BitXor = 7,
        Rsh = 8,
        Lsh = 9,

        CmpInt = 10,
        CmpLt = 11,
        CmpGt = 12,
        CmpEq = 13,
        CmpNe = 14,
        CmpGe = 15,
        CmpLe = 16,
        Cmp = 17,
    }
}

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::Add => f.write_str("add"),
            Self::Sub => f.write_str("sub"),
            Self::Mul => f.write_str("mul"),
            Self::Div => f.write_str("div"),
            Self::Mod => f.write_str("mod"),
            Self::BitAnd => f.write_str("bit_and"),
            Self::BitOr => f.write_str("bit_or"),
            Self::BitXor => f.write_str("bit_xor"),
            Self::Rsh => f.write_str("rsh"),
            Self::CmpInt => f.write_str("cmp_int"),
            Self::CmpLt => f.write_str("cmp_lt"),
            Self::CmpGt => f.write_str("cmp_gt"),
            Self::CmpLe => f.write_str("cmp_le"),
            Self::CmpGe => f.write_str("cmp_ge"),
            Self::CmpEq => f.write_str("cmp_eq"),
            Self::CmpNe => f.write_str("cmp_ne"),
            Self::Cmp => f.write_str("cmp"),
            val => todo!("Invalid Operand {:?}", val),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct UnaryOp {
        Minus = 0,
        BitNot = 1,
        LogicNot = 2,
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct OverflowBehaviour{
        Wrap = 0,
        Trap = 1,
        Checked = 2,
        Unchecked = 3,
        Saturate = 4,
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct LValueOp{
        Xchg = 0,
        Cmpxchg = 1,
        Wcmpxchg = 2,
        PreInc = 3,
        PostInc = 4,
        PreDec = 5,
        PostDec = 6,
    }
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

    /// Exits the function
    ///
    /// # Stack
    ///
    /// Type Checking: [..,T1,T2,...,Tn]=>diverged
    ///
    /// Operands: [..,v1,v2,...,vn]=>diverged
    Exit {
        values: u16,
    },

    BinaryOp(BinaryOp, OverflowBehaviour),
    UnaryOp(UnaryOp, OverflowBehaviour),
    CallFunction(FnType),
    Branch {
        cond: BranchCondition,
        target: u32,
    },
    BranchIndirect,
    Convert(ConversionStrength, Type),
    Derive(PointerType, Box<Self>),
    Local(u32),
    Pop(u32),
    Dup(u32),
    Pivot(u32, u32),
    Aggregate(AggregateCtor),
    Member(String),
    MemberIndirect(String),
    Assign(AccessClass),
    AsRValue(AccessClass),
    CompoundAssign(BinaryOp, OverflowBehaviour, AccessClass),
    LValueOp(LValueOp, OverflowBehaviour, AccessClass),
    Indirect,
    AddrOf,
    Sequence(AccessClass),
    Fence(AccessClass),
    Switch(Switch),
    Tailcall(FnType),
    Asm(AsmExpr),
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AsmConstraint {
    AnyRegister,
    AnyOfClass(String),
    Register(String),
    Flag(String),
    Memory,
    AnyVolatile(String),
    AnyNonVolatile(String),
}

bitflags::bitflags! {
    #[repr(transparent)]
    pub struct AsmOptions : u32{
        const VOLATILE = 0x00000001;
        const TRANSPARENT = 0x00000002;
        const NOSTACK = 0x00000004;
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AsmExpr {
    pub opts: AsmOptions,
    pub annotations: AnnotatedElement,
    pub clobbers: Vec<AsmConstraint>,
    pub inputs: Vec<AsmConstraint>,
    pub outputs: Vec<AsmConstraint>,
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
            0 => {}
            1 => f.write_fmt(format_args!("{}atomic relaxed", sep))?,
            2 => f.write_fmt(format_args!("{}atomic release", sep))?,
            3 => f.write_fmt(format_args!("{}atomic acquire", sep))?,
            4 => f.write_fmt(format_args!("{}atomic acq_rel", sep))?,
            5 => f.write_fmt(format_args!("{}atomic seq_cst", sep))?,
            x @ 6..=16 => panic!("unknown atomic({})", x),
            _ => unreachable!(),
        }
        if (bits & 0x80) != 0 {
            f.write_str(" ")?;
            f.write_str("fail relaxed")?;
        }
        Ok(())
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
        self.0 |= rhs.0;
    }
}
impl BitAndAssign for AccessClass {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u32)]
pub enum Switch {
    Hash(HashSwitch),
    Linear(LinearSwitch),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HashSwitch {
    pub cases: Vec<Pair<Value, u32>>,
    pub default: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LinearSwitch {
    pub ty: Type,
    pub min: u128,
    pub scale: u32,
    pub default: u32,
    pub cases: Vec<u32>,
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

impl core::fmt::Display for StackItem {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if StackValueKind::LValue == self.kind {
            f.write_str("lvalue ")?;
        }

        self.ty.fmt(f)
    }
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

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash,Default)]
    pub enum struct Abi {
        C = 0,
        Cdecl = 1,
        Fastcall = 2,
        Stdcall = 3,
        Vectorcall = 4,
        Thiscall = 5,
        SysV = 6,
    }
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
