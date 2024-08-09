#![allow(dead_code, unused_imports)]
use std::convert::Infallible;

use xlang::abi::collection::HashMap;
use xlang_frontend::{
    diagnostics::NoLocation,
    iter::with_rewinder_accept_on_continue,
    keywords,
    parse::{do_lexeme_class, do_lexeme_classes, PError, PLexeme, PLexemeClass, PResult, Parser},
    punctuation,
    span::NoHygiene,
};

use crate::lexer::XirLexer;

pub use xlang::ir;

pub struct XirParser;

keywords! {
    function => Function,
    public => Public,
    origin => Origin,
    module => Module,
    private => Private,

    atomic => Atomic,
    relaxed => Relaxed,
    release => Release,
    acquire => Acquire,
    acq_rel => AcqRel,
    seq_cst => SeqCst,
    volatile => Volatile,
    nontemporal => Nontemporal,
    freeze => Freeze,

    mutable => Mutable,

    asm => Asm,
    pure => Pure,
    transparent => Transparent,
    nostack => NoStack,
    nomem => NoMem,
    noexit => NoExit,
    class => Class,
    syntax => Syntax,
    clobbers => Clobbers,
    targets => Targets,
    late => Late,

    unicode => Unicode,

    target => Target,

    fallthrough => Fallthrough,
    cold => Cold,
    continue => Continue,

    willreturn => WillReturn,

    next => Next,

    static => Static,
    immut => Immutable,

    void => Void,
    int => Int,
    uint => UInt,
    float => Float,
    uchar => UChar,
    schar => SChar,
    min => Min,
    max => Max,
    posit => Posit,
    fixed => Fixed,
    fractbits => FractBits,
    binary => Binary,
    decimal => Decimal,
    dfloat => DFloat,
    extrange => ExtRange,
    extprecision => ExtPrecision,
    tagged => Tagged,
    product => Product,
    aligned => Aligned,
    vectorsize => VectorSize,
    nonzero => Nonzero,
    finite => Finite,
    nonnan => Nonnan,

    struct => Struct,
    union => Union,
    enum => Enum,

    sequence => Sequence,

    add => Add,
    sub => Sub,
    mul => Mul,
    div => Div,
    mod => Mod,
    bitand => BitAnd,
    bitor => BitOr,
    bitxor => BitXor,
    rsh => Rsh,
    lsh => Lsh,
    cmp => Cmp,
    cmp_lt => CmpLt,
    cmp_gt => CmpGt,
    cmp_eq => CmpEq,
    cmp_ne => CmpNe,
    cmp_ge => CmpGe,
    cmp_le => CmpLe,

    minus => Minus,
    bitnot => BitNot,
    logicnot => LogicNot,

    preinc => PreInc,
    postinc => PostInc,
    predec => PreDec,
    postdec => PostDec,

    xchg => Xchg,
    cmpxchg => CmpXchg,
    wcmpxchg => WCmpXchg,

    convert => Convert,
    weak => Weak,
    strong => Strong,
    reinterpret => Reinterpret,
    derive => Derive,
    local => Local,
    pop => Pop,
    dup => Dup,
    pivot => Pivot,

    member => Member,
    indirect => Indirect,
    aggregate => Aggregate,

    assign => Assign,
    as_rvalue => AsRvalue,

    compound_assign => CompoundAssign,
    fetch_assign => FetchAssign,

    addr_of => AddrOf,

    fence => Fence,
    begin => Begin,
    end => End,
    storage => Storage,
    select => Select,


}

punctuation! {
    -> => Arrow,
    :: => PathSep,
    : => Colon,
    ; => SemiColon,
    # => Pound,
    @ => At,
    * => Star,
}

impl Parser for XirParser {
    type Loc = NoLocation;

    type Lexer = XirLexer;

    type Keyword = Keyword;

    type Punct = Punctuation;

    type Other = Infallible;

    type Hygiene = NoHygiene;

    fn build_loc(
        &self,
        _: &xlang_frontend::span::Spanned<
            xlang_frontend::lexer::Lexeme<Self::Lexer>,
            Self::Hygiene,
        >,
    ) -> Self::Loc {
        NoLocation
    }
}

pub type Error = PError<XirParser>;
pub type SResult<T> = PResult<T, XirParser>;
pub type LexemeClass = PLexemeClass<XirParser>;
pub type Lexeme = PLexeme<XirParser>;
pub type Span = xlang_frontend::span::Span;

pub struct DiagSpanMap {
    pub item_spans: HashMap<ir::Path, Span>,
}
