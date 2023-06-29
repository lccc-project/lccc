use xlang::abi::collection::HashMap;

pub use crate::ast::Attr;

pub use crate::ast::Spanned;

use crate::interning::Symbol;
use crate::lang::LangItem;
use crate::span::Span;

use self::ty::FnType;

macro_rules! as_simple_component {
    (self) => {
        $crate::ast::SimplePathSegment::SelfPath
    };
    (super) => {
        $crate::ast::SimplePathSegment::SuperPath
    };
    (crate) => {
        $crate::ast::SimplePathSegment::CratePath
    };
    ($id:ident) => {
        $crate::ast::SimplePathSegment::Identifier($crate::interning::Symbol::intern(
            ::core::stringify!($id),
        ))
    };
}

macro_rules! matches_simple_path{
    ($e:expr, :: $($id:ident)::+) => {
        {
            let path = [$(as_simple_component!($id)),*];

            let to_match: &$crate::ast::SimplePath = &($e);

            to_match.from_root && to_match.segments.iter().map(|seg| seg.body)
                .zip(&path)
                .all(|(a,b)| a==b)
        }
    };
    ($e:expr, $($id:ident)::+) => {
        {
            let path = [$(as_simple_component!($id)),*];

            let to_match: &$crate::ast::SimplePath = &($e);

            !to_match.from_root && to_match.segments.iter().map(|seg| seg.body)
                .zip(&path)
                .all(|(a,b)| a==b)
        }
    };
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub text: String,
    pub category: ErrorCategory,
    pub at_item: DefId,
    pub containing_item: DefId,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ErrorCategory {
    Other,
    Unstable,
    CannotFindName,
    InvisibleEntity,
}

pub mod ty;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct DefId(u32);

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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Definition {
    pub visible_from: DefId,
    pub attrs: Vec<Spanned<Attr>>,
    pub inner: Spanned<DefinitionInner>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefinitionInner {
    Module(Module),
    IncompleteType,
    IncompletAlias,
    ExternalFunction(FnType),
    IncompleteFunction(FnType),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    pub types: HashMap<Symbol, DefId>,
    pub values: HashMap<Symbol, DefId>,
}

pub struct Definitions {
    crates: HashMap<Symbol, DefId>,
    curcrate: DefId,
    defs: HashMap<DefId, Definition>,
    lang_items: HashMap<LangItem, DefId>,
}
