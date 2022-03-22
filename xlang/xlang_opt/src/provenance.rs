use xlang::abi::collection::HashMap;

use crate::expr::LValue;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Provenance {
    /// Empty provenance. Cannot be used to access any memory (but can access ZSTs)
    Empty,
    /// Provenance of the null pointer. Cannot be used to access memory including ZSTs
    Null,
    /// A known invalid/invalidated pointer
    Invalid,

    /// Unknown/Escaped Provenance
    Unknown,

    /// Root Object Provenance
    Root(Box<LValue>),

    /// Trace Provenance
    Trace(Trace),

    /// Union Provenance
    Union(Vec<Provenance>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Trace {
    pub edge: u32,
    pub vertex: Box<Provenance>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PGraph {
    pub vertecies: Vec<u32>,
    pub edges: HashMap<u32, (usize, usize)>,
}
