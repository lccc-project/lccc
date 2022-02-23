#![allow(dead_code)] // For now

use std::collections::HashMap;

pub use crate::lex::GroupType;
use crate::{lex::Lexeme, parse::SimplePath};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroDefn {
    DeclMacro(Vec<MacroArm>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MacroArm {
    pub matchers: Vec<MacroMatcher>,
    pub expansion: Vec<MacroOutput>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroMatcher {
    RawToken(Lexeme),
    Group(GroupType, Vec<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroOutput {
    RawToken(Lexeme),
    Group(GroupType, Vec<Self>),
}

#[derive(Clone, Debug)]
pub struct Macros {
    _defns: HashMap<SimplePath, MacroDefn>,
}
