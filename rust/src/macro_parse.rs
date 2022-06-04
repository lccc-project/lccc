#![allow(dead_code)] // For now

use peekmore::PeekMoreIterator;
use xlang::abi::collection::{HashMap, HashSet};

pub use crate::lex::GroupType;
use crate::{
    lex::Lexeme,
    parse::{Item, SimplePath, Visibility},
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u16)]
pub enum HygieneMode {
    CallSiteOnly = 0,
    MixedSite = 1,
    DefSiteOnly = 2,
    NoGlobals = 3,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct HygieneId {
    pub crate_id: u64,
    pub xref_index: u32,
    pub hygiene: HygieneMode,
    pub flags: u16,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroDefn {
    DeclMacro(DeclMacro),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DeclMacro {
    pub exported: bool,
    pub arms: Vec<MacroArm>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MacroArm {
    pub matchers: Vec<MacroMatcher>,
    pub expansion: Vec<MacroOutput>,
}

pub fn parse_macro_arm<I: Iterator>(it: &mut PeekMoreIterator<I>) -> MacroArm {
    todo!()
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroMatcher {
    RawToken(Lexeme),
    Group(GroupType, Vec<Self>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MacroOutput {
    RawToken(HygieneId, Lexeme),
    Group(GroupType, Vec<Self>),
    LitDollar,
    Repetition(Repetition),
    Expansion(Expansion),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Repetition {}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expansion {}

#[derive(Clone, Debug)]
pub struct Macros {
    defns: HashMap<SimplePath, MacroDefn>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NoMoreMacros;

pub fn find_macros(
    macros: &mut Macros,
    mut path: SimplePath,
    item: &Item,
) -> Result<(), NoMoreMacros> {
    let mut ret = Err(NoMoreMacros);
    match item {
        Item::ExternBlock { .. }
        | Item::MacroExpansion { .. }
        | Item::FnDeclaration { .. }
        | Item::Type(_)
        | Item::Adt { .. }
        | Item::TypeAlias { .. } => {}
        Item::MacroRules {
            attrs,
            visibility,
            name,
            content,
        } => {
            todo!("macro_rules");
        }
        Item::Mod { name, content, .. } => {
            path.idents.push(name.clone());
            for item in &content.items {
                if let Ok(()) = find_macros(macros, path.clone(), item) {
                    ret = Ok(())
                }
            }
        }
        Item::Trait { .. } => {}
        Item::Impl { .. } => {}
        Item::Static { .. } => {}
        Item::Const { .. } => {}
    }
    ret
}
