#![allow(dead_code)] // For now

use xlang::abi::collection::{HashMap, HashSet};

pub use crate::lex::GroupType;
use crate::{
    lex::Lexeme,
    parse::{Item, SimplePath, Visibility},
};

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
            arms,
        } => {
            let mut path = path;
            let mut decl = DeclMacro {
                exported: false,
                arms: arms.clone(),
            };
            for attr in attrs {
                match attr {
                    crate::parse::Meta::Ident(SimplePath {
                        idents,
                        root: false,
                    }) if idents[..] == ["macro_export"] => {
                        path.idents.truncate(1);
                        decl.exported = true;
                    }
                    _ => {}
                }
            }
            if let Some(Visibility::Pub) = visibility {
                decl.exported = true;
            }
            path.idents.push(name.clone());
            if !macros.defns.get(&path).is_some() {
                macros.defns.insert(path, MacroDefn::DeclMacro(decl));
                ret = Ok(());
            }
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
