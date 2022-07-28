use std::{fmt, collections::HashMap, sync::{RwLock, Arc}};
use super::parse;

pub use parse::{Safety, Visibility};

#[derive(Debug)]
pub enum PathComponent {
    Ident(String),
}

impl fmt::Display for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => ident.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct QualPath {
    is_crate: bool,
    components: Vec<PathComponent>,
}

impl QualPath {
    pub fn crate_root() -> Self {
        Self {
            is_crate: true,
            components: Vec::new(),
        }
    }
}

impl fmt::Display for QualPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_crate {
            "crate".fmt(f)?;
        }
        for component in &self.components {
            write!(f, "::{}", component)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum PathRoot {
    CrateRoot,
    Root,
    Unqualified,
}

#[derive(Debug)]
pub struct Path {
    root: PathRoot,
    components: Vec<PathComponent>,
    resolved: QualPath,
}

#[derive(Debug)]
pub enum ScopeMember {
    Expr,
    Scope(Scope),
}

#[derive(Debug)]
pub struct Scope {
    root: QualPath,
    imports: HashMap<String, Arc<RwLock<Path>>>,
    members: HashMap<String, ScopeMember>,
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "/* Scope of `{}`:", self.root)?;
        write!(f, "*/")
    }
}

impl From<QualPath> for Scope {
    fn from(root: QualPath) -> Self {
        Scope { root, imports: HashMap::new(), members: HashMap::new(), }
    }
}

#[derive(Debug)]
pub enum DeclarationBody {
    Fn {
        safety: Safety,
    }
}

#[derive(Debug)]
pub struct Declaration {
    name: String,
    vis: Visibility,
    body: DeclarationBody,
}

#[derive(Debug)]
pub struct Mod {
    scope: Scope,
    mods: Vec<Mod>,
    decls: Vec<Declaration>,
}

impl fmt::Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "mod {} {{", self.scope.root)?;
        writeln!(f, "{}", self.scope)?;
        "}".fmt(f)
    }
}

#[derive(Debug)]
pub struct Program {
    root: Mod,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.root)
    }
}

pub fn convert_mod(module: &parse::Mod, mut scope: Scope) -> Mod {
    let mods = Vec::new();
    let mut decls = Vec::new();

    for attr in &module.attrs {
        todo!();
    }

    for item in &module.items {
        match item {
            parse::Item::FnDeclaration {
                attrs,
                visibility,
                is_const,
                is_async,
                safety,
                abi,
                name,
                generics,
                params,
                return_ty,
                block,
            } => {
                for attr in attrs {
                    todo!();
                }
                if *is_const || *is_async || !generics.params.is_empty() || !generics.where_bounds.is_empty() {
                    todo!();
                }
                scope.members.insert(name.clone(), ScopeMember::Expr);
                decls.push(Declaration {
                    name: name.clone(),
                    vis: *visibility,
                    body: DeclarationBody::Fn {
                        safety: *safety,
                    }
                })
            },
            x => todo!("{:?}", x),
        }
    }

    Mod { scope, mods, decls }
}

pub fn convert(root: &parse::Mod) -> Program {
    Program {
        root: convert_mod(root, Scope::from(QualPath::crate_root())),
    }
}
