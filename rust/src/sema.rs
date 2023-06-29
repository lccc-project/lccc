use xlang::abi::collection::HashMap;
use xlang::abi::collection::HashSet;

use crate::ast;
pub use crate::ast::Attr;

pub use crate::ast::Spanned;

use crate::helpers::FetchIncrement;
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
pub struct SemaHint {
    pub text: String,
    pub itemref: DefId,
    pub refspan: Span,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub text: String,
    pub category: ErrorCategory,
    pub at_item: DefId,
    pub containing_item: DefId,
    pub relevant_item: DefId,
    pub hints: Vec<SemaHint>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ErrorCategory {
    Other,
    Unstable,
    CannotFindName,
    InvisibleEntity,
    AlreadyDefined,
}

pub mod ty;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct DefId(u32);

impl DefId {
    pub const ROOT: DefId = DefId(0);
}

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
    pub parent: DefId,
    pub attrs: Vec<Spanned<Attr>>,
    pub inner: Spanned<DefinitionInner>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefinitionInner {
    Placeholder,
    Module(Module),
    IncompleteType,
    IncompletAlias,
    ExternalFunction(FnType),
    IncompleteFunction(FnType),
    UseName(DefId),
    UseWildcard(DefId),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    pub types: HashMap<Symbol, DefId>,
    pub values: HashMap<Symbol, DefId>,
    pub wildcard_imports: Vec<DefId>,
}

pub struct Definitions {
    crates: HashMap<Symbol, DefId>,
    curcrate: DefId,
    defs: HashMap<DefId, Definition>,
    lang_items: HashMap<LangItem, DefId>,
    nextdefid: u32,
    prelude_import: DefId,
    visibility_cache: HashSet<(DefId, DefId)>,
}

impl Definitions {
    pub fn new() -> Definitions {
        Definitions {
            crates: HashMap::new(),
            curcrate: DefId::ROOT,
            defs: HashMap::new(),
            lang_items: HashMap::new(),
            nextdefid: 1,
            prelude_import: DefId::ROOT,
            visibility_cache: HashSet::new(),
        }
    }

    pub fn spanof(&self, defid: DefId) -> Span {
        self.definition(defid).inner.span
    }

    pub fn definition(&self, defid: DefId) -> &Definition {
        self.defs
            .get(&defid)
            .expect("Sharing DefId's between `Definitions` is not permitted") // This is an ICE, not a user-facing error
    }

    pub fn definition_mut(&mut self, defid: DefId) -> &mut Definition {
        self.defs
            .get_mut(&defid)
            .expect("Sharing DefId's between `Definitions` is not permitted") // This is an ICE, not a user-facing error
    }

    pub fn allocate_defid(&mut self) -> DefId {
        let mut def = DefId(self.nextdefid.fetch_increment());

        self.defs.insert(
            def,
            Definition {
                visible_from: DefId::ROOT,
                parent: DefId::ROOT,
                attrs: vec![],
                inner: Spanned {
                    body: DefinitionInner::Placeholder,
                    span: Span::empty(),
                },
            },
        );

        def
    }

    pub fn set_current_crate(&mut self, curcrate: DefId) {
        self.curcrate = curcrate;
    }

    pub fn assign_type_id(&mut self, curmod: DefId, iname: Symbol, span: Span) -> Result<DefId> {
        let ndef = self.allocate_defid();
        self.definition_mut(ndef).inner.span = span;
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition_mut(curmod)
        {
            if let Some(&id) = md.types.get(&iname) {
                return Err(Error {
                    span,
                    text: format!("Item {} is already defined", iname),
                    category: ErrorCategory::AlreadyDefined,
                    at_item: ndef,
                    containing_item: curmod,
                    relevant_item: id,
                    hints: vec![SemaHint {
                        text: format!("Item {} previously defined here", iname),
                        itemref: id,
                        refspan: self.spanof(id),
                    }],
                });
            }

            assert!(md.types.insert(iname, ndef).is_none());

            Ok(ndef)
        } else {
            panic!("Expected a module for defid {}", curmod)
        }
    }

    fn visibility_matches(&self, vis: DefId, curmod: DefId) -> bool {
        if vis == curmod {
            return true;
        } else if curmod == DefId::ROOT {
            return false;
        } else {
            self.visibility_matches(vis, self.definition(curmod).parent)
        }
    }

    pub fn visible_from(&self, item: DefId, curmod: DefId) -> bool {
        let vis = self.definition(item).visible_from;

        self.visibility_matches(vis, curmod)
    }

    pub fn find_type_in_mod(
        &self,
        curmod: DefId,
        searchmod: DefId,
        id: Spanned<Symbol>,
        at_item: DefId,
    ) -> Result<DefId> {
        if searchmod == DefId::ROOT {
            self.crates.get(&id.body).copied().ok_or_else(|| Error {
                span: id.span,
                text: format!("Cannot find crate {}", id.body),
                category: ErrorCategory::CannotFindName,
                at_item,
                containing_item: curmod,
                relevant_item: DefId::ROOT,
                hints: vec![],
            })
        } else {
            if let Definition {
                inner:
                    Spanned {
                        body: DefinitionInner::Module(md),
                        ..
                    },
                ..
            } = self.definition(searchmod)
            {
                let mut def = match md.types.get(&id.body) {
                    Some(id) => *id,
                    None => {
                        let mut found = Vec::new();
                        for &usestar in &md.wildcard_imports {
                            if !self.visible_from(usestar, curmod) {
                                continue;
                            }
                            if let Ok(id) = self.find_type_in_mod(curmod, usestar, id, at_item) {
                                found.push(id);
                            }
                        }

                        match found.len() {
                            0 => {
                                return Err(Error {
                                    span: id.span,
                                    text: format!("Cannot find item {}", id.body),
                                    category: ErrorCategory::CannotFindName,
                                    at_item,
                                    containing_item: curmod,
                                    relevant_item: searchmod,
                                    hints: vec![],
                                })
                            }
                            1 => found[0],
                            _ => {
                                return Err(Error {
                                    span: id.span,
                                    text: format!("Item {} is ambiguous", id.body),
                                    category: ErrorCategory::CannotFindName,
                                    at_item,
                                    containing_item: curmod,
                                    relevant_item: searchmod,
                                    hints: vec![],
                                })
                            }
                        }
                    }
                };

                if !self.visible_from(def, curmod) {
                    return Err(Error {
                        span: id.span,
                        text: format!("Cannot find item {}", id.body),
                        category: ErrorCategory::InvisibleEntity,
                        at_item,
                        containing_item: curmod,
                        relevant_item: searchmod,
                        hints: vec![],
                    });
                }

                while let Definition {
                    inner:
                        Spanned {
                            body: DefinitionInner::UseName(actual),
                            ..
                        },
                    ..
                } = self.definition(def)
                {
                    def = *actual;
                }

                Ok(def)
            } else {
                panic!("Expected defid {} to be a module", searchmod);
            }
        }
    }

    pub fn find_type(&self, curmod: DefId, path: &ast::Path, at_item: DefId) -> Result<DefId> {
        let mut resolve_mod = match &path.prefix {
            None => None,
            Some(Spanned {
                body: ast::PathPrefix::SimplePrefix(prefix),
                ..
            }) => Some(match prefix {
                None => DefId::ROOT,
                Some(Spanned {
                    body: ast::SimplePathSegment::CratePath,
                    ..
                }) => self.curcrate,
                Some(Spanned {
                    body: ast::SimplePathSegment::SelfPath,
                    ..
                }) => curmod,
                Some(Spanned {
                    body: ast::SimplePathSegment::SuperPath,
                    ..
                }) => self.definition(curmod).parent,
                Some(Spanned { body, span }) => panic!("Unexpected prefix for path {:?}", body),
            }),
            _ => todo!("Complex paths"),
        };

        for seg in &path.segments {
            if let Some(md) = resolve_mod {
                resolve_mod = Some(self.find_type_in_mod(curmod, md, seg.body.ident, at_item)?);
            } else {
                match self.find_type_in_mod(curmod, curmod, seg.body.ident, at_item) {
                    Ok(id) => resolve_mod = Some(id),
                    Err(e) => {
                        if self.prelude_import != DefId::ROOT {
                            if let Ok(id) = self.find_type_in_mod(
                                curmod,
                                self.prelude_import,
                                seg.body.ident,
                                at_item,
                            ) {
                                resolve_mod = Some(id);
                                continue;
                            }
                        }

                        if let Some(id) = self.crates.get(&seg.body.ident.body) {
                            resolve_mod = Some(*id);
                            continue;
                        } else {
                            return Err(e);
                        }
                    }
                }
            }
            if let Some(generics) = &seg.generics {
                todo!("Generics")
            }
        }

        Ok(resolve_mod.expect("Empty path is not allowed"))
    }
}
