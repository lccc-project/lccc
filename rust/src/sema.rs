use ast::Mutability;
use ast::Safety;
use xlang::abi::collection::HashMap;
use xlang::abi::collection::HashSet;
use xlang::abi::pair::Pair;

use crate::ast;
pub use crate::ast::Attr;

pub use crate::span::Spanned;

use crate::helpers::FetchIncrement;
use crate::helpers::TabPrinter;
use crate::interning::Symbol;
use crate::lang::LangItem;
use crate::sema::ty::AsyncType;
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
    InvalidAbi,
    InvalidFunction,
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
pub enum FunctionBody {
    Incomplete,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UserTypeKind {
    Struct,
    Union,
    Enum,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UserType {
    Incomplete(UserTypeKind),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefinitionInner {
    Placeholder,
    Module(Module),
    UserType(UserType),
    IncompletAlias,
    Function(FnType, Option<FunctionBody>),
    UseName(DefId),
    UseWildcard(DefId),
    ExternBlock,
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
    extern_blocks: Vec<DefId>,
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
            extern_blocks: Vec::new(),
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

    pub fn insert_type(&mut self, curmod: DefId, name: Spanned<Symbol>, item: DefId) -> Result<()> {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition_mut(curmod)
        {
            if let Some(existing) = md.types.insert(name.body, item) {
                let existingspan = self.spanof(existing);
                Err(Error {
                    span: name.span,
                    text: format!("Item {} already exists", name.body),
                    category: ErrorCategory::AlreadyDefined,
                    at_item: item,
                    containing_item: curmod,
                    relevant_item: existing,
                    hints: vec![SemaHint {
                        text: format!("Item {} previously defined here", name.body),
                        itemref: existing,
                        refspan: existingspan,
                    }],
                })
            } else {
                Ok(())
            }
        } else {
            panic!("Expected defid {} to be a module",)
        }
    }

    pub fn insert_value(
        &mut self,
        curmod: DefId,
        name: Spanned<Symbol>,
        item: DefId,
    ) -> Result<()> {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition_mut(curmod)
        {
            if let Some(existing) = md.values.insert(name.body, item) {
                let existingspan = self.spanof(existing);
                Err(Error {
                    span: name.span,
                    text: format!("Item {} already exists", name.body),
                    category: ErrorCategory::AlreadyDefined,
                    at_item: item,
                    containing_item: curmod,
                    relevant_item: existing,
                    hints: vec![SemaHint {
                        text: format!("Item {} previously defined here", name.body),
                        itemref: existing,
                        refspan: existingspan,
                    }],
                })
            } else {
                Ok(())
            }
        } else {
            panic!("Expected defid {} to be a module",)
        }
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
                panic!("Expected a module for item {}",searchmod); // TODO handle Ty::ASSOC_TYPE
            }
        }
    }

    pub fn find_type(&self, curmod: DefId, path: &ast::Path, at_item: DefId) -> Result<DefId> {
        let mut resolve_mod = None;

        match &path.root{
            Some(Spanned{body: ast::PathRoot::Root,..}) => {
                resolve_mod = Some(DefId(0));
            }
            Some(Spanned{body: ast::PathRoot::QSelf(_, _),..}) => todo!("QSelf"),
            None => {}
        }

        for seg in &path.segments{
            match &seg.ident.body{
                ast::SimplePathSegment::Identifier(id) => {
                    let id = Spanned{body: *id, span: seg.ident.span};

                    if let Some(md) = resolve_mod{
                        resolve_mod = Some(self.find_type_in_mod(curmod, md, id, at_item)?);
                    }else{
                        match self.find_type_in_mod(curmod,curmod,id,at_item){
                            Ok(item) => resolve_mod = Some(item),
                            Err(e) => {
                                if self.prelude_import.0 != 0{
                                    if let Ok(item) = self.find_type_in_mod(curmod, self.prelude_import, id, at_item){
                                        resolve_mod = Some(item)
                                    }else if let Ok(item) = self.find_type_in_mod(curmod, DefId::ROOT,id,at_item){
                                        resolve_mod = Some(item)
                                    }else{
                                        return Err(e);
                                    }
                                }else if let Ok(item) = self.find_type_in_mod(curmod, DefId::ROOT,id,at_item){
                                    resolve_mod = Some(item)
                                }else{
                                    return Err(e);
                                }
                            }
                        }

                        if let Some(generics) = &seg.generics{
                            todo!("generics")
                        }
                    }
                },
                ast::SimplePathSegment::SuperPath => {
                    if let Some(md) = resolve_mod{
                        if md==DefId::ROOT{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`super` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint{text: format!("Remove the `::`"),itemref: at_item,refspan: path.root.as_ref().unwrap().span}],
                            });
                        }else{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`super` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    }else{
                        resolve_mod = Some(self.definition(curmod).parent);
                    }

                    if let Some(generics) = &seg.generics{
                        return Err(Error{
                            span: generics.span,
                            text: format!("`super` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                },
                ast::SimplePathSegment::SelfPath => {
                    if let Some(md) = resolve_mod{
                        if md==DefId::ROOT{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`self` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint{text: format!("Remove the `::`"),itemref: at_item,refspan: path.root.as_ref().unwrap().span}],
                            });
                        }else{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`self` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    }else{
                        resolve_mod = Some(curmod);
                    }

                    if let Some(generics) = &seg.generics{
                        return Err(Error{
                            span: generics.span,
                            text: format!("`self` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                },
                ast::SimplePathSegment::CratePath => {
                    if let Some(md) = resolve_mod{
                        if md==DefId::ROOT{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`crate` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint{text: format!("Remove the `::`"),itemref: at_item,refspan: path.root.as_ref().unwrap().span}],
                            });
                        }else{
                            return Err(Error{
                                span: seg.ident.span,
                                text: format!("`crate` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    }else{
                        resolve_mod = Some(self.curcrate);
                    }

                    if let Some(generics) = &seg.generics{
                        return Err(Error{
                            span: generics.span,
                            text: format!("`crate` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                },
                ast::SimplePathSegment::MacroCratePath => todo!("$crate"), // this needs to look in the macro DefId table
            }
        }

        Ok(resolve_mod.expect("Parsing an empty path is impossible"))
    }
}

impl Definitions {
    fn display_item(
        &self,
        fmt: &mut core::fmt::Formatter,
        id: DefId,
        item_name: &str,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        let def = self.definition(id);
        let vis = def.visible_from;

        tabs.fmt(fmt)?;
        fmt.write_str("pub(in ")?;
        vis.fmt(fmt)?;
        fmt.write_str(") ")?;

        match &def.inner.body {
            DefinitionInner::Placeholder | DefinitionInner::ExternBlock => unreachable!("This should not happen"),
            DefinitionInner::Module(md) => {
                fmt.write_str("mod ")?;
                fmt.write_str(item_name)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/{\n")?;
                let nested = tabs.nest();

                for &star_mod in &md.wildcard_imports {
                    fmt.write_fmt(format_args!("{}use {}::*;\n", nested, star_mod))?;
                }

                for &Pair(name, def) in &md.types {
                    self.display_item(fmt, def, &name, nested)?;
                }
                for &Pair(name, def) in &md.values {
                    self.display_item(fmt, def, &name, nested)?;
                }
                tabs.fmt(fmt)?;
                fmt.write_str("}\n")
            }
            DefinitionInner::UserType(UserType::Incomplete(kind)) => {
                match kind {
                    UserTypeKind::Enum => fmt.write_str("enum ")?,
                    UserTypeKind::Struct => fmt.write_str("struct ")?,
                    UserTypeKind::Union => fmt.write_str("union ")?,
                }
                fmt.write_str(item_name)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/{")?;
                let nested = tabs.nest();
                nested.fmt(fmt)?;
                fmt.write_str("/*incomplete*/\n")?;
                tabs.fmt(fmt)?;
                fmt.write_str("}\n")
            }
            DefinitionInner::IncompletAlias => {
                fmt.write_str("type ")?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/ = /*incomplete*/;\n")
            }
            DefinitionInner::Function(fnty, body) => {
                if let Mutability::Const = fnty.constness.body {
                    fmt.write_str("const ")?;
                }

                if let AsyncType::Async = fnty.asyncness.body {
                    fmt.write_str("async ")?;
                }

                if let Safety::Unsafe = fnty.safety.body {
                    fmt.write_str("unsafe ")?;
                }

                fnty.tag.body.fmt(fmt)?;

                fmt.write_str(" fn ")?;

                fmt.write_str(item_name)?;

                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/(")?;

                let mut n = 0;
                let mut sep = "";

                for ty in &fnty.paramtys {
                    fmt.write_str(sep)?;
                    sep = ", ";
                    fmt.write_fmt(format_args!("_{}: {}", n.fetch_increment(), &ty.body))?;
                }

                if *fnty.iscvarargs {
                    fmt.write_str(sep)?;
                    fmt.write_fmt(format_args!("_{}: ...", n))?;
                }

                fmt.write_str(") -> ")?;
                fnty.retty.body.fmt(fmt)?;
                match body {
                    None => fmt.write_str(";\n"),
                    Some(FunctionBody::Incomplete) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        nested.fmt(fmt)?;
                        fmt.write_str("/*incomplete*/\n")?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                }
            }
            DefinitionInner::UseName(item) => {
                fmt.write_str("use ")?;
                item.fmt(fmt)?;
                fmt.write_str(" as ")?;
                fmt.write_str(item_name)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/\n")
            }
            DefinitionInner::UseWildcard(_) => unreachable!("This should never happen"),
        }
    }
}

impl core::fmt::Display for Definitions {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for &Pair(name, crdef) in &self.crates {
            f.write_str("extern crate ")?;
            f.write_str(&name)?;
            f.write_str(" /*")?;
            crdef.fmt(f)?;
            f.write_str("*/;\n")?;
        }

        f.write_str("\n")?;

        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition(self.curcrate)
        {
            let tabs = TabPrinter::new();
            for &star_mod in &md.wildcard_imports {
                f.write_fmt(format_args!("{}use {}::*;\n", tabs, star_mod))?;
            }

            for &Pair(name, def) in &md.types {
                self.display_item(f, def, &name, tabs)?;
            }
            for &Pair(name, def) in &md.values {
                self.display_item(f, def, &name, tabs)?;
            }
        } else {
            unreachable!("Expected a module for defid {}", self.curcrate);
        }

        Ok(())
    }
}

fn scan_modules(defs: &mut Definitions, curmod: DefId, md: &Spanned<ast::Mod>) -> Result<()> {
    let mut ret = Module {
        types: HashMap::new(),
        values: HashMap::new(),
        wildcard_imports: Vec::new(),
    };
    for item in &md.items {
        let span = item.span;

        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let defid = defs.allocate_defid();

                let def = defs.definition_mut(defid);
                def.parent = curmod;
                def.attrs = item.attrs.clone();

                scan_modules(defs, curmod, md.content.as_ref().unwrap())?;

                if let Some(existing) = ret.types.insert(md.name.body, defid) {
                    return Err(Error {
                        span: md.name.span,
                        text: format!("Cannot redefine module {}", md.name.body),
                        category: ErrorCategory::AlreadyDefined,
                        at_item: defid,
                        containing_item: curmod,
                        relevant_item: existing,
                        hints: vec![],
                    });
                }
            }
            _ => {}
        }
    }

    defs.definition_mut(curmod).inner = md.copy_span(|_| DefinitionInner::Module(ret));

    Ok(())
}

fn convert_visibility(
    defs: &mut Definitions,
    curmod: DefId,
    curvis: Option<&Spanned<ast::Visibility>>,
) -> Result<Option<DefId>> {
    let curvis = if let Some(curvis) = curvis {
        curvis
    } else {
        return Ok(None);
    };

    match &curvis.body {
        ast::Visibility::Pub => Ok(Some(DefId::ROOT)),
        ast::Visibility::Priv => Ok(Some(curmod)),
        ast::Visibility::Scoped(_) => todo!("pub(in)"),
    }
}

fn collect_types(defs: &mut Definitions, curmod: DefId, md: &Spanned<ast::Mod>) -> Result<()> {
    for item in &md.items {
        let visible_from = convert_visibility(defs, curmod, item.vis.as_ref())?.unwrap_or(curmod);
        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let mddef = defs
                    .find_type_in_mod(curmod, curmod, md.name, curmod)
                    .unwrap();

                collect_types(defs, mddef, md.content.as_ref().unwrap())?;

                defs.definition_mut(mddef).visible_from = visible_from;
            }
            ast::ItemBody::UserType(uty) => {
                let defid = defs.allocate_defid();
                let utys = match &uty.body.body.body {
                    ast::UserTypeBody::Struct(
                        Spanned {
                            body: ast::StructKind::Struct,
                            ..
                        },
                        _,
                    ) => UserType::Incomplete(UserTypeKind::Struct),
                    ast::UserTypeBody::Struct(
                        Spanned {
                            body: ast::StructKind::Union,
                            ..
                        },
                        _,
                    ) => UserType::Incomplete(UserTypeKind::Union),
                    ast::UserTypeBody::Enum(_) => UserType::Incomplete(UserTypeKind::Enum),
                };

                let def = defs.definition_mut(defid);
                def.attrs = item.attrs.clone();
                def.parent = curmod;
                def.inner = uty.copy_span(|_| DefinitionInner::UserType(utys));
                def.visible_from = visible_from;

                defs.insert_type(curmod, uty.name, defid)?;
            }
            ast::ItemBody::Use(_) => todo!("use"),
            ast::ItemBody::Value(_)
            | ast::ItemBody::ExternCrate { .. }
            | ast::ItemBody::Function(_)
            | ast::ItemBody::ExternBlock(_) => {}
        }
    }

    Ok(())
}

fn collect_function(
    defs: &mut Definitions,
    curmod: DefId,
    item: DefId,
    itemfn: &Spanned<ast::Function>,
    outer_tag: Option<Spanned<ty::AbiTag>>,
    outer_safety: Option<Spanned<Safety>>,
    outer_span: Option<Span>,
) -> Result<DefinitionInner> {
    let actual_tag = itemfn
        .abi
        .map(|tag| {
            Ok(Spanned {
                body: ty::convert_tag(tag, curmod, item)?,
                span: tag.span,
            })
        })
        .transpose()?;

    let default_safety = outer_safety.unwrap_or(Spanned {
        body: Safety::Safe,
        span: Span::empty(),
    });

    let tag = match (outer_tag, actual_tag) {
        (Some(outer_tag), Some(actual_tag)) => Err(Error {
            span: actual_tag.span,
            text: format!(
                "Cannot nest extern qualifiers (function declared {} in extern block declared {})",
                actual_tag.body, outer_tag.body
            ),
            category: ErrorCategory::InvalidFunction,
            at_item: item,
            containing_item: curmod,
            relevant_item: item,
            hints: vec![
                SemaHint {
                    text: format!("Declared in an extern block here"),
                    itemref: item,
                    refspan: outer_span.unwrap_or_else(Span::empty),
                },
                SemaHint {
                    text: format!("extern block has abi {}", outer_tag.body),
                    itemref: item,
                    refspan: outer_tag.span,
                },
            ],
        })?,
        (Some(tag), None) | (None, Some(tag)) => tag,
        (None, None) => Spanned {
            body: ty::AbiTag::Rust,
            span: Span::empty(),
        },
    };

    let safety = itemfn.safety.unwrap_or(default_safety);

    let constness = itemfn.constness.unwrap_or(Spanned {
        body: Mutability::Mut,
        span: Span::empty(),
    });
    let asyncness = itemfn.is_async.map_or(
        Spanned {
            body: ty::AsyncType::Normal,
            span: Span::empty(),
        },
        |isasync| Spanned {
            body: ty::AsyncType::Async,
            span: isasync.span,
        },
    );

    if outer_tag.is_some() && constness.body == Mutability::Const {
        Err(Error {
            span: constness.span,
            text: format!("Cannot declare an external const fn",),
            category: ErrorCategory::InvalidFunction,
            at_item: item,
            containing_item: curmod,
            relevant_item: item,
            hints: vec![SemaHint {
                text: format!("Declared in an extern block here"),
                itemref: item,
                refspan: outer_span.unwrap_or_else(Span::empty),
            }],
        })?;
    }
    if outer_tag.is_some() && asyncness.body == ty::AsyncType::Async {
        Err(Error {
            span: asyncness.span,
            text: format!("Cannot declare an external async fn",),
            category: ErrorCategory::InvalidFunction,
            at_item: item,
            containing_item: curmod,
            relevant_item: item,
            hints: vec![SemaHint {
                text: format!("Declared in an extern block here"),
                itemref: item,
                refspan: outer_span.unwrap_or_else(Span::empty),
            }],
        })?;
    }

    let paramtys = itemfn
        .params
        .iter()
        .map(|param| &param.ty)
        .map(|ty| ty.try_copy_span(|ty| ty::convert_type(defs, curmod, item, ty)))
        .collect::<Result<Vec<_>>>()?;

    let retty = Box::new(
        itemfn
            .ret_ty
            .as_ref()
            .map(|ty| ty.try_copy_span(|ty| ty::convert_type(defs, curmod, item, ty)))
            .transpose()?
            .unwrap_or(Spanned {
                body: ty::Type::UNIT,
                span: Span::empty(),
            }),
    );

    let iscvarargs = itemfn.varargs.as_ref().map_or(
        Spanned {
            body: false,
            span: Span::empty(),
        },
        |span| span.copy_span(|_| true),
    );

    let fnty = FnType {
        safety,
        constness,
        asyncness,
        tag,
        retty,
        paramtys,
        iscvarargs,
    };

    let fnbody = match &itemfn.body.body {
        Some(_) => Some(FunctionBody::Incomplete),
        None => None,
    };

    Ok(DefinitionInner::Function(fnty, fnbody))
}

fn collect_values(defs: &mut Definitions, curmod: DefId, md: &Spanned<ast::Mod>) -> Result<()> {
    for item in &md.items {
        let visible_from = convert_visibility(defs, curmod, item.vis.as_ref())?.unwrap_or(curmod);
        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let mddef = defs
                    .find_type_in_mod(curmod, curmod, md.name, curmod)
                    .unwrap();

                collect_values(defs, mddef, md.content.as_ref().unwrap())?;
            }
            ast::ItemBody::UserType(_) | ast::ItemBody::ExternCrate { .. } => {}
            ast::ItemBody::Use(_) => todo!("use"),
            ast::ItemBody::Value(_) => todo!("value"),
            ast::ItemBody::Function(itemfn) => {
                let defid = defs.allocate_defid();
                let inner = collect_function(defs, curmod, defid, itemfn, None, None, None)?;

                let def = defs.definition_mut(defid);
                def.attrs = item.attrs.clone();
                def.visible_from = visible_from;
                def.parent = curmod;
                def.inner = itemfn.copy_span(|_| inner);

                defs.insert_value(curmod, itemfn.name, defid)?;
            }
            ast::ItemBody::ExternBlock(blk) => {
                let defid = defs.allocate_defid();

                let def = defs.definition_mut(defid);

                def.attrs = item.attrs.clone();
                def.visible_from = curmod;
                def.inner = blk.copy_span(|_| DefinitionInner::ExternBlock);

                let tag = blk
                    .tag
                    .map(|tag| {
                        Ok(Spanned {
                            body: ty::convert_tag(tag, curmod, curmod)?,
                            span: tag.span,
                        })
                    })
                    .transpose()?
                    .unwrap_or(Spanned {
                        body: ty::AbiTag::C { unwind: false },
                        span: Span::empty(),
                    });
                for item in &blk.items {
                    let visible_from =
                        convert_visibility(defs, curmod, item.vis.as_ref())?.unwrap_or(curmod);
                        match &item.item.body{
                            ast::ItemBody::Function(itemfn) => {
                                let defid = defs.allocate_defid();
                                let inner = collect_function(defs, curmod, defid, itemfn, Some(tag), Some(Spanned{body: Safety::Unsafe, span: Span::empty()}), Some(blk.span))?;
                                
                                let def = defs.definition_mut(defid);
                                def.attrs = item.attrs.clone();
                                def.visible_from = visible_from;
                                def.parent = curmod;
                                def.inner = itemfn.copy_span(|_| inner);
                
                                defs.insert_value(curmod, itemfn.name, defid)?;
                            }
                            ast::ItemBody::Value(_) => todo!("value"),
                            _ => {
                                return Err(Error{
                                    span: item.span,
                                    text: format!("Cannot define items other than functions or statics in an extern block"),
                                    category: ErrorCategory::Other,
                                    at_item: defid,
                                    containing_item: curmod,
                                    relevant_item: defid,
                                    hints: vec![SemaHint{ text: format!("Declared inside this extern block"), itemref: defid, refspan: blk.span }]
                                })
                            }
                        }
                }
            }
        }
    }

    Ok(())
}

pub fn convert_crate(defs: &mut Definitions, md: &Spanned<ast::Mod>) -> Result<()> {
    let root = defs.allocate_defid();
    defs.set_current_crate(root);

    scan_modules(defs, root, md)?;
    collect_types(defs, root, md)?;
    collect_values(defs,root,md)?;
    Ok(())
}
