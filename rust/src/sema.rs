use std::cell::RefCell;

use ast::Mutability;
use ast::Safety;
use cx::ConstExprConstructor;
use ty::AbiTag;
use xlang::abi::collection::HashMap;
use xlang::abi::collection::HashSet;
use xlang::abi::pair::Pair;
use xlang::targets::properties::TargetProperties;

use crate::lang::LangItemTarget;
use crate::CrateType;
use crate::{
    ast,
    irgen::visitor::{visit_module, ModVisitor},
};
pub use attr::Attr;

pub use crate::span::Spanned;

use crate::helpers::FetchIncrement;
use crate::helpers::TabPrinter;
use crate::interning::Symbol;
use crate::lang::LangItem;
use crate::sema::ty::AsyncType;
use crate::span::Span;

use self::generics::GenericArgs;
use self::intrin::IntrinsicDef;
use self::ty::convert_type;
use self::ty::FnType;
use self::ty::SemaLifetime;
use self::ty::Type;
use self::ty::TypeLayout;
use self::tyck::ThirBlock;
use self::tyck::ThirFunctionBody;
use self::{hir::HirFunctionBody, tyck::Inferer};
use self::{hir::HirLowerer, mir::MirFunctionBody};

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
                .all(|(a,b)| a==*b)
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
    #[allow(dead_code)]
    Unstable,
    CannotFindName,
    InvisibleEntity,
    AlreadyDefined,
    InvalidAbi,
    InvalidFunction,
    InvalidExpression,
    InvalidAttr,
    InvalidLiteral,
    BasicCoherenceError,
    TycheckError(tyck::TycheckErrorCategory),
    BorrowckError(mir::BorrowckErrorCategory),
    ConstEvalError(cx::ConstEvalError),
}

pub mod attr;
pub mod cx;
pub mod gen;
pub mod generics;
pub mod hir;
pub mod intrin;
pub mod mir;
pub mod ty;
pub mod tyck;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct DefId(pub(crate) u32);

impl DefId {
    pub const ROOT: DefId = DefId(0);

    #[doc(hidden)]
    pub const fn __new_unchecked(x: u32) -> Self {
        Self(x)
    }
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

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericParamInfo {
    Lifetime(Vec<SemaLifetime>),
    Type(Vec<ty::SemaTypeBound>),
    Const(Type),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GenericParams {
    pub params: Vec<GenericParamInfo>,
    pub by_name: HashMap<Spanned<Symbol>, u32>,
}

impl core::fmt::Display for GenericParams {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("<")?;
        let mut sep = "";
        for (n, param) in self.params.iter().enumerate() {
            f.write_str(sep)?;
            sep = ", ";
            match param {
                GenericParamInfo::Lifetime(bounds) => {
                    f.write_fmt(format_args!("'%{}", n))?;
                    let mut sep = ": ";

                    for bound in bounds {
                        f.write_str(sep)?;
                        sep = ", ";
                        bound.fmt(f)?;
                    }
                }
                GenericParamInfo::Type(bounds) => {
                    f.write_fmt(format_args!("%{}", n))?;
                    let mut sep = ": ";

                    for bound in bounds {
                        f.write_str(sep)?;
                        sep = ", ";
                        bound.fmt(f)?;
                    }
                }
                GenericParamInfo::Const(ty) => {
                    f.write_fmt(format_args!("const %{}: %{}", n, ty))?
                }
            }
        }
        f.write_str(">")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Definition {
    pub visible_from: DefId,
    pub parent: DefId,
    pub attrs: Vec<Spanned<Attr>>,
    pub inner: Spanned<DefinitionInner>,
    pub generics: GenericParams,
    pub canon_name: Symbol,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FunctionBody {
    Incomplete,
    AstBody(Vec<Spanned<ast::Param>>, Spanned<ast::Block>),
    HirBody(Spanned<HirFunctionBody>),
    ThirBody(Spanned<ThirFunctionBody>),
    MirBody(Spanned<MirFunctionBody>),
    Intrinsic(intrin::IntrinsicDef),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UserTypeKind {
    Struct,
    Union,
    Enum,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleField {
    pub attrs: Vec<Attr>,
    pub visible_from: DefId,
    pub ty: ty::Type,
}

impl core::fmt::Display for TupleField {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for attr in &self.attrs {
            attr.fmt(f)?;
            f.write_str(" ")?;
        }

        f.write_str("pub(in ")?;
        self.visible_from.fmt(f)?;
        f.write_str(") ")?;
        self.ty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField {
    pub attrs: Vec<Attr>,
    pub visible_from: DefId,
    pub name: Spanned<Symbol>,
    pub ty: Spanned<ty::Type>,
}

impl core::fmt::Display for StructField {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for attr in &self.attrs {
            attr.fmt(f)?;
            f.write_str(" ")?;
        }

        f.write_str("pub(in ")?;
        self.visible_from.fmt(f)?;
        f.write_str(") ")?;
        self.name.body.fmt(f)?;
        f.write_str(": ")?;
        self.ty.body.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Constructor {
    Unit,
    #[allow(dead_code)]
    Tuple(Vec<Spanned<TupleField>>),
    Struct(Vec<Spanned<StructField>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructDefinition {
    pub ctor: Spanned<Constructor>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UserType {
    Incomplete(UserTypeKind),
    Struct(UserTypeKind, StructDefinition),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplBlock {
    pub safety: Safety,
    pub trait_def: Option<Spanned<DefId>>,
    pub ty: Spanned<ty::Type>,
    pub provides: HashMap<DefId, DefId>,
    pub assoc_types: HashMap<Symbol, DefId>,
    pub values: HashMap<Symbol, DefId>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitDef {
    pub auto: bool,
    pub safety: Safety,
    pub supertraits: Vec<ty::SemaTypeBound>,
    pub assoc_types: HashMap<Symbol, DefId>,
    pub values: HashMap<Symbol, DefId>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitFnSignature {
    pub fnty: FnType,
    pub has_receiver: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DefinitionInner {
    Placeholder,
    Module(Module),
    UserType(UserType),
    #[allow(dead_code)]
    IncompletAlias,
    Function(FnType, Option<FunctionBody>),
    #[allow(dead_code)]
    TraitFunction(TraitFnSignature, Option<FunctionBody>),
    #[allow(dead_code)]
    ImplFunction(TraitFnSignature, Option<DefId>, Option<FunctionBody>),
    Trait(TraitDef),
    Impl(ImplBlock),
    #[allow(dead_code)]
    UseName(DefId),
    #[allow(dead_code)]
    UseWildcard(DefId),
    ExternBlock,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    pub types: HashMap<Symbol, DefId>,
    pub values: HashMap<Symbol, DefId>,
    pub impls: HashMap<u64, DefId>,
    pub wildcard_imports: Vec<DefId>,
}

#[allow(dead_code)]
pub struct Definitions {
    crates: HashMap<Symbol, DefId>,
    curcrate: DefId,
    defs: HashMap<DefId, Definition>,
    lang_items: HashMap<LangItem, DefId>,
    nextdefid: u32,
    prelude_import: DefId,
    visibility_cache: HashSet<(DefId, DefId)>,
    extern_blocks: Vec<DefId>,
    intrinsics: HashMap<intrin::IntrinsicDef, DefId>,
    layout_cache: RefCell<HashMap<ty::Type, *mut TypeLayout>>,
    fields_cache: RefCell<HashMap<ty::Type, *mut Vec<ty::TypeField>>>,
    inherent_impls_cache: RefCell<HashMap<ty::Type, *mut Vec<DefId>>>,
    properties: &'static TargetProperties<'static>,
}

impl Drop for Definitions {
    fn drop(&mut self) {
        for Pair(_, layout) in self.layout_cache.get_mut() {
            unsafe {
                let _ = Box::from_raw(*layout);
            }
        }

        for Pair(_, fields) in self.fields_cache.get_mut() {
            unsafe {
                let _ = Box::from_raw(*fields);
            }
        }

        for Pair(_, inherent_impls) in self.inherent_impls_cache.get_mut() {
            unsafe {
                let _ = Box::from_raw(*inherent_impls);
            }
        }
    }
}

impl Definitions {
    pub fn new(properties: &'static TargetProperties<'static>) -> Definitions {
        Definitions {
            crates: HashMap::new(),
            curcrate: DefId::ROOT,
            defs: HashMap::new(),
            lang_items: HashMap::new(),
            nextdefid: 1,
            prelude_import: DefId::ROOT,
            visibility_cache: HashSet::new(),
            extern_blocks: Vec::new(),
            intrinsics: HashMap::new(),
            layout_cache: RefCell::new(HashMap::new()),
            fields_cache: RefCell::new(HashMap::new()),
            properties,
            inherent_impls_cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_intrinsic(&self, defid: DefId) -> Option<IntrinsicDef> {
        match self.definition(defid).inner.body {
            DefinitionInner::Function(_, Some(FunctionBody::Intrinsic(intrin))) => Some(intrin),
            _ => None,
        }
    }

    pub fn spanof(&self, defid: DefId) -> Span {
        self.definition(defid).inner.span
    }

    #[allow(dead_code)]
    pub fn as_module_mut(&mut self, defid: DefId) -> &mut Module {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition_mut(defid)
        {
            md
        } else {
            panic!("Expected defid {} to be a module", defid)
        }
    }

    pub fn as_module(&self, defid: DefId) -> &Module {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition(defid)
        {
            md
        } else {
            panic!("Expected defid {} to be a module", defid)
        }
    }

    pub fn is_module(&self, defid: DefId) -> bool {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(_),
                    ..
                },
            ..
        } = self.definition(defid)
        {
            true
        } else {
            false
        }
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

    pub fn insert_impl(&mut self, curmod: DefId, impl_id: u64, item: DefId) -> Result<()> {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition_mut(curmod)
        {
            if let Some(_) = md.impls.insert(impl_id, item) {
                // TODO: This shouldn't happen, period. If nothing else, this should be a rehash.
                panic!("ICE: impl block with id {} duplicated. This can happen extraordinarily rarely - try recompiling (if you set `LCCC_SEED` in the environment, set it to a different value, or unset it). If this message persists, file a bug report.", impl_id)
            } else {
                Ok(())
            }
        } else {
            panic!("Expected defid {} to be a module", curmod)
        }
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
            panic!("Expected defid {} to be a module", curmod)
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
            panic!("Expected defid {} to be a module", curmod)
        }
    }

    pub fn allocate_defid(&mut self, name: Symbol) -> DefId {
        let def = DefId(self.nextdefid.fetch_increment());

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
                generics: GenericParams {
                    params: vec![],
                    by_name: HashMap::new(),
                },
                canon_name: name,
            },
        );

        def
    }

    pub fn set_current_crate(&mut self, curcrate: DefId) {
        self.curcrate = curcrate;
    }

    pub fn set_current_crate_name(&mut self, name: impl Into<Symbol>) {
        self.crates.insert(name.into(), self.curcrate);
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
        let vis = self.visibility_of(item); // So we handle `visible_from(ROOT)` properly.
        self.visibility_matches(vis, curmod)
    }

    pub fn visibility_of(&self, item: DefId) -> DefId {
        if item == DefId::ROOT {
            item
        } else {
            self.definition(item).visible_from
        }
    }

    pub fn owning_crate(&self, item: DefId) -> DefId {
        if item == DefId::ROOT {
            item
        } else {
            let parent = self.definition(item).parent;

            if parent == DefId::ROOT {
                item
            } else {
                self.owning_crate(parent)
            }
        }
    }

    pub fn find_impl_in_mod(
        &self,
        _curmod: DefId,
        searchmod: DefId,
        impl_id: Spanned<u64>,
        _at_item: DefId,
    ) -> Result<DefId> {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(md),
                    ..
                },
            ..
        } = self.definition(searchmod)
        {
            Ok(md.impls[&impl_id.body])
        } else {
            panic!("Expected a module for item {}", searchmod); // TODO handle Ty::ASSOC_TYPE
        }
    }

    pub fn find_type_in_trait(
        &self,
        curmod: DefId,
        searchdef: DefId,
        id: Spanned<Symbol>,
        at_item: DefId,
    ) -> Result<DefId> {
        match &self.definition(searchdef).inner.body {
            DefinitionInner::Trait(tr) => {
                if let Some(x) = tr.assoc_types.get(&id.body) {
                    Ok(*x)
                } else {
                    for bound in &tr.supertraits {
                        match bound {
                            ty::SemaTypeBound::Trait(tr) => {
                                if let Ok(def) = self.find_type_in_trait(curmod, *tr, id, at_item) {
                                    return Ok(def);
                                }
                            }
                            ty::SemaTypeBound::Life(_) => {}
                        }
                    }
                    Err(Error {
                        span: id.span,
                        text: format!("Cannot find associated type {} in trait", *id),
                        category: ErrorCategory::CannotFindName,
                        at_item,
                        containing_item: curmod,
                        relevant_item: searchdef,
                        hints: vec![],
                    })
                }
            }
            DefinitionInner::Impl(blk) => {
                if let Some(x) = blk
                    .values
                    .get(&id.body)
                    .copied()
                    .filter(|&x| self.visible_from(x, curmod))
                {
                    Ok(x)
                } else {
                    if let Some(tr) = blk.trait_def {
                        return self.find_type_in_trait(curmod, tr.body, id, at_item);
                    } else {
                        Err(Error {
                            span: id.span,
                            text: format!("Cannot find associated type {} in impl block", *id),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: searchdef,
                            hints: vec![],
                        })
                    }
                }
            }
            _ => panic!("Expected a trait or impl"),
        }
    }

    #[allow(dead_code)]
    pub fn find_value_in_trait(
        &self,
        curmod: DefId,
        searchdef: DefId,
        id: Spanned<Symbol>,
        at_item: DefId,
    ) -> Result<DefId> {
        match &self.definition(searchdef).inner.body {
            DefinitionInner::Trait(tr) => {
                if let Some(x) = tr.values.get(&id.body) {
                    Ok(*x)
                } else {
                    for bound in &tr.supertraits {
                        match bound {
                            ty::SemaTypeBound::Trait(tr) => {
                                if let Ok(def) = self.find_value_in_trait(curmod, *tr, id, at_item)
                                {
                                    return Ok(def);
                                }
                            }
                            ty::SemaTypeBound::Life(_) => {}
                        }
                    }
                    Err(Error {
                        span: id.span,
                        text: format!("Cannot find associated type {} in trait", *id),
                        category: ErrorCategory::CannotFindName,
                        at_item,
                        containing_item: curmod,
                        relevant_item: searchdef,
                        hints: vec![],
                    })
                }
            }
            DefinitionInner::Impl(blk) => {
                if let Some(x) = blk
                    .values
                    .get(&id.body)
                    .copied()
                    .filter(|&x| self.visible_from(x, curmod))
                {
                    Ok(x)
                } else {
                    if let Some(tr) = blk.trait_def {
                        return self.find_value_in_trait(curmod, tr.body, id, at_item);
                    } else {
                        Err(Error {
                            span: id.span,
                            text: format!("Cannot find associated type {} in impl block", *id),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: searchdef,
                            hints: vec![],
                        })
                    }
                }
            }
            _ => panic!("Expected a trait or impl"),
        }
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
                panic!("Expected a module for item {}", searchmod); // TODO handle Ty::ASSOC_TYPE
            }
        }
    }

    pub fn find_value_in_mod(
        &self,
        curmod: DefId,
        searchmod: DefId,
        id: Spanned<Symbol>,
        at_item: DefId,
    ) -> Result<DefId> {
        if searchmod == DefId::ROOT {
            Err(Error {
                span: id.span,
                text: format!("Cannot find `::{}`", id.body),
                category: ErrorCategory::CannotFindName,
                at_item,
                containing_item: curmod,
                relevant_item: at_item,
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
                let mut def = match md.values.get(&id.body) {
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
                panic!("Expected a module for item {}", searchmod); // TODO handle Ty::ASSOC_TYPE
            }
        }
    }

    pub fn find_constructor(
        &self,
        curmod: DefId,
        path: &ast::Path,
        at_item: DefId,
        self_ty: Option<&Type>,
    ) -> Result<(DefId, generics::GenericArgs)> {
        self.find_type(curmod, path, at_item, self_ty)
    }

    fn collect_impls_in(&self, impls: &mut Vec<DefId>, searchmod: DefId, ty: &ty::Type) {
        match &self.definition(searchmod).inner.body {
            DefinitionInner::Module(md) => {
                for &Pair(_, defid) in &md.impls {
                    match &self.definition(defid).inner.body {
                        DefinitionInner::Impl(ImplBlock {
                            ty: impl_ty,
                            trait_def: None,
                            ..
                        }) => {
                            if ty.matches_ignore_bounds(impl_ty) {
                                impls.push(defid);
                            }
                        }
                        _ => {}
                    }
                }

                for &Pair(_, defid) in &md.types {
                    if self.is_module(defid) {
                        self.collect_impls_in(impls, defid, ty);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn find_inherent_impls(&self, ty: &ty::Type) -> &[DefId] {
        let mut guard = self.inherent_impls_cache.borrow_mut();
        if let Some(element) = guard.get(ty).copied() {
            unsafe { &*element }
        } else {
            let mut impls = Vec::new();

            let defid = self.type_defid(ty);
            if let DefinitionInner::Impl(_) = &self.definition(defid).inner.body {
                impls.push(defid);
                if let Some(rtlang) = ty.rt_impl_lang().and_then(|lang| self.get_lang_item(lang)) {
                    impls.push(rtlang);
                }
            } else {
                let cr = self.owning_crate(defid);
                self.collect_impls_in(&mut impls, cr, ty);
            }

            let val = Box::into_raw(Box::new(impls));
            let _ = guard.insert(ty.clone(), val);
            return unsafe { &*val };
        }
    }

    #[allow(clippy::while_let_on_iterator)] // This currently isn't needed, but in the future the body will need to call `path.as_slice()` and recurse.
    fn find_type_in_type(
        &self,
        curmod: DefId,
        path: &[Spanned<ast::PathSegment>],
        at_item: DefId,
        search_ty: Type,
        _self_ty: Option<&Type>,
    ) -> Result<DefId> {
        let mut path = path.iter();

        while let Some(component) = path.next() {
            match &component.ident.body {
                ast::SimplePathSegment::Identifier(id) => {
                    let impls = self.find_inherent_impls(&search_ty);

                    for &i in impls {
                        if let Ok(x) = self.find_type_in_trait(
                            curmod,
                            i,
                            Spanned {
                                body: *id,
                                span: component.ident.span,
                            },
                            at_item,
                        ) {
                            if path.len() == 0 {
                                // I swear if clippy lints this in the future
                                return Ok(x);
                            }
                            todo!("Well, this is akward - we have an item in the type namespace, but no actual definition type to determine what it is")
                        }
                    }

                    todo!("Collect possible source traits")
                }
                seg => {
                    return Err(Error {
                        span: component.span,
                        text: format!("Path segment `{}` cannot be found within a type", seg),
                        category: ErrorCategory::CannotFindName,
                        at_item,
                        containing_item: curmod,
                        relevant_item: self.type_defid(&search_ty),
                        hints: vec![],
                    })
                }
            }
        }
        panic!("Impossible to parse an empty path")
    }

    pub fn convert_generics_for_def(
        &self,
        def: DefId,
        generics: &mut GenericArgs,
        ast_generics: &ast::GenericArgs,
        curmod: DefId,
        at_item: DefId,
        self_ty: Option<&ty::Type>,
    ) -> Result<()> {
        let generic_params = &self.definition(def).generics;

        for (arg, param) in ast_generics.args.iter().zip(&generic_params.params) {
            match (&arg.body, &param) {
                (ast::GenericArg::Id(p), GenericParamInfo::Type(_)) => {
                    let (defid, gen) = self.find_type(curmod, p, at_item, self_ty)?; // TODO: `<N>`
                    generics
                        .params
                        .push(generics::GenericArg::Type(Type::UserType(defid, gen)));
                }
                (ast::GenericArg::Type(ty), GenericParamInfo::Type(_)) => {
                    let ty = convert_type(self, curmod, at_item, ty, self_ty)?;
                    generics.params.push(generics::GenericArg::Type(ty));
                }
                _ => todo!(),
            }
        }

        Ok(())
    }

    pub fn find_type(
        &self,
        curmod: DefId,
        path: &ast::Path,
        at_item: DefId,
        self_ty: Option<&Type>,
    ) -> Result<(DefId, generics::GenericArgs)> {
        let mut resolve_base = None;

        match &path.root {
            Some(Spanned {
                body: ast::PathRoot::Root,
                ..
            }) => {
                resolve_base = Some(DefId(0));
            }
            Some(Spanned {
                body: ast::PathRoot::QSelf(ty, None),
                ..
            }) => {
                todo!("QSelf")
            }
            Some(Spanned {
                body: ast::PathRoot::QSelf(_, Some(_)),
                ..
            }) => {}
            None => {}
        }
        let mut path_iter = path.segments.iter();

        let mut generics = generics::GenericArgs::default();

        while let Some(seg) = path_iter.next() {
            match &seg.ident.body {
                ast::SimplePathSegment::Identifier(id) => {
                    let id = Spanned {
                        body: *id,
                        span: seg.ident.span,
                    };

                    if let Some(md) = resolve_base {
                        match &self.definition(md).inner.body {
                            DefinitionInner::Module(_) => {
                                resolve_base = Some(self.find_type_in_mod(curmod, md, id, at_item)?)
                            }
                            DefinitionInner::UserType(_) => {
                                let ty = Type::UserType(md, generics::GenericArgs::default());
                                return Ok((
                                    self.find_type_in_type(
                                        curmod,
                                        path_iter.as_slice(), // FIXME: We need to not step the iterator by here
                                        at_item,
                                        ty,
                                        self_ty,
                                    )?,
                                    generics,
                                ));
                            }
                            DefinitionInner::Trait(_) => {
                                resolve_base =
                                    Some(self.find_type_in_trait(curmod, md, id, at_item)?);
                            }
                            _ => todo!(""),
                        }
                    } else {
                        match self.find_type_in_mod(curmod, curmod, id, at_item) {
                            Ok(item) => resolve_base = Some(item),
                            Err(e) => {
                                if self.prelude_import.0 != 0 {
                                    if let Ok(item) = self.find_type_in_mod(
                                        curmod,
                                        self.prelude_import,
                                        id,
                                        at_item,
                                    ) {
                                        resolve_base = Some(item)
                                    } else if let Ok(item) =
                                        self.find_type_in_mod(curmod, DefId::ROOT, id, at_item)
                                    {
                                        resolve_base = Some(item)
                                    } else {
                                        return Err(e);
                                    }
                                } else if let Ok(item) =
                                    self.find_type_in_mod(curmod, DefId::ROOT, id, at_item)
                                {
                                    resolve_base = Some(item)
                                } else {
                                    return Err(e);
                                }
                            }
                        }

                        if let Some(ast_generics) = &seg.generics {
                            let def = resolve_base.expect("just resolved");
                            self.convert_generics_for_def(
                                def,
                                &mut generics,
                                ast_generics,
                                curmod,
                                at_item,
                                self_ty,
                            )?;
                        }
                    }
                }
                ast::SimplePathSegment::SuperPath => {
                    if let Some(md) = resolve_base {
                        if md == DefId::ROOT {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`super` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint {
                                    text: format!("Remove the `::`"),
                                    itemref: at_item,
                                    refspan: path.root.as_ref().unwrap().span,
                                }],
                            });
                        } else {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`super` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    } else {
                        resolve_base = Some(self.definition(curmod).parent);
                    }

                    if let Some(generics) = &seg.generics {
                        return Err(Error {
                            span: generics.span,
                            text: format!("`super` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                }
                ast::SimplePathSegment::SelfPath => {
                    if let Some(md) = resolve_base {
                        if md == DefId::ROOT {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`self` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint {
                                    text: format!("Remove the `::`"),
                                    itemref: at_item,
                                    refspan: path.root.as_ref().unwrap().span,
                                }],
                            });
                        } else {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`self` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    } else {
                        resolve_base = Some(curmod);
                    }

                    if let Some(generics) = &seg.generics {
                        return Err(Error {
                            span: generics.span,
                            text: format!("`self` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                }
                ast::SimplePathSegment::CratePath => {
                    if let Some(md) = resolve_base {
                        if md == DefId::ROOT {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`crate` is not allowed in a root"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![SemaHint {
                                    text: format!("Remove the `::`"),
                                    itemref: at_item,
                                    refspan: path.root.as_ref().unwrap().span,
                                }],
                            });
                        } else {
                            return Err(Error {
                                span: seg.ident.span,
                                text: format!("`crate` cannot appear after any other components"),
                                category: ErrorCategory::CannotFindName,
                                at_item,
                                containing_item: curmod,
                                relevant_item: at_item,
                                hints: vec![],
                            });
                        }
                    } else {
                        resolve_base = Some(self.curcrate);
                    }

                    if let Some(generics) = &seg.generics {
                        return Err(Error {
                            span: generics.span,
                            text: format!("`crate` cannot be generic"),
                            category: ErrorCategory::CannotFindName,
                            at_item,
                            containing_item: curmod,
                            relevant_item: at_item,
                            hints: vec![],
                        });
                    }
                }
                ast::SimplePathSegment::MacroCratePath => todo!("$crate"), // this needs to look in the macro DefId table
            }
        }

        Ok((
            resolve_base.expect("Parsing an empty path is impossible"),
            generics,
        ))
    }

    pub fn is_copy(&self, ty: &ty::Type) -> bool {
        match ty {
            ty::Type::Bool
            | ty::Type::Int(_)
            | ty::Type::Float(_)
            | ty::Type::Char
            | ty::Type::Str
            | ty::Type::Never
            | ty::Type::FnPtr(_)
            | ty::Type::FnItem(_, _, _)
            | ty::Type::Pointer(_, _) => true,
            ty::Type::Reference(_, mt, _) => mt.body == Mutability::Const,
            ty::Type::Tuple(inner) => inner.iter().all(|ty| self.is_copy(ty)),
            ty::Type::UserType(_, _) => false, // for now
            ty::Type::IncompleteAlias(_) => panic!("incomplete alias held too late"),
            ty::Type::UnresolvedLangItem(_, _) => panic!("Unresolved lang item"),
            ty::Type::Array(ty, _) => self.is_copy(ty),
            ty::Type::InferableInt(_) | ty::Type::Inferable(_) => {
                panic!("Cannot determine copyability of an uninfered type")
            }
            ty::Type::TraitSelf(_) => false, // for now
            ty::Type::Param(_) => false,
            ty::Type::DropFlags(_) => false,
        }
    }

    pub fn get_lang_item(&self, lang: LangItem) -> Option<DefId> {
        self.lang_items.get(&lang).copied()
    }

    pub fn require_lang_item(&self, lang: LangItem) -> Result<DefId> {
        self.get_lang_item(lang).ok_or_else(|| Error {
            span: Span::synthetic(),
            text: format!("Lang item {} required but not defined", lang.name()),
            category: ErrorCategory::CannotFindName,
            containing_item: self.curcrate,
            at_item: self.curcrate,
            relevant_item: DefId::ROOT,
            hints: vec![],
        })
    }

    pub fn type_defid(&self, ty: &ty::Type) -> DefId {
        // todo: look up primitive impl lang item
        match ty {
            ty::Type::UserType(defid, _) | ty::Type::FnItem(_, defid, _) => *defid,
            ty::Type::IncompleteAlias(_) => panic!("incomplete alias held too late"),
            ty::Type::InferableInt(_) | ty::Type::Inferable(_) => {
                panic!("Canot determine owning definition of an uninfered type")
            }
            ty::Type::Param(_) => DefId::ROOT,
            ty::Type::TraitSelf(defid) => *defid,
            prim => prim
                .as_lang_item()
                .and_then(|lang| self.get_lang_item(lang))
                .unwrap_or(DefId::ROOT),
        }
    }

    pub fn visit_all_crates<V: ModVisitor>(&self, mut visitor: V) {
        for c in &self.crates {
            visit_module(c.1, self, &mut visitor, &mut vec![c.0]);
        }
    }

    fn collect_lang_items(&mut self, defid: DefId, targ: &[LangItemTarget]) -> Result<()> {
        let def = &self.defs[&defid];
        let containing_item = def.parent;

        for attr in &def.attrs {
            match attr.body {
                Attr::Lang(lang) => {
                    if !targ.into_iter().any(|&targ| lang.target() == targ) {
                        return Err(Error {
                            span: lang.span,
                            text: format!("Lang item {} does not apply to this item", lang.name()),
                            category: ErrorCategory::InvalidAttr,
                            at_item: defid,
                            containing_item,
                            relevant_item: defid,
                            hints: vec![],
                        });
                    }

                    if let Some(existing) = self.lang_items.insert(*lang, defid) {
                        return Err(Error {
                            span: lang.span,
                            text: format!(
                                "Found existing definition for lang item {}",
                                lang.name()
                            ),
                            category: ErrorCategory::InvalidAttr,
                            at_item: defid,
                            containing_item,
                            relevant_item: existing,
                            hints: vec![SemaHint {
                                text: format!(
                                    "Previous definiton for lang item {} here",
                                    lang.name()
                                ),
                                itemref: existing,
                                refspan: self.defs[&existing].inner.span,
                            }],
                        });
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub fn type_of_constructor(&self, ctor_def: DefId) -> DefId {
        match &self.definition(ctor_def).inner.body {
            DefinitionInner::UserType(_) => ctor_def,
            _ => panic!("Not a struct or variant {}", ctor_def),
        }
    }

    pub fn layout_of(
        &self,
        ty: &ty::Type,
        at_item: DefId,
        containing_item: DefId,
    ) -> &ty::TypeLayout {
        let cache = self.layout_cache.borrow();
        if let Some(inner) = cache.get(ty) {
            unsafe { &**inner }
        } else {
            drop(cache);
            let layout = match ty {
                Type::Bool => ty::TypeLayout {
                    size: Some(1),
                    align: Some(1),
                    enum_layout: None,
                    wide_ptr_metadata: None,
                    field_offsets: HashMap::new(),
                    mutable_fields: HashSet::new(),
                    niches: Some(ty::Niches::Scalar(ty::ScalarNiches {
                        is_nonzero: false,
                        max_value: 1,
                    })),
                },
                Type::Int(ity) => {
                    let width = match ity.width {
                        ty::IntWidth::Bits(n) => n.get(),
                        ty::IntWidth::Size => self.properties.primitives.ptrbits,
                    };

                    let max_align = self.properties.primitives.max_align;

                    let size = width >> 3;

                    let align = size.min(max_align);

                    let size = size as u64;
                    let align = align as u64;

                    ty::TypeLayout {
                        size: Some(size),
                        align: Some(align),
                        wide_ptr_metadata: None,
                        enum_layout: None,
                        field_offsets: HashMap::new(),
                        mutable_fields: HashSet::new(),
                        niches: None,
                    }
                }
                Type::Float(fty) => {
                    let (size, align) = {
                        let max_align = self.properties.primitives.max_align;
                        let width = fty.width.get();
                        let size = width >> 8;

                        let align = size.next_power_of_two().min(max_align);

                        (size as u64, align as u64)
                    };

                    ty::TypeLayout {
                        size: Some(size),
                        align: Some(align),
                        wide_ptr_metadata: None,
                        enum_layout: None,
                        field_offsets: HashMap::new(),
                        mutable_fields: HashSet::new(),
                        niches: None,
                    }
                }
                Type::Char => ty::TypeLayout {
                    size: Some(4),
                    align: Some(self.properties.primitives.max_align.min(4) as u64),
                    enum_layout: None,
                    wide_ptr_metadata: None,
                    field_offsets: HashMap::new(),
                    mutable_fields: HashSet::new(),
                    niches: Some(ty::Niches::Scalar(ty::ScalarNiches {
                        is_nonzero: false,
                        max_value: 0x10FFFF,
                    })),
                },
                Type::Str => ty::TypeLayout {
                    size: None,
                    align: Some(1),
                    wide_ptr_metadata: Some(ty::WidePtrMetadata::SliceLen),
                    enum_layout: None,
                    field_offsets: HashMap::new(),
                    mutable_fields: HashSet::new(),
                    niches: None,
                },
                Type::Never => ty::TypeLayout {
                    size: Some(0),
                    align: Some(1),
                    enum_layout: None,
                    wide_ptr_metadata: None,
                    field_offsets: HashMap::new(),
                    mutable_fields: HashSet::new(),
                    niches: Some(ty::Niches::Uninhabited),
                },
                Type::Tuple(fields) => {
                    let mut ordered_fields = Vec::new();

                    let mut metadata = None;

                    if let Some((back, rest)) = fields.split_last() {
                        ordered_fields.extend(
                            rest.iter()
                                .map(|ty| self.layout_of(ty, at_item, containing_item)),
                        );

                        ordered_fields.sort_by_key(|layout| layout.align);
                        let back_layout = self.layout_of(back, at_item, containing_item);
                        ordered_fields.push(back_layout);

                        metadata = back_layout.wide_ptr_metadata.clone();
                    }

                    let mut cur_align = 0;
                    let mut cur_offset = 0;

                    let mut field_offsets = HashMap::new();

                    let mut field_niches = Vec::new();

                    for (field, ty_layout) in ordered_fields.into_iter().enumerate() {
                        let align = ty_layout.align.unwrap();

                        cur_align = cur_align.max(align);
                        let offset = (cur_offset + (align - 1)) & !(align - 1);

                        let name =
                            ty::FieldName::Field(Symbol::intern_by_val(format!("{}", field)));

                        if let Some(niches) = ty_layout.niches.clone() {
                            field_niches.push((name, niches));
                        }

                        field_offsets.insert(name, offset);

                        let size = ty_layout.size.unwrap();

                        cur_offset = offset + size;
                        // TODO: Handle DST tails
                    }

                    let size = (cur_offset + (cur_align - 1)) & !(cur_align - 1);

                    let align = cur_align;

                    ty::TypeLayout {
                        size: Some(size),
                        align: Some(align),
                        enum_layout: None,
                        wide_ptr_metadata: metadata,
                        field_offsets,
                        mutable_fields: HashSet::new(),
                        niches: if !field_niches.is_empty() {
                            Some(ty::Niches::Aggregate(field_niches))
                        } else {
                            None
                        },
                    }
                }
                Type::FnPtr(_) => {
                    let size = self.properties.primitives.fnptrbits >> 3;
                    let align = self.properties.primitives.ptralign.min(size);

                    ty::TypeLayout {
                        size: Some(size as u64),
                        align: Some(align as u64),
                        enum_layout: None,
                        wide_ptr_metadata: None,
                        field_offsets: HashMap::new(),
                        mutable_fields: HashSet::new(),
                        niches: Some(ty::Niches::NonNullPointer),
                    }
                }
                Type::FnItem(_, _, _) => ty::TypeLayout {
                    size: Some(0),
                    align: Some(1),
                    enum_layout: None,
                    wide_ptr_metadata: None,
                    field_offsets: HashMap::new(),
                    mutable_fields: HashSet::new(),
                    niches: None,
                },
                Type::UserType(def, generics) => {
                    let def = self.definition(*def);

                    let repr = def
                        .attrs
                        .iter()
                        .find_map(|f| match &f.body {
                            Attr::Repr(x) => Some(x.body),
                            _ => None,
                        })
                        .unwrap_or(attr::Repr::RUST);

                    match &def.inner.body {
                        DefinitionInner::UserType(UserType::Struct(_, def)) => {
                            let mut field_layouts = Vec::new();

                            match &def.ctor.body {
                                Constructor::Unit => {}
                                Constructor::Tuple(fields) => {
                                    for (n, field) in fields.iter().enumerate() {
                                        let name = Symbol::intern_by_val(format!("{}", n));
                                        let name = ty::FieldName::Field(name);
                                        let layout =
                                            self.layout_of(&field.ty, at_item, containing_item);
                                        field_layouts.push((name, layout));
                                    }
                                }
                                Constructor::Struct(fields) => {
                                    for field in fields {
                                        let name = ty::FieldName::Field(field.name.body);
                                        let layout =
                                            self.layout_of(&field.ty, at_item, containing_item);
                                        field_layouts.push((name, layout));
                                    }
                                }
                            }

                            match &repr.base.body {
                                attr::ReprBase::Rust | attr::ReprBase::LCRust(None | Some(0)) => {
                                    field_layouts
                                        .sort_by_key(|(_, layout)| layout.align.unwrap_or(!0));
                                }
                                _ => {}
                            }

                            let mut field_offsets = HashMap::new();
                            let mut field_niches = Vec::new();

                            let mut curr_offset = 0;
                            let mut is_unaligned = false;
                            let mut is_unsized = false;
                            let mut wide_ptr_metadata = None;
                            let max_interfield_align = match repr.alignment {
                                Some(Spanned {
                                    body: attr::AlignmentSpec::Packed(a),
                                    ..
                                }) => a,
                                _ => !0,
                            };
                            let mut struct_align = match repr.alignment {
                                Some(Spanned {
                                    body: attr::AlignmentSpec::Aligned(a),
                                    ..
                                }) => a,
                                _ => 1,
                            };

                            for (field, layout) in field_layouts {
                                let align = if let Some(align) = layout.align {
                                    align.min(max_interfield_align)
                                } else {
                                    wide_ptr_metadata.clone_from(&layout.wide_ptr_metadata);
                                    is_unaligned = true;
                                    is_unsized = true;
                                    break;
                                };

                                struct_align = align.max(struct_align);

                                curr_offset = (curr_offset + (align - 1)) & (align - 1);

                                field_offsets.insert(field, curr_offset);

                                if let Some(niches) = layout.niches.clone() {
                                    field_niches.push((field, niches))
                                }

                                if let Some(size) = layout.size {
                                    curr_offset += size;
                                } else {
                                    wide_ptr_metadata.clone_from(&layout.wide_ptr_metadata);
                                    is_unsized = false;
                                    break;
                                }
                            }

                            let niches = if field_niches.is_empty() {
                                None
                            } else {
                                Some(ty::Niches::Aggregate(field_niches))
                            };

                            let size = if is_unsized { None } else { Some(curr_offset) };

                            let align = if is_unaligned {
                                None
                            } else {
                                Some(struct_align)
                            };

                            ty::TypeLayout {
                                size,
                                align,
                                enum_layout: None,
                                wide_ptr_metadata,
                                field_offsets,
                                mutable_fields: HashSet::new(),
                                niches,
                            }
                        }
                        _ => panic!("Bad type ref"),
                    }
                }
                Type::IncompleteAlias(_) => todo!("incomplete alias held too late"),
                Type::UnresolvedLangItem(_, _) => panic!("Unresolved lang item not resolved"),
                Type::Pointer(_, pte) => {
                    let layout = self.layout_of(pte, at_item, containing_item);

                    if let Some(metadata) = &layout.wide_ptr_metadata {
                        let meta_ty = metadata.to_canonical_type(self);
                        let meta_layout = self.layout_of(&meta_ty, at_item, containing_item);

                        let data_ptr_size = (self.properties.primitives.ptrbits >> 3) as u64;
                        let data_ptr_align =
                            data_ptr_size.min(self.properties.primitives.ptralign as u64);

                        let meta_size = meta_layout.size.unwrap();
                        let meta_align = meta_layout.align.unwrap();

                        let align = data_ptr_align.max(meta_align) as u64;

                        let data_ptr_offset = if meta_align > data_ptr_align {
                            meta_size
                        } else {
                            0
                        };

                        let meta_offset = if meta_align < data_ptr_align {
                            data_ptr_size
                        } else {
                            0
                        };

                        let fields_size = data_ptr_size + meta_size;

                        let size = (fields_size + (align - 1)) & !(align - 1);

                        let mut fields = HashMap::new();

                        fields.insert(
                            ty::FieldName::FatPtrPart(ty::FatPtrPart::Payload),
                            data_ptr_offset,
                        );
                        fields.insert(
                            ty::FieldName::FatPtrPart(ty::FatPtrPart::Metadata),
                            meta_offset,
                        );

                        ty::TypeLayout {
                            size: Some(size),
                            align: Some(align),
                            enum_layout: None,
                            wide_ptr_metadata: None,
                            field_offsets: fields,
                            mutable_fields: HashSet::new(),
                            niches: None,
                        }
                    } else {
                        let size = (self.properties.primitives.ptrbits >> 3) as u64;
                        let align = size.min(self.properties.primitives.ptralign as u64);

                        ty::TypeLayout {
                            size: Some(size),
                            align: Some(align),
                            enum_layout: None,
                            wide_ptr_metadata: None,
                            field_offsets: HashMap::new(),
                            mutable_fields: HashSet::new(),
                            niches: None,
                        }
                    }
                }
                Type::Array(ty, elems) => {
                    let elems = self
                        .evaluate_as_u64(elems, at_item, containing_item)
                        .expect("Left over generic parameter or wrong type");

                    let inner_layout = self.layout_of(ty, at_item, containing_item);

                    let size = inner_layout.size.unwrap();
                    let align = inner_layout.align.unwrap();

                    let array_size = size * elems;

                    ty::TypeLayout {
                        size: Some(array_size),
                        align: Some(align),
                        enum_layout: None,
                        wide_ptr_metadata: None,
                        field_offsets: HashMap::new(),
                        mutable_fields: HashSet::new(),
                        niches: None, // TODO: we need
                    }
                }
                Type::Inferable(_) | Type::InferableInt(_) => {
                    todo!("Cannot determine properties of an uninferred type")
                }
                Type::Reference(_, _, ty) => {
                    let layout = self.layout_of(ty, at_item, containing_item);

                    if let Some(metadata) = &layout.wide_ptr_metadata {
                        let meta_ty = metadata.to_canonical_type(self);
                        let meta_layout = self.layout_of(&meta_ty, at_item, containing_item);

                        let data_ptr_size = (self.properties.primitives.ptrbits >> 3) as u64;
                        let data_ptr_align =
                            data_ptr_size.min(self.properties.primitives.ptralign as u64);

                        let meta_size = meta_layout.size.unwrap();
                        let meta_align = meta_layout.align.unwrap();

                        let align = data_ptr_align.max(meta_align) as u64;

                        let data_ptr_offset = if meta_align > data_ptr_align {
                            meta_size
                        } else {
                            0
                        };

                        let meta_offset = if meta_align < data_ptr_align {
                            data_ptr_size
                        } else {
                            0
                        };

                        let fields_size = data_ptr_size + meta_size;

                        let size = (fields_size + (align - 1)) & !(align - 1);

                        let mut fields = HashMap::new();

                        fields.insert(
                            ty::FieldName::FatPtrPart(ty::FatPtrPart::Payload),
                            data_ptr_offset,
                        );
                        fields.insert(
                            ty::FieldName::FatPtrPart(ty::FatPtrPart::Metadata),
                            meta_offset,
                        );

                        let mut field_niches = Vec::new();

                        field_niches.push((
                            ty::FieldName::FatPtrPart(ty::FatPtrPart::Payload),
                            ty::Niches::NonNullPointer,
                        ));

                        ty::TypeLayout {
                            size: Some(size),
                            align: Some(align),
                            enum_layout: None,
                            wide_ptr_metadata: None,
                            field_offsets: fields,
                            mutable_fields: HashSet::new(),
                            niches: Some(ty::Niches::Aggregate(field_niches)),
                        }
                    } else {
                        let size = (self.properties.primitives.ptrbits >> 3) as u64;
                        let align = size.min(self.properties.primitives.ptralign as u64);

                        ty::TypeLayout {
                            size: Some(size),
                            align: Some(align),
                            enum_layout: None,
                            wide_ptr_metadata: None,
                            field_offsets: HashMap::new(),
                            mutable_fields: HashSet::new(),
                            niches: Some(ty::Niches::NonNullPointer),
                        }
                    }
                }
                Type::Param(_) => panic!("generics not allowed, monomorphize types first"),
                Type::TraitSelf(_) => todo!("generics not allowed, monomorphize types first"),
                Type::DropFlags(_fl) => todo!("drop flags layout computation is needed"),
            };

            let layout = Box::new(layout);

            let ptr = Box::into_raw(layout);

            self.layout_cache.borrow_mut().insert(ty.clone(), ptr);

            unsafe { &*ptr }
        }
    }

    #[allow(dead_code)]
    pub fn size_of(&self, ty: &ty::Type) -> Option<u64> {
        self.layout_of(ty, DefId::ROOT, DefId::ROOT).size
    }

    #[allow(dead_code)]
    pub fn align_of(&self, ty: &ty::Type) -> Option<u64> {
        self.layout_of(ty, DefId::ROOT, DefId::ROOT).align
    }

    #[allow(dead_code)]
    pub fn field_visibility(&self, ty: &ty::Type, fname: &ty::FieldName) -> Option<DefId> {
        self.fields_of(ty)
            .iter()
            .filter(|field| &field.name == fname)
            .map(|field| field.vis)
            .next()
    }

    #[allow(dead_code)]
    pub fn field_type(&self, ty: &ty::Type, fname: &ty::FieldName) -> Option<&ty::Type> {
        self.fields_of(ty)
            .iter()
            .filter(|field| &field.name == fname)
            .map(|field: &ty::TypeField| &field.ty)
            .next()
    }

    pub fn fields_of(&self, ty: &ty::Type) -> &Vec<ty::TypeField> {
        let mut borrow = self.fields_cache.borrow_mut();
        if let Some(ty) = borrow.get(ty) {
            unsafe { &**ty }
        } else {
            let fields = match ty {
                Type::Bool
                | Type::Int(_)
                | Type::Float(_)
                | Type::Char
                | Type::Str
                | Type::Never
                | Type::FnPtr(_)
                | Type::FnItem(_, _, _)
                | Type::Param(_)
                | Type::Reference(_, _, _)
                | Type::Pointer(_, _)
                | Type::Array(_, _)
                | Type::TraitSelf(_)
                | Type::UnresolvedLangItem(_, _) => Vec::new(),
                Type::Tuple(elems) => {
                    let mut fields = Vec::new();

                    for (field, ty) in elems.iter().enumerate() {
                        let tyfield = ty::TypeField {
                            name: ty::FieldName::Field(Symbol::intern_by_val(field.to_string())),
                            ty: ty.body.clone(),
                            vis: DefId::ROOT,
                        };
                        fields.push(tyfield);
                    }

                    fields
                }
                Type::UserType(defid, _) => {
                    let def = self.definition(*defid);

                    match &def.inner.body {
                        DefinitionInner::UserType(ty) => match ty {
                            UserType::Incomplete(_) => unreachable!("late incomplete type"),
                            UserType::Struct(_, ctor) => {
                                let mut fields = Vec::new();
                                match &ctor.ctor.body {
                                    Constructor::Unit => {}
                                    Constructor::Tuple(elems) => {
                                        for (name, field) in elems.iter().enumerate() {
                                            let name = Symbol::intern_by_val(name.to_string());
                                            let tyfield = ty::TypeField {
                                                name: ty::FieldName::Field(name),
                                                ty: field.ty.clone(),
                                                vis: field.visible_from,
                                            };
                                            fields.push(tyfield);
                                        }
                                    }
                                    Constructor::Struct(elems) => {
                                        for field in elems {
                                            let name = field.name.body;
                                            let tyfield = ty::TypeField {
                                                name: ty::FieldName::Field(name),
                                                ty: field.ty.body.clone(),
                                                vis: field.visible_from,
                                            };
                                            fields.push(tyfield);
                                        }
                                    }
                                }
                                fields
                            }
                        },
                        _ => panic!("Not a user type"),
                    }
                }
                Type::IncompleteAlias(_) => unreachable!("late incomplete alias"),

                Type::Inferable(_) => Vec::new(),
                Type::InferableInt(_) => Vec::new(),
                Type::DropFlags(_) => Vec::new(),
            };

            let fields = Box::into_raw(Box::new(fields));

            borrow.insert(ty.clone(), fields);

            unsafe { &*fields }
        }
    }

    pub fn diag_path(&self, def: DefId) -> Vec<Symbol> {
        if def == DefId::ROOT {
            Vec::new()
        } else {
            let definition = self.definition(def);
            let mut diag_path = self.diag_path(definition.parent);
            diag_path.push(definition.canon_name);
            diag_path
        }
    }

    pub fn type_name(&self, ty: &Type) -> Symbol {
        use core::fmt::Write;
        match ty {
            Type::Bool => Symbol::intern("bool"),
            Type::Int(ity) => Symbol::intern_by_val(ity.to_string()),
            Type::Float(fty) => Symbol::intern_by_val(fty.to_string()),
            Type::Char => Symbol::intern("char"),
            Type::Str => Symbol::intern("str"),
            Type::Never => Symbol::intern("!"),
            Type::Tuple(tys) => {
                let mut st = String::new();
                st.push_str("(");
                let mut sep = "";
                for ty in tys {
                    st.push_str(sep);
                    st.push_str(&self.type_name(ty));
                    sep = ", ";
                }
                st.push_str(")");
                Symbol::intern_by_val(st)
            }
            Type::FnPtr(ptr) => {
                let mut st = String::new();
                if ptr.safety.body == Safety::Unsafe {
                    st.push_str("unsafe ");
                }
                match &ptr.tag.body {
                    AbiTag::Rust => {}
                    tag => {
                        write!(st, "extern \"{tag}\" ").unwrap();
                    }
                }
                st.push_str("fn(");
                let mut sep = "";
                for ty in &ptr.paramtys {
                    st.push_str(sep);
                    st.push_str(&self.type_name(ty));
                    sep = ", ";
                }
                if ptr.iscvarargs.body {
                    st.push_str(sep);
                    st.push_str("...");
                }
                st.push_str(") -> ");
                st.push_str(&self.type_name(&ptr.retty));
                Symbol::intern_by_val(st)
            }
            Type::FnItem(fnty, def, _) => {
                let mut st = String::new();
                if fnty.asyncness.body == ty::AsyncType::Async {
                    st.push_str("async ");
                }
                if fnty.safety.body == Safety::Unsafe {
                    st.push_str("unsafe ");
                }
                match &fnty.tag.body {
                    AbiTag::Rust => {}
                    tag => {
                        write!(st, "extern \"{tag}\" ").unwrap();
                    }
                }
                st.push_str("fn(");
                let mut sep = "";
                for ty in &fnty.paramtys {
                    st.push_str(sep);
                    st.push_str(&self.type_name(ty));
                    sep = ", ";
                }
                if fnty.iscvarargs.body {
                    st.push_str(sep);
                    st.push_str("...");
                }
                st.push_str(") -> ");
                st.push_str(&self.type_name(&fnty.retty));

                st.push_str(" {");

                sep = "";

                for seg in self.diag_path(*def) {
                    st.push_str(sep);
                    st.push_str(&seg);
                    sep = "::"
                }
                st.push_str("}");

                Symbol::intern_by_val(st)
            }
            Type::UserType(def, _) => {
                let mut st = String::new();
                let mut sep = "";

                for seg in self.diag_path(*def) {
                    st.push_str(sep);
                    st.push_str(&seg);
                    sep = "::"
                }
                Symbol::intern_by_val(st)
            }

            Type::Pointer(_, _) => todo!("pointer"),
            Type::Array(_, _) => todo!("array"),
            Type::Reference(_, _, _) => todo!("reference"),
            Type::UnresolvedLangItem(_, _)
            | Type::IncompleteAlias(_)
            | Type::Inferable(_)
            | Type::InferableInt(_)
            | Type::Param(_)
            | Type::TraitSelf(_)
            | Type::DropFlags(_) => panic!("Invalid Internal type for `type_name`"),
        }
    }
}

impl Definitions {
    pub fn evaluate_const_expr(
        &self,
        cx: &Spanned<cx::ConstExpr>,
        at_item: DefId,
        containing_item: DefId,
    ) -> Result<cx::ConstExpr> {
        match &cx.body {
            cx::ConstExpr::MirVal(_) => todo!("evaluate mir"),
            cx::ConstExpr::HirVal(_) => panic!("Hir must be expanded before evaluation"),
            cx::ConstExpr::Param(_) => panic!("Substitute Generics before evaluation"),
            cx::ConstExpr::IntConst(ity, val) => Ok(cx::ConstExpr::IntConst(*ity, *val)),
            cx::ConstExpr::Const(defid, generics) => {
                Ok(cx::ConstExpr::Const(*defid, generics.clone()))
            }
            cx::ConstExpr::Constructor(ctor) => {
                let ctor_id = ctor.ctor_id;
                let generics = ctor.generics.clone();
                let fields = ctor
                    .fields
                    .iter()
                    .map(|Pair(field, val)| {
                        Ok((
                            field.clone(),
                            val.try_copy_span(|_| {
                                self.evaluate_const_expr(val, at_item, containing_item)
                            })?,
                        ))
                    })
                    .collect::<Result<_>>()?;

                Ok(cx::ConstExpr::Constructor(ConstExprConstructor {
                    ctor_id,
                    generics,
                    fields,
                }))
            }
        }
    }

    pub fn evaluate_as_u64(
        &self,
        cx: &Spanned<cx::ConstExpr>,
        at_item: DefId,
        containing_item: DefId,
    ) -> Result<u64> {
        match self.evaluate_const_expr(cx, at_item, containing_item)? {
            cx::ConstExpr::HirVal(_) | cx::ConstExpr::MirVal(_) | cx::ConstExpr::Param(_) => {
                unreachable!("Unexpanded Complex Expressions")
            }
            cx::ConstExpr::IntConst(_, val) => val.try_into().map_err(|_| Error {
                span: cx.span,
                text: format!("value {} is not in range", val),
                category: ErrorCategory::ConstEvalError(cx::ConstEvalError::EvaluatorError),
                at_item,
                containing_item,
                relevant_item: at_item,
                hints: vec![],
            }),
            cx::ConstExpr::Const(defid, _) => panic!("{defid} is not a u64"),
            cx::ConstExpr::Constructor(ctor) => panic!("{ctor} is not a u64"),
        }
    }

    pub fn evaluate_as_constructor(
        &self,
        cx: &Spanned<cx::ConstExpr>,
        at_item: DefId,
        containing_item: DefId,
    ) -> Result<DefId> {
        match self.evaluate_const_expr(cx, at_item, containing_item)? {
            cx::ConstExpr::HirVal(_) | cx::ConstExpr::MirVal(_) | cx::ConstExpr::Param(_) => {
                unreachable!("Unexpanded Complex Expressions")
            }
            cx::ConstExpr::IntConst(intty, val) => {
                panic!("{val}_{intty} is not a struct, union, or enum type")
            }
            cx::ConstExpr::Const(defid, _) => {
                panic!("{defid} is not a struct, union, or enum type")
            }
            cx::ConstExpr::Constructor(ctor) => Ok(ctor.ctor_id),
        }
    }

    pub fn evaluate_as_discriminant(
        &self,
        cx: &Spanned<cx::ConstExpr>,
        at_item: DefId,
        containing_item: DefId,
    ) -> Result<u128> {
        let ctor = self.evaluate_as_constructor(cx, at_item, containing_item)?;

        todo!("evaluate_as_discriminant")
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

        for attr in &def.attrs {
            tabs.fmt(fmt)?;
            attr.body.fmt(fmt)?;
            fmt.write_str("\n")?;
        }

        tabs.fmt(fmt)?;
        fmt.write_str("pub(in ")?;
        vis.fmt(fmt)?;
        fmt.write_str(") ")?;

        match &def.inner.body {
            DefinitionInner::Placeholder | DefinitionInner::ExternBlock => {
                unreachable!("This should not happen")
            }
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
                for &Pair(impl_id, def) in &md.impls {
                    let name = format!("impl {:016X}", impl_id);
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
                def.generics.fmt(fmt)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/{\n")?;
                let nested = tabs.nest();
                nested.fmt(fmt)?;
                fmt.write_str("/*incomplete*/\n")?;
                tabs.fmt(fmt)?;
                fmt.write_str("}\n")
            }
            DefinitionInner::UserType(UserType::Struct(kind, ctor)) => {
                match kind {
                    UserTypeKind::Enum => fmt.write_str("enum ")?,
                    UserTypeKind::Struct => fmt.write_str("struct ")?,
                    UserTypeKind::Union => fmt.write_str("union ")?,
                }
                fmt.write_str(item_name)?;
                def.generics.fmt(fmt)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/")?;
                match &ctor.ctor.body {
                    Constructor::Unit => fmt.write_str(";\n"),
                    Constructor::Tuple(fields) => {
                        fmt.write_str("(")?;
                        let mut sep = "";

                        for field in fields {
                            fmt.write_str(sep)?;
                            sep = ", ";

                            field.body.fmt(fmt)?;
                        }
                        fmt.write_str(");\n")
                    }
                    Constructor::Struct(fields) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        for field in fields {
                            nested.fmt(fmt)?;
                            field.body.fmt(fmt)?;
                            fmt.write_str(",\n")?;
                        }
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                }
            }
            DefinitionInner::IncompletAlias => {
                fmt.write_str("type ")?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                def.generics.fmt(fmt)?;
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
                def.generics.fmt(fmt)?;

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
                    Some(FunctionBody::AstBody(_, stmts)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        for stmt in &stmts.stmts {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, stmt.body))?;
                        }
                        if let Some(tail) = &stmts.tail_expr {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, tail.body))?;
                        }
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::HirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::ThirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::MirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(self, fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::Intrinsic(intrin)) => {
                        fmt.write_str(" = ")?;
                        fmt.write_str(intrin.name())?;
                        fmt.write_str(";\n")
                    }
                }
            }
            DefinitionInner::TraitFunction(TraitFnSignature { fnty, has_receiver }, body) => {
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
                def.generics.fmt(fmt)?;

                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/(")?;

                let mut n = 0;
                let mut sep = if *has_receiver { "self: " } else { "" };

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
                    Some(FunctionBody::AstBody(_, stmts)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        for stmt in &stmts.stmts {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, stmt.body))?;
                        }
                        if let Some(tail) = &stmts.tail_expr {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, tail.body))?;
                        }
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::HirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::ThirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::MirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(self, fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::Intrinsic(intrin)) => {
                        fmt.write_str(" = ")?;
                        fmt.write_str(intrin.name())?;
                        fmt.write_str(";\n")
                    }
                }
            }
            DefinitionInner::ImplFunction(TraitFnSignature { fnty, has_receiver }, base, body) => {
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
                def.generics.fmt(fmt)?;

                fmt.write_str(" /*")?;
                id.fmt(fmt)?;

                if let &Some(base) = base {
                    fmt.write_str(" impl(")?;
                    base.fmt(fmt)?;
                    fmt.write_str(")")?;
                }

                fmt.write_str("*/(")?;

                let mut n = 0;
                let mut sep = if *has_receiver { "self: " } else { "" };

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
                    Some(FunctionBody::AstBody(_, stmts)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        for stmt in &stmts.stmts {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, stmt.body))?;
                        }
                        if let Some(tail) = &stmts.tail_expr {
                            fmt.write_fmt(format_args!("{}{:?}\n", nested, tail.body))?;
                        }
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::HirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::ThirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::MirBody(body)) => {
                        fmt.write_str("{\n")?;
                        let nested = tabs.nest();
                        body.display_body(self, fmt, nested)?;
                        tabs.fmt(fmt)?;
                        fmt.write_str("}\n")
                    }
                    Some(FunctionBody::Intrinsic(intrin)) => {
                        fmt.write_str(" = ")?;
                        fmt.write_str(intrin.name())?;
                        fmt.write_str(";\n")
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
                fmt.write_str("*/;\n")
            }
            DefinitionInner::UseWildcard(_) => unreachable!("This should never happen"),
            DefinitionInner::Trait(tr) => {
                tabs.fmt(fmt)?;
                match tr.safety {
                    Safety::Unsafe => fmt.write_str("unsafe ")?,
                    Safety::Safe => {}
                }

                if tr.auto {
                    fmt.write_str("auto ")?;
                }

                fmt.write_str("trait ")?;

                item_name.fmt(fmt)?;
                def.generics.fmt(fmt)?;
                fmt.write_str(" /*")?;
                id.fmt(fmt)?;
                fmt.write_str("*/")?;

                let mut sep = ": ";
                for bound in &tr.supertraits {
                    fmt.write_str(sep)?;
                    sep = " + ";
                    bound.fmt(fmt)?;
                }

                fmt.write_str("{\n")?;

                let nested = tabs.nest();

                for &Pair(name, id) in &tr.assoc_types {
                    self.display_item(fmt, id, &name, nested)?;
                }

                for &Pair(name, id) in &tr.values {
                    self.display_item(fmt, id, &name, nested)?;
                }

                tabs.fmt(fmt)?;
                fmt.write_str("}\n")
            }
            DefinitionInner::Impl(blk) => {
                tabs.fmt(fmt)?;
                if let Safety::Unsafe = blk.safety {
                    fmt.write_str("unsafe ")?;
                }

                fmt.write_str("impl")?;
                def.generics.fmt(fmt)?;
                fmt.write_str(" ")?;
                if let Some(trdef) = blk.trait_def {
                    trdef.fmt(fmt)?;
                    fmt.write_str(" for ")?;
                }

                blk.ty.fmt(fmt)?;
                fmt.write_str(" /*")?;
                fmt.write_str(item_name)?;
                fmt.write_str(" (")?;
                id.fmt(fmt)?;
                fmt.write_str(")*/ {\n")?;
                let nested = tabs.nest();

                for &Pair(name, id) in &blk.assoc_types {
                    self.display_item(fmt, id, &name, nested)?;
                }

                for &Pair(name, id) in &blk.values {
                    self.display_item(fmt, id, &name, nested)?;
                }

                tabs.fmt(fmt)?;
                fmt.write_str("}\n")
            }
        }
    }
}

impl core::fmt::Display for Definitions {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for &Pair(name, crdef) in &self.crates {
            for attr in &self.definition(crdef).attrs {
                attr.body.fmt(f)?;
                f.write_str("\n")?;
            }
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
            attrs,
            ..
        } = self.definition(self.curcrate)
        {
            for attr in attrs {
                attr.body.fmt(f)?;
                f.write_str("\n")?;
            }
            f.write_str("extern crate self /*")?;
            self.curcrate.fmt(f)?;
            f.write_str("*/;\n\n")?;

            let tabs = TabPrinter::new();
            for &star_mod in &md.wildcard_imports {
                f.write_fmt(format_args!("{}use {}::*;\n", tabs, star_mod))?;
            }

            for &Pair(name, def) in &md.types {
                self.display_item(f, def, &name, tabs)?;
            }
            for &Pair(impl_id, def) in &md.impls {
                let name = format!("impl {:016X}", impl_id);
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
        impls: HashMap::new(),
    };
    for item in &md.items {
        let _span = item.span;

        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let defid = defs.allocate_defid(md.name.body);

                let def = defs.definition_mut(defid);
                def.parent = curmod;
                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, defid, defid))
                    .collect::<Result<_>>()?;

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
    let def = defs.definition_mut(curmod);

    def.inner = md.copy_span(|_| DefinitionInner::Module(ret));

    def.attrs = md
        .attrs
        .iter()
        .map(|attr| attr::parse_meta(attr, curmod, curmod))
        .collect::<Result<_>>()?;

    Ok(())
}

fn convert_visibility(
    _defs: &mut Definitions,
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
                let defid = defs.allocate_defid(uty.name.body);
                let utys = match &uty.body.body {
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
                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, defid, curmod))
                    .collect::<Result<_>>()?;

                def.parent = curmod;
                def.inner = uty.copy_span(|_| DefinitionInner::UserType(utys));
                def.visible_from = visible_from;

                defs.insert_type(curmod, uty.name, defid)?;
                defs.collect_lang_items(defid, &[LangItemTarget::Type])?;
            }
            ast::ItemBody::Use(_) => todo!("use"),
            ast::ItemBody::Value(_)
            | ast::ItemBody::ExternCrate { .. }
            | ast::ItemBody::Function(_)
            | ast::ItemBody::ExternBlock(_) => {}
            ast::ItemBody::MacroRules(_) => {
                unreachable!("macros are expanded before semantic analysis")
            }
            ast::ItemBody::Trait(tr) => {
                let defid = defs.allocate_defid(tr.name.body);
                let trbody = TraitDef {
                    auto: tr.auto.is_some(),
                    safety: tr.safety.map(|s| s.body).unwrap_or(Safety::Safe),
                    supertraits: vec![],
                    assoc_types: HashMap::new(),
                    values: HashMap::new(),
                };

                let def = defs.definition_mut(defid);
                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, defid, curmod))
                    .collect::<Result<_>>()?;

                def.parent = curmod;

                for item in &tr.body.body {
                    match &item.body.item.body {
                        ast::ItemBody::Value(_) | ast::ItemBody::Function(_) => {}
                        item => unreachable!(
                            "{:?} Bad item. This should be filtered during ast validation",
                            item
                        ),
                    }
                }

                def.inner = tr.copy_span(|_| DefinitionInner::Trait(trbody));
                def.visible_from = visible_from;

                defs.insert_type(curmod, tr.name, defid)?;
                defs.collect_lang_items(defid, &[LangItemTarget::Trait])?;
            }
            ast::ItemBody::ImplBlock(_) => {}
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
    outer_defid: Option<DefId>,
    _self_ty: Option<&ty::Type>,
) -> Result<(DefinitionInner, GenericParams)> {
    let actual_tag = itemfn
        .abi
        .map(|tag| {
            tag.try_map_span(|tag| {
                tag.tag.map_or(Ok(ty::AbiTag::C { unwind: false }), |tag| {
                    ty::convert_tag(tag, curmod, item)
                })
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
        span: Span::synthetic(),
    });
    let asyncness = itemfn.is_async.map_or(
        Spanned {
            body: ty::AsyncType::Normal,
            span: Span::synthetic(),
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
        .map(|ty| ty.try_copy_span(|ty| ty::convert_type(defs, curmod, item, ty, None)))
        .collect::<Result<Vec<_>>>()?;

    let retty = Box::new(
        itemfn
            .ret_ty
            .as_ref()
            .map(|ty| ty.try_copy_span(|ty| ty::convert_type(defs, curmod, item, ty, None)))
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

    let mut fnty = FnType {
        safety,
        constness,
        asyncness,
        tag,
        retty,
        paramtys,
        iscvarargs,
    };

    let mut generic_params = GenericParams {
        params: vec![],
        by_name: HashMap::new(),
    };

    for generic in itemfn.generics.iter().flat_map(|generics| &generics.params) {
        match &generic.body {
            ast::GenericParam::Type(ty) => {
                let idx = generic_params.params.len() as u32;
                generic_params.params.push(GenericParamInfo::Type(vec![])); // TODO: Bounds
                generic_params.by_name.insert(ty.name, idx);
            }
            _ => todo!("Value and lifetimes"),
        }
    }

    let fnbody = match &itemfn.body.body {
        Some(_) => Some((FunctionBody::Incomplete)),
        None => {
            if let Some(Spanned {
                body: ty::AbiTag::RustIntrinsic,
                ..
            }) = outer_tag
            {
                if let Some(intrin) = intrin::IntrinsicDef::from_name(&itemfn.name) {
                    let sig = intrin.signature();

                    fnty.safety = sig.safety;
                    fnty.constness = sig.constness;

                    fnty.paramtys.iter()
                        .zip(&sig.paramtys)
                        .enumerate()
                        .map(|(n,(def,intrin))| {
                            if def.body != intrin.body{
                                Err(Error {
                                    span: itemfn.name.span,
                                    text: format!(
                                        "Wrong signature for intrinsic function `{}`",
                                        itemfn.name.body
                                    ),
                                    category: ErrorCategory::InvalidFunction,
                                    at_item: item,
                                    containing_item: curmod,
                                    relevant_item: item,
                                    hints: vec![SemaHint {
                                        text: format!(
                                            "Declared in an `extern \"rust-intrinsics\"` block here"
                                        ),
                                        itemref: outer_defid.unwrap(),
                                        refspan: outer_span.unwrap(),
                                    }, SemaHint {
                                        text: format!("Expected type `{}` for parameter {}, got type `{}` instead", intrin.body,n,def.body),
                                        itemref: item,
                                        refspan: def.span
                                    }],
                                })
                            }else{
                                Ok(())
                            }
                        })
                        .try_for_each(|e|e)?;

                    if fnty.retty.body != sig.retty.body {
                        return Err(Error {
                            span: itemfn.name.span,
                            text: format!(
                                "Wrong signature for intrinsic function `{}`",
                                itemfn.name.body
                            ),
                            category: ErrorCategory::InvalidFunction,
                            at_item: item,
                            containing_item: curmod,
                            relevant_item: item,
                            hints: vec![
                                SemaHint {
                                    text: format!(
                                        "Declared in an `extern \"rust-intrinsics\"` block here"
                                    ),
                                    itemref: outer_defid.unwrap(),
                                    refspan: outer_span.unwrap(),
                                },
                                SemaHint {
                                    text: format!(
                                        "Expected return type to be `{}` got type `{}` instead",
                                        sig.retty.body, fnty.retty.body
                                    ),
                                    itemref: item,
                                    refspan: fnty.retty.span,
                                },
                            ],
                        });
                    }

                    Some((FunctionBody::Intrinsic(intrin)))
                } else {
                    return Err(Error {
                        span: itemfn.name.span,
                        text: format!("Unrecognized intrinsic `{}`", itemfn.name.body),
                        category: ErrorCategory::InvalidFunction,
                        at_item: item,
                        containing_item: curmod,
                        relevant_item: item,
                        hints: vec![SemaHint {
                            text: format!("Declared in an `extern \"rust-intrinsics\"` block here"),
                            itemref: outer_defid.unwrap(),
                            refspan: outer_span.unwrap(),
                        }],
                    });
                }
            } else {
                None
            }
        }
    };

    Ok((DefinitionInner::Function(fnty, fnbody), generic_params))
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
                let defid = defs.allocate_defid(itemfn.name.body);
                let (inner, generics) =
                    collect_function(defs, curmod, defid, itemfn, None, None, None, None, None)?;

                let def = defs.definition_mut(defid);
                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, defid, curmod))
                    .collect::<Result<_>>()?;

                def.generics = generics;

                def.visible_from = visible_from;
                def.parent = curmod;
                def.inner = itemfn.copy_span(|_| inner);

                defs.insert_value(curmod, itemfn.name, defid)?;
                defs.collect_lang_items(defid, &[LangItemTarget::Function])?;
            }
            ast::ItemBody::ExternBlock(blk) => {
                let extern_defid = defs.allocate_defid(Symbol::intern("<extern block>"));

                let def = defs.definition_mut(extern_defid);

                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, extern_defid, curmod))
                    .collect::<Result<_>>()?;
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
                            let defid = defs.allocate_defid(itemfn.name.body);
                            let (inner, generics) = collect_function(defs, curmod, defid, itemfn, Some(tag), Some(Spanned{body: Safety::Unsafe, span: Span::empty()}), Some(blk.span),Some(extern_defid),None)?;

                            let def = defs.definition_mut(defid);
                            def.attrs = item.attrs.iter().map(|attr|attr::parse_meta(attr, defid, defid))
                                .collect::<Result<_>>()?;

                            // TODO: check for override attribute

                            def.attrs.push(Spanned{body: attr::Attr::NoMangle,span: blk.span});

                            def.generics = generics;
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
                                at_item: extern_defid,
                                containing_item: curmod,
                                relevant_item: extern_defid,
                                hints: vec![SemaHint{ text: format!("Declared inside this extern block"), itemref: extern_defid, refspan: blk.span }]
                            })
                        }
                    }
                }
            }
            ast::ItemBody::MacroRules(_) => {
                unreachable!("macros are expanded before semantic analysis")
            }
            ast::ItemBody::Trait(tr) => {
                let defid = defs.find_type_in_mod(curmod, curmod, tr.name, curmod)?;
                for item in &tr.body.body {
                    match &item.body.item.body {
                        ast::ItemBody::Value(_) => {
                            todo!("Associated Const in Trait")
                        }
                        ast::ItemBody::Function(f) => {
                            let ty = Type::TraitSelf(defid);
                            let fndef = defs.allocate_defid(f.name.body);
                            todo!("trait body {} {}", ty, fndef);
                        }
                        item => unreachable!(
                            "{:?} Bad item. This should be filtered during ast validation",
                            item
                        ),
                    }
                }
            }
            ast::ItemBody::ImplBlock(blk) => {
                let defid = defs.allocate_defid(Symbol::intern("<impl block>"));
                if let Some(vis) = item.vis.as_ref() {
                    return Err(Error {
                        span: vis.span,
                        text: format!("impl blocks cannot have a visibility"),
                        category: ErrorCategory::InvisibleEntity,
                        at_item: defid,
                        containing_item: curmod,
                        relevant_item: visible_from,
                        hints: vec![],
                    });
                }
                let ty = blk
                    .ty
                    .try_copy_span(|ty| convert_type(defs, curmod, defid, ty, None))?;
                let trbody = ImplBlock {
                    safety: blk.safety.map(|s| s.body).unwrap_or(Safety::Safe),
                    trait_def: blk
                        .tr
                        .as_ref()
                        .map(|p| {
                            p.try_copy_span(|p| {
                                defs.find_type(curmod, p, defid, Some(&ty))
                                    .map(|(def, _)| def) // FIXME: Generics
                            })
                        })
                        .transpose()?,
                    assoc_types: HashMap::new(),
                    values: HashMap::new(),
                    ty: ty,
                    provides: HashMap::new(),
                };

                let visible_from = defs.visibility_of(
                    trbody
                        .trait_def
                        .map(|s| s.body)
                        .unwrap_or_else(|| defs.type_defid(&trbody.ty)),
                );
                // Note: primitive

                let def = defs.definition_mut(defid);
                def.attrs = item
                    .attrs
                    .iter()
                    .map(|attr| attr::parse_meta(attr, defid, curmod))
                    .collect::<Result<_>>()?;

                def.parent = curmod;

                for item in &blk.body.body {
                    match &item.body.item.body {
                        ast::ItemBody::Value(_) | ast::ItemBody::Function(_) => {
                            todo!("item in trait")
                        }
                        item => unreachable!(
                            "{:?} Bad item. This should be filtered during ast validation",
                            item
                        ),
                    }
                }

                def.inner = blk.copy_span(|_| DefinitionInner::Impl(trbody));
                def.visible_from = visible_from;

                defs.insert_impl(curmod, blk.impl_id.body, defid)?;
                defs.collect_lang_items(defid, &[LangItemTarget::ImplBlock])?;
            }
        }
    }

    Ok(())
}

pub fn convert_types(defs: &mut Definitions, curmod: DefId, md: &Spanned<ast::Mod>) -> Result<()> {
    for item in &md.items {
        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let defid = defs.find_type_in_mod(curmod, curmod, md.name, curmod)?;
                convert_types(defs, defid, md.content.as_ref().unwrap())?;
            }
            ast::ItemBody::UserType(uty) => {
                let defid = defs.find_type_in_mod(curmod, curmod, uty.name, curmod)?;
                match &uty.body.body {
                    ast::UserTypeBody::Struct(ty, ctor) => {
                        let utykind = match ty.body {
                            ast::StructKind::Union => UserTypeKind::Union,
                            ast::StructKind::Struct => UserTypeKind::Struct,
                        };

                        let ctor = ctor.try_copy_span(|ctor| match ctor {
                            ast::Constructor::Struct(ctor) => {
                                let mut fields = Vec::new();
                                for field in &ctor.fields {
                                    fields.push(field.try_copy_span(|field| {
                                        let attrs = Vec::new();
                                        let visible_from =
                                            convert_visibility(defs, curmod, field.vis.as_ref())?
                                                .unwrap_or(curmod);
                                        let name = field.name;
                                        let ty = field.ty.try_copy_span(|ty| {
                                            ty::convert_type(
                                                defs,
                                                curmod,
                                                defid,
                                                ty,
                                                Some(&Type::UserType(
                                                    defid,
                                                    generics::GenericArgs::default(),
                                                )), // TODO: Grab generics
                                            )
                                        })?;
                                        Ok(StructField {
                                            attrs,
                                            visible_from,
                                            name,
                                            ty,
                                        })
                                    })?);
                                }

                                Ok(Constructor::Struct(fields))
                            }
                            ast::Constructor::Tuple(_) => todo!(),
                            ast::Constructor::Unit => Ok(Constructor::Unit),
                        })?;

                        defs.definition_mut(defid).inner.body = DefinitionInner::UserType(
                            UserType::Struct(utykind, StructDefinition { ctor }),
                        );
                    }
                    ast::UserTypeBody::Enum(_) => todo!("enum"),
                }
            }
            ast::ItemBody::Value(_) => {}
            ast::ItemBody::ExternCrate { .. } => {}
            ast::ItemBody::Use(_) => {}
            ast::ItemBody::Function(_) => {}
            ast::ItemBody::ExternBlock(_) => {}
            ast::ItemBody::MacroRules(_) => {
                unreachable!("macros are expanded before semantic analysis")
            }
            ast::ItemBody::Trait(_) => {}
            ast::ItemBody::ImplBlock(blk) => {
                let defid = defs.find_impl_in_mod(curmod, curmod, blk.impl_id, curmod)?;

                let mut impl_blk = core::mem::replace(
                    &mut defs.definition_mut(defid).inner.body,
                    DefinitionInner::Placeholder,
                );

                match &mut impl_blk {
                    DefinitionInner::Impl(impl_blk) => {
                        let cr = defs.owning_crate(defs.type_defid(&impl_blk.ty));

                        if let Some(tr) = impl_blk.trait_def {
                            let tr_cr = defs.owning_crate(tr.body);

                            if defs.curcrate != tr_cr && defs.curcrate != cr {
                                return Err(Error {
                                    span: blk.impl_id.span,
                                    text: format!(
                                        "Attempt to implement foreign trait {:?} for foreign type {:?}",
                                        blk.tr.as_ref().unwrap().body,
                                        blk.ty.body
                                    ),
                                    category: ErrorCategory::BasicCoherenceError,
                                    at_item: defid,
                                    containing_item: curmod,
                                    relevant_item: tr.body,
                                    hints: vec![],
                                });
                            }
                        } else {
                            // TODO: Support `#[lang = "f32_std"]`/`#[lang = "f64_std"]`
                            if defs.curcrate != cr {
                                return Err(Error {
                                    span: blk.impl_id.span,
                                    text: format!(
                                        "Attempt to define inherent impl for foreign type {:?}",
                                        blk.ty.body
                                    ),
                                    category: ErrorCategory::BasicCoherenceError,
                                    at_item: defid,
                                    containing_item: curmod,
                                    relevant_item: cr,
                                    hints: vec![],
                                });
                            }
                        }
                        for item in &blk.body.body {
                            match &item.body.item.body {
                                ast::ItemBody::Value(_) | ast::ItemBody::Function(_) => {
                                    todo!("item in trait")
                                }
                                item => unreachable!(
                                    "{:?} Bad item. This should be filtered during ast validation",
                                    item
                                ),
                            }
                        }
                    }
                    _ => panic!("Not an impl but we have an impl"),
                }

                defs.definition_mut(defid).inner.body = impl_blk;
            }
        }
    }

    Ok(())
}

pub fn convert_values(defs: &mut Definitions, curmod: DefId, md: &Spanned<ast::Mod>) -> Result<()> {
    for item in &md.items {
        match &item.item.body {
            ast::ItemBody::Mod(md) => {
                let name = md.name;

                let semamod = defs.find_type_in_mod(curmod, curmod, name, curmod)?;

                convert_values(
                    defs,
                    semamod,
                    md.content
                        .as_ref()
                        .expect("out-of-line mods aren't a thing in sema"),
                )?;
            }
            ast::ItemBody::Value(_) => todo!("static or const"),

            ast::ItemBody::Function(fbody) => {
                let name = fbody.name;

                let fndef = defs.find_value_in_mod(curmod, curmod, name, curmod)?;

                if let Definition {
                    inner:
                        Spanned {
                            body: DefinitionInner::Function(_, body),
                            ..
                        },
                    ..
                } = defs.definition_mut(fndef)
                {
                    if let Some(ast_body) = &fbody.body.body {
                        *body = Some(FunctionBody::AstBody(
                            fbody.params.clone(),
                            ast_body.clone(),
                        ));
                    } else {
                        return Err(Error {
                            span: fbody.span,
                            text: format!("Expected fn {} to have a body", name.body),
                            category: ErrorCategory::InvalidFunction,
                            at_item: fndef,
                            containing_item: curmod,
                            relevant_item: fndef,
                            hints: vec![],
                        });
                    }
                } else {
                    panic!(
                        "Expected {} (defid {}) to name a function",
                        name.body, fndef
                    );
                }
            }
            ast::ItemBody::Use(_)
            | ast::ItemBody::UserType(_)
            | ast::ItemBody::ExternCrate { .. }
            | ast::ItemBody::ExternBlock(_) => {}
            ast::ItemBody::MacroRules(_) => unreachable!(),
            ast::ItemBody::Trait(_) => {}
            ast::ItemBody::ImplBlock(_) => {}
        }
    }

    Ok(())
}

pub fn desugar_fn(defs: &mut Definitions, curmod: DefId, defid: DefId) -> Result<()> {
    let def = defs.definition_mut(defid);
    match &mut def.inner.body {
        DefinitionInner::Function(fnty, val @ Some(FunctionBody::AstBody(_, _))) => {
            match (fnty.clone(), val.take()) {
                (fnty, Some(FunctionBody::AstBody(params, block))) => {
                    let parent = def.parent;

                    let selfty = match &defs.definition(parent).inner.body {
                        DefinitionInner::Module(_) => None,
                        DefinitionInner::Impl(blk) => Some(blk.ty.body.clone()),
                        DefinitionInner::Trait(_) => Some(Type::TraitSelf(parent)),
                        _ => None,
                    };

                    let mut vardebugmap = HashMap::new();
                    let safety = fnty.safety.body;
                    let span = block.span;
                    let mut localitems = Vec::new();
                    let mut builder = HirLowerer::new(
                        defs,
                        defid,
                        curmod,
                        &mut vardebugmap,
                        selfty.as_ref(),
                        &mut localitems,
                    );

                    for param in params {
                        match &param.pat {
                            Some(Spanned {
                                body: ast::Pattern::BareId(sym),
                                ..
                            }) => {
                                let var_id = hir::HirVarId(builder.nexthirvarid);
                                builder.vardebugmap.insert(var_id, *sym);
                                builder.varnames.insert(sym.body, var_id);
                                builder.nexthirvarid += 1;
                            }
                            _ => todo!(),
                        }
                    }

                    builder
                        .desugar_block(Some(&mut |expr| hir::HirStatement::Return(expr)), &block)?;

                    let body = builder.into_fn_body(safety, span)?;

                    match &mut defs.definition_mut(defid).inner.body {
                        DefinitionInner::Function(_, val) => {
                            *val = Some(FunctionBody::HirBody(block.copy_span(|_| body)))
                        }
                        _ => unreachable!(),
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
    Ok(())
}

pub fn desugar_values(defs: &mut Definitions, curmod: DefId) -> Result<()> {
    let tys = defs.as_module(curmod).types.clone();

    for &Pair(_, defid) in &tys {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(_),
                    ..
                },
            ..
        } = defs.definition(defid)
        {
            desugar_values(defs, defid)?;
        }
    }

    let values = defs.as_module(curmod).values.clone();

    for &Pair(_, defid) in &values {
        let def = defs.definition(defid);
        match &def.inner.body {
            DefinitionInner::Function(_, Some(FunctionBody::AstBody(_, _))) => {
                desugar_fn(defs, curmod, defid)?;
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn tycheck_function(defs: &mut Definitions, curmod: DefId, defid: DefId) -> Result<()> {
    let def = defs.definition_mut(defid);
    match &mut def.inner.body {
        DefinitionInner::Function(fnty, val @ Some(FunctionBody::HirBody(_))) => match val.take() {
            Some(FunctionBody::HirBody(block)) => {
                let fnty = fnty.clone();

                let localitems = block.body.localitems;

                for &(_, defid) in &localitems {
                    tycheck_function(defs, curmod, defid)?;
                }

                let (stmts, safety) = match block.body.body.body {
                    hir::HirBlock::Normal(stmts) => (stmts, Safety::Safe),
                    hir::HirBlock::Unsafe(stmts) => (stmts, Safety::Unsafe),
                    hir::HirBlock::If { .. } | hir::HirBlock::Loop(_) | hir::HirBlock::Match(_) => {
                        unreachable!()
                    }
                };

                let mut converter = tyck::ThirConverter::new(
                    defs,
                    safety,
                    defid,
                    curmod,
                    block.body.vardebugmap,
                    localitems,
                    fnty,
                );

                for stmt in stmts {
                    converter.write_statement(&stmt)?;
                }

                let mut body = converter.into_thir_body(block.body.body.span);

                let mut inferer = Inferer::new(&defs, defid, curmod, &mut body.vardefs);

                let mut iter_count = 0;
                const MAX_ITER: usize = 4096;

                loop {
                    if inferer.unify_block(&mut body.body.body)?.is_complete() {
                        break;
                    }

                    if inferer.propagate_block(&mut body.body.body)?.is_complete() {
                        break;
                    }

                    iter_count += 1;

                    if iter_count > MAX_ITER {
                        panic!("Semantic analyzer failed to resolve types. This is an ICE in this case.");
                    }
                }

                match &mut defs.definition_mut(defid).inner.body {
                    DefinitionInner::Function(_, val) => {
                        *val = Some(FunctionBody::ThirBody(Spanned {
                            body,
                            span: block.span,
                        }))
                    }
                    _ => unreachable!(),
                }
            }
            _ => {}
        },
        _ => {}
    }
    Ok(())
}

pub fn tycheck_values(defs: &mut Definitions, curmod: DefId) -> Result<()> {
    let tys = defs.as_module(curmod).types.clone();

    for &Pair(_, defid) in &tys {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(_),
                    ..
                },
            ..
        } = defs.definition(defid)
        {
            tycheck_values(defs, defid)?;
        }
    }

    let values = defs.as_module(curmod).values.clone();

    for &Pair(_, defid) in &values {
        let def = defs.definition_mut(defid);
        match &mut def.inner.body {
            DefinitionInner::Function(_, Some(FunctionBody::HirBody(_))) => {
                tycheck_function(defs, curmod, defid)?;
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn lower_function(defs: &mut Definitions, curmod: DefId, defid: DefId) -> Result<()> {
    let def = defs.definition_mut(defid);
    match &mut def.inner.body {
        DefinitionInner::Function(fnty, val @ Some(FunctionBody::ThirBody(_))) => {
            match val.take() {
                Some(FunctionBody::ThirBody(block)) => {
                    let fnty = fnty.clone();
                    let localitems = block.body.localitems;

                    for &(_, defid) in &localitems {
                        lower_function(defs, curmod, defid)?;
                    }

                    let mut lowerer = mir::MirConverter::new(
                        defs,
                        defid,
                        curmod,
                        block.body.vardefs,
                        localitems,
                        fnty,
                    );
                    let stmts = match block.body.body.body {
                        ThirBlock::Normal(stmts) | ThirBlock::Unsafe(stmts) => stmts,
                        _ => unreachable!(),
                    };

                    for stmt in stmts {
                        lowerer.write_statement(stmt)?;
                    }

                    let body = lowerer.finish();

                    match &mut defs.definition_mut(defid).inner.body {
                        DefinitionInner::Function(_, ibody) => {
                            *ibody = Some(FunctionBody::MirBody(Spanned {
                                span: block.span,
                                body,
                            }));
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            }
        }
        _ => {}
    }
    Ok(())
}

pub fn mir_lower(defs: &mut Definitions, curmod: DefId) -> Result<()> {
    let tys = defs.as_module(curmod).types.clone();

    for &Pair(_, defid) in &tys {
        if let Definition {
            inner:
                Spanned {
                    body: DefinitionInner::Module(_),
                    ..
                },
            ..
        } = defs.definition(defid)
        {
            tycheck_values(defs, defid)?;
        }
    }

    let values = defs.as_module(curmod).values.clone();

    for &Pair(_sym, defid) in &values {
        let def = defs.definition_mut(defid);
        match &mut def.inner.body {
            DefinitionInner::Function(_, Some(FunctionBody::ThirBody(_))) => {
                lower_function(defs, curmod, defid)?;
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn run_passes_on(body: &mut MirFunctionBody, max_opt: u32) -> Result<()> {
    for pass in mir::transform::REQ_PASSES {
        pass.accept_function(body)?;
    }

    for pass in mir::transform::OPT_PASSES {
        match pass.pass_type() {
            mir::transform::MirPassType::Optional(stat) if stat > max_opt => {}
            _ => {
                pass.accept_function(body)?;
            }
        }
    }

    Ok(())
}

pub fn run_passes(defs: &mut Definitions, curmod: DefId, max_opt: u32) -> Result<()> {
    for Pair(&defid, def) in &mut defs.defs {
        match &mut def.inner.body {
            DefinitionInner::Function(_, Some(FunctionBody::MirBody(body))) => {
                run_passes_on(body, max_opt)?;
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn find_main(defs: &mut Definitions, root: DefId) -> Result<(DefId, Span)> {
    if let Some(defid) = defs.get_lang_item(LangItem::Main) {
        Ok((defid, defs.definition(defid).inner.span))
    } else {
        let defid = defs.find_value_in_mod(
            root,
            root,
            Spanned {
                body: Symbol::intern("main"),
                span: Span::synthetic(),
            },
            root,
        )?;

        Ok((defid, defs.definition(defid).inner.span))
    }
}

pub fn convert_crate(
    defs: &mut Definitions,
    md: &Spanned<ast::Mod>,
    cr_type: CrateType,
    crate_name: Symbol,
) -> Result<()> {
    let root = defs.allocate_defid(crate_name);
    defs.set_current_crate(root);

    scan_modules(defs, root, md)?;
    collect_types(defs, root, md)?;
    collect_values(defs, root, md)?;
    convert_types(defs, root, md)?;
    convert_values(defs, root, md)?;

    desugar_values(defs, root)?;
    eprintln!("\n{}", defs);
    tycheck_values(defs, root)?;
    eprintln!("\n{}", defs);
    mir_lower(defs, root)?;

    // TODO: Check if we're actually inside a bin crate

    let is_no_main = defs
        .definition(root)
        .attrs
        .iter()
        .any(|x| **x == Attr::NoMain);

    if cr_type == CrateType::Bin && !is_no_main {
        let lccc_main = defs.allocate_defid(Symbol::intern("__lccc_main"));
        let (main, main_span) = find_main(defs, root)?;

        let ity = ty::IntType::i32;

        let first_block = mir::BasicBlockId(0);

        let fnty = match &defs.definition(main).inner.body {
            DefinitionInner::Function(fnty, _) => fnty.clone(),
            _ => panic!("Invalid item found for `main`"),
        };

        let retty = fnty.retty.body.clone();

        let body = if retty == Type::Never {
            mir::mir! {
                @0: { []
                    tailcall #<main>: <fnty>()
                }
            }
        } else {
            mir::mir! {
                @0: { []
                    call _0 = #<main>: <fnty>() next fallthrough @1 []
                }
                @1: { [_0: <retty>]
                    store dead _0;
                    return 0_i32
                }
            }
        };

        let fnty = mir::mir_fnty!(unsafe extern "lcrust-v0" fn() -> i32);
        let def = defs.definition_mut(lccc_main);

        def.inner = Spanned {
            body: DefinitionInner::Function(
                fnty,
                Some(FunctionBody::MirBody(Spanned {
                    body,
                    span: main_span,
                })),
            ),
            span: main_span,
        };
        def.attrs = vec![Spanned {
            body: attr::Attr::NoMangle,
            span: main_span,
        }];
        def.visible_from = lccc_main;

        defs.insert_value(
            root,
            Spanned {
                body: Symbol::intern("__lccc_main"),
                span: Span::synthetic(),
            },
            lccc_main,
        )?;
    }

    run_passes(defs, root, 1024)?;

    Ok(())
}
