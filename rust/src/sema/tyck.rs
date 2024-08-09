use xlang::abi::collection::{HashMap, HashSet};

use crate::{
    ast::{self, CharType, Mutability, Safety, StringType},
    helpers::{CyclicOperationStatus, FetchIncrement, TabPrinter},
    interning::Symbol,
    span::Span,
};

pub use super::hir::{BinaryOp, PatternPathFragment, UnaryOp};
pub use super::ty::Type;

use super::{
    cx::ConstExpr,
    generics::GenericArgs,
    hir::{HirPatternBinding, HirVarId},
    ty::{FieldName, FnType, IntType, SemaLifetime},
    DefId, DefinitionInner, Definitions, Error, ErrorCategory, Result, SemaHint, Spanned,
};

use CyclicOperationStatus::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TycheckErrorCategory {
    InferenceFailure,
    NoConversion,
    FailedToUnify,
    DuplicateField,
    MissingField,
    NoSuchField,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct InferId(u32);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Movability {
    Movable,
    Unmovable,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValueCategory {
    Rvalue,
    Lvalue(Movability, Mutability),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirPattern {
    pub matcher: Spanned<ThirPatternMatcher>,
    pub bindings: Vec<Spanned<HirPatternBinding>>,
}

impl core::fmt::Display for ThirPattern {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.matcher.fmt(f)?;

        f.write_str(" @ {")?;

        let mut sep = "";

        for binding in &self.bindings {
            f.write_str(sep)?;
            sep = ", ";
            binding.body.fmt(f)?;
        }
        f.write_str("}")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirPatternMatcher {
    pub ty: Type,
    pub inner: ThirMatcherInner,
}

impl core::fmt::Display for ThirPatternMatcher {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.inner.fmt(f)?;
        f.write_str(": ")?;
        self.ty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ThirMatcherInner {
    Hole,
    ConstInt(u128, Option<Spanned<IntType>>),
    Const(DefId),
}

impl core::fmt::Display for ThirMatcherInner {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Hole => f.write_str("_"),
            Self::ConstInt(val, intty) => {
                val.fmt(f)?;
                if let Some(intty) = intty {
                    f.write_str("_")?;
                    intty.body.fmt(f)?;
                }
                Ok(())
            }
            Self::Const(def) => def.fmt(f),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirExpr {
    pub ty: Type,
    pub cat: ValueCategory,
    pub inner: ThirExprInner,
}

impl core::fmt::Display for ThirExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.inner.fmt(f)?;
        f.write_str(": ")?;

        if let ValueCategory::Lvalue(mv, mt) = self.cat {
            f.write_str("ref ")?;
            if mt == Mutability::Mut {
                f.write_str("mut ")?;
            }
            if mv == Movability::Movable {
                f.write_str("move ")?;
            }
        }

        self.ty.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ThirExprInner {
    Read(Box<Spanned<ThirExpr>>),
    Unreachable,
    Var(HirVarId),
    Const(DefId, GenericArgs),
    ConstInt(Option<Spanned<IntType>>, u128),
    ConstString(StringType, Spanned<Symbol>),
    ConstChar(CharType, u32),
    Cast(Box<Spanned<ThirExpr>>, Spanned<Type>),
    Tuple(Vec<Spanned<ThirExpr>>),
    Ctor(Spanned<ThirConstructor>),
    MemberAccess(Box<Spanned<ThirExpr>>, Spanned<FieldName>),
    UnaryExpr(Spanned<UnaryOp>, Box<Spanned<ThirExpr>>),
    BinaryExpr(
        Spanned<BinaryOp>,
        Box<Spanned<ThirExpr>>,
        Box<Spanned<ThirExpr>>,
    ),
    Array(Vec<Spanned<ThirExpr>>),
    Index(Box<Spanned<ThirExpr>>, Box<Spanned<ThirExpr>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirConstructor {
    pub base: DefId,
    pub fields: Vec<(Spanned<FieldName>, Spanned<ThirExpr>)>,
    pub rest_init: Option<Box<Spanned<ThirExpr>>>,
}

impl core::fmt::Display for ThirConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.base.fmt(f)?;
        f.write_str("{ ")?;

        let mut sep = "";

        for (field, init) in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            field.body.fmt(f)?;
            f.write_str(": (")?;
            init.body.fmt(f)?;
            f.write_str(")")?;
        }

        if let Some(rest_init) = &self.rest_init {
            f.write_str(sep)?;
            f.write_str("..(")?;
            rest_init.body.fmt(f)?;
            f.write_str(")")?;
        }
        f.write_str(" }")
    }
}

impl core::fmt::Display for ThirExprInner {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ThirExprInner::Read(inner) => {
                f.write_str("read(")?;
                inner.body.fmt(f)?;
                f.write_str(")")
            }
            ThirExprInner::Unreachable => f.write_str("unreachable"),
            ThirExprInner::Var(var) => var.fmt(f),
            ThirExprInner::Const(def, generics) => f.write_fmt(format_args!("{}{}", def, generics)),
            ThirExprInner::ConstInt(ity, val) => {
                val.fmt(f)?;
                if let Some(ity) = ity {
                    ity.body.fmt(f)?;
                }
                Ok(())
            }
            ThirExprInner::ConstString(_, str) => {
                f.write_str("\"")?;
                str.escape_default().fmt(f)?;
                f.write_str("\"")
            }
            ThirExprInner::ConstChar(CharType::Default, val) => {
                f.write_str("'")?;
                if let Some(c) = char::from_u32(*val) {
                    c.escape_default().fmt(f)?;
                } else {
                    f.write_fmt(format_args!("\\u{{invalid char: {:04x}}}", val))?;
                }

                f.write_str("'")
            }
            ThirExprInner::ConstChar(CharType::Byte, val) => {
                f.write_str("'")?;
                match val {
                    &val @ 0x20..=0x7F => (val as u8 as char).fmt(f)?,
                    val => f.write_fmt(format_args!("\\x{:02x}", val))?,
                }
                f.write_str("'")
            }
            ThirExprInner::Cast(e, ty) => {
                f.write_str("(")?;
                e.body.fmt(f)?;
                f.write_str(") as ")?;
                ty.body.fmt(f)
            }
            ThirExprInner::Tuple(v) => {
                f.write_str("(")?;
                let mut sep = "";
                for val in v {
                    f.write_str(sep)?;
                    sep = ", ";
                    val.body.fmt(f)?;
                }
                if v.len() == 1 {
                    f.write_str(",")?;
                }
                f.write_str(")")
            }
            ThirExprInner::Ctor(ctor) => ctor.body.fmt(f),
            ThirExprInner::MemberAccess(base, field) => {
                f.write_str("(")?;
                base.body.fmt(f)?;
                f.write_str(").")?;
                field.body.fmt(f)
            }
            ThirExprInner::BinaryExpr(op, lhs, rhs) => {
                write!(f, "({} {} {})", lhs.body, op.body, rhs.body)
            }
            ThirExprInner::UnaryExpr(op, val) => {
                write!(f, "({}{})", op.body, val.body)
            }
            ThirExprInner::Array(elements) => {
                write!(f, "[")?;
                let mut sep = "";
                for element in elements {
                    write!(f, "{}{}", sep, element.body)?;
                    sep = ", ";
                }
                write!(f, "]")
            }
            ThirExprInner::Index(base, index) => {
                write!(f, "{}[{}]", base.body, index.body)
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ThirStatement {
    Assign {
        dest: Spanned<ThirExpr>,
        val: Spanned<ThirExpr>,
        op: Option<Spanned<ast::BinaryOp>>,
    },
    Define {
        mutability: Spanned<Mutability>,
        var: Spanned<HirVarId>,
        ty: Spanned<super::ty::Type>,
    },
    Return(Spanned<ThirExpr>),
    Block(Spanned<ThirBlock>),
    Discard(Spanned<ThirExpr>),
    Call {
        retplace: Option<Spanned<ThirExpr>>,
        fnexpr: Spanned<ThirExpr>,
        method_name: Option<Spanned<Symbol>>,
        params: Vec<Spanned<ThirExpr>>,
    },
}

type ThirSimpleBlock = Vec<Spanned<ThirStatement>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirMatch {
    pub discriminee: Spanned<ThirExpr>,
    pub arms: Vec<ThirMatchArm>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirMatchArm {
    pub discrim: Spanned<ThirPattern>,
    pub guard: Option<Spanned<ThirExpr>>,
    pub expansion: Spanned<ThirSimpleBlock>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ThirBlock {
    Normal(ThirSimpleBlock),
    Unsafe(ThirSimpleBlock),
    Loop(ThirSimpleBlock),
    If {
        cond: Spanned<ThirExpr>,
        block: Box<Spanned<ThirBlock>>,
        elseblock: Option<Box<Spanned<ThirBlock>>>,
    },
    Match(ThirMatch),
}

impl ThirBlock {
    // fn inner_mut(&mut self) -> &mut Vec<Spanned<ThirStatement>> {
    //     match self {
    //         Self::Normal(x) => x,
    //         Self::Unsafe(x) => x,
    //         Self::Loop(x) => x,
    //     }
    // }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirFunctionBody {
    pub vardefs: HashMap<HirVarId, ThirVarDef>,
    pub body: Spanned<ThirBlock>,
    pub localitems: Vec<(Symbol, DefId)>,
}

impl ThirFunctionBody {
    pub fn display_body(
        &self,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        let stmts = match &self.body.body {
            ThirBlock::Normal(stmts) | ThirBlock::Unsafe(stmts) => stmts,
            _ => unreachable!(),
        };

        for stmt in stmts {
            self.display_statement(stmt, f, tabs)?;
        }

        Ok(())
    }

    fn display_block(
        &self,
        block: &ThirBlock,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match &block {
            ThirBlock::Normal(stmts) => {
                f.write_str("{\n")?;
                let nested = tabs.nest();
                for stmt in stmts {
                    self.display_statement(stmt, f, nested)?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
            ThirBlock::Unsafe(stmts) => {
                f.write_str("unsafe {\n")?;
                let nested = tabs.nest();
                for stmt in stmts {
                    self.display_statement(stmt, f, nested)?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
            ThirBlock::Loop(stmts) => {
                f.write_str("loop {\n")?;
                let nested = tabs.nest();
                for stmt in stmts {
                    self.display_statement(stmt, f, nested)?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
            ThirBlock::If {
                cond,
                block,
                elseblock,
            } => {
                write!(f, "if {} ", cond.body)?;
                self.display_block(block, f, tabs)?;
                if let Some(elseblk) = elseblock {
                    write!(f, "{}else ", tabs)?;
                    self.display_block(elseblk, f, tabs)
                } else {
                    Ok(())
                }
            }
            ThirBlock::Match(m) => {
                let nested = tabs.nest();
                write!(f, "match {} {{\n", m.discriminee.body)?;
                for arm in &m.arms {
                    nested.fmt(f)?;
                    let subnested = nested.nest();
                    arm.discrim.body.fmt(f)?;

                    if let Some(guard) = &arm.guard {
                        write!(f, "if {}", guard.body)?;
                    }

                    f.write_str(" => {\n")?;

                    for stmt in &arm.expansion.body {
                        self.display_statement(stmt, f, subnested)?;
                    }
                    nested.fmt(f)?;
                    f.write_str("}\n")?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
        }
    }

    fn display_statement(
        &self,
        stmt: &ThirStatement,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match stmt {
            ThirStatement::Assign { dest, val, op } => {
                tabs.fmt(f)?;
                dest.body.fmt(f)?;

                f.write_str(" ")?;

                if let Some(op) = op {
                    op.body.fmt(f)?;
                }

                f.write_str("= ")?;

                val.body.fmt(f)?;

                f.write_str(";\n")
            }
            ThirStatement::Return(expr) => {
                f.write_fmt(format_args!("{}return {};\n", tabs, expr.body))
            }
            ThirStatement::Block(b) => {
                tabs.fmt(f)?;
                self.display_block(b, f, tabs)
            }
            ThirStatement::Discard(expr) => f.write_fmt(format_args!("{}{};\n", tabs, expr.body)),
            ThirStatement::Call {
                retplace,
                fnexpr,
                method_name,
                params,
            } => {
                tabs.fmt(f)?;
                if let Some(retplace) = retplace {
                    retplace.body.fmt(f)?;
                    f.write_str(" = ")?;
                }

                fnexpr.body.fmt(f)?;
                if let Some(name) = method_name {
                    f.write_str(".")?;
                    f.write_str(name)?;
                }
                f.write_str("(")?;

                let mut sep = "";

                for param in params {
                    f.write_str(sep)?;
                    sep = ", ";
                    param.body.fmt(f)?;
                }
                f.write_str(");\n")
            }
            ThirStatement::Define {
                mutability,
                var,
                ty,
            } => {
                tabs.fmt(f)?;
                f.write_str("let ")?;
                if mutability.body == Mutability::Mut {
                    f.write_str("mut ")?;
                }

                var.body.fmt(f)?;

                if let Some(name) = self.vardefs[&var.body].debug_name {
                    f.write_str(" /* ")?;
                    f.write_str(&name)?;
                    f.write_str(" */")?;
                }

                f.write_str(": ")?;
                ty.body.fmt(f)?;

                f.write_str(";\n") // Note: Assignment is on a different line
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirVarDef {
    pub mt: Spanned<Mutability>,
    pub ty: Spanned<Type>,
    pub debug_name: Option<Spanned<Symbol>>,
}

use super::hir;

pub struct ThirConverter<'a> {
    defs: &'a Definitions,
    inference_set: HashMap<InferId, Type>,
    var_defs: HashMap<HirVarId, ThirVarDef>,
    safety: Safety,
    stmts: Vec<Spanned<ThirStatement>>,
    at_item: DefId,
    containing_item: DefId,
    next_infer: u32,
    dbgnames: HashMap<HirVarId, Spanned<Symbol>>,
    localitems: Vec<(Symbol, DefId)>,
}

impl<'a> ThirConverter<'a> {
    pub fn new(
        defs: &'a Definitions,
        safety: Safety,
        at_item: DefId,
        containing_item: DefId,
        dbgnames: HashMap<HirVarId, Spanned<Symbol>>,
        localitems: Vec<(Symbol, DefId)>,
        fnty: FnType,
    ) -> ThirConverter<'a> {
        let mut result = ThirConverter {
            defs,
            inference_set: HashMap::new(),
            var_defs: HashMap::new(),
            safety: Safety::Safe,
            stmts: Vec::new(),
            at_item,
            containing_item,
            next_infer: 0,
            dbgnames,
            localitems,
        };

        for (id, param) in fnty.paramtys.into_iter().enumerate() {
            let id = HirVarId(id as u32);
            result.var_defs.insert(
                id,
                ThirVarDef {
                    mt: param.copy_span(|_| Mutability::Const),
                    ty: param,
                    debug_name: result.dbgnames.get(&id).copied(),
                },
            );
        }

        result
    }

    pub fn convert_rvalue(
        &mut self,
        expr: &Spanned<hir::HirExpr>,
    ) -> super::Result<Spanned<ThirExpr>> {
        let mut ret = self.convert_expr(expr)?;

        if ret.cat != ValueCategory::Rvalue {
            let ty = ret.ty.clone();
            let inner = ThirExprInner::Read(Box::new(ret));

            ret = Spanned {
                span: expr.span,
                body: ThirExpr {
                    ty,
                    inner,
                    cat: ValueCategory::Rvalue,
                },
            };
        }

        Ok(ret)
    }

    pub fn convert_lvalue(
        &mut self,
        expr: &Spanned<hir::HirExpr>,
    ) -> super::Result<Spanned<ThirExpr>> {
        let mut ret = self.convert_expr(expr)?;

        if ret.cat == ValueCategory::Rvalue {
            todo!("Temporary");
        }

        Ok(ret)
    }

    pub fn convert_syntatic_type(&mut self, ty: Spanned<Type>) -> super::Result<Spanned<Type>> {
        ty.try_map_span(|ty| match ty {
            Type::Inferable(None) => Ok(Type::Inferable(Some(InferId(
                self.next_infer.fetch_increment(),
            )))),

            Type::IncompleteAlias(_) => todo!("incomplete alias"),

            Type::Tuple(tys) => Ok(Type::Tuple(
                tys.into_iter()
                    .map(|ty| self.convert_syntatic_type(ty))
                    .collect::<super::Result<_>>()?,
            )),
            Type::FnPtr(mut fnty) => {
                fnty.paramtys = fnty
                    .paramtys
                    .into_iter()
                    .map(|ty| self.convert_syntatic_type(ty))
                    .collect::<super::Result<_>>()?;
                *fnty.retty = self.convert_syntatic_type(*fnty.retty)?;

                Ok(Type::FnPtr(fnty))
            }

            Type::Pointer(mt, mut ty) => {
                *ty = self.convert_syntatic_type(*ty)?;

                Ok(Type::Pointer(mt, ty))
            }
            Type::Array(mut ty, cx) => {
                *ty = self.convert_syntatic_type(*ty)?;

                Ok(Type::Array(ty, cx))
            }
            Type::Reference(life, mt, mut ty) => {
                *ty = self.convert_syntatic_type(*ty)?;

                Ok(Type::Reference(life, mt, ty))
            }
            val @ (Type::Inferable(Some(_))
            | Type::InferableInt(_)
            | Type::Param(_)
            | Type::TraitSelf(_)
            | Type::DropFlags(_)
            | Type::Bool
            | Type::Int(_)
            | Type::Float(_)
            | Type::Char
            | Type::Str
            | Type::FnItem(_, _, _)
            | Type::UserType(_, _)
            | Type::Never) => Ok(val),
            Type::UnresolvedLangItem(lang, args) => {
                Ok(Type::UserType(self.defs.require_lang_item(lang)?, args))
            }
        })
    }

    pub fn convert_expr(
        &mut self,
        expr: &Spanned<hir::HirExpr>,
    ) -> super::Result<Spanned<ThirExpr>> {
        expr.try_copy_span(|thirexpr| match thirexpr {
            hir::HirExpr::Var(varid) => {
                let inner = ThirExprInner::Var(*varid);
                let def = self.var_defs.get(varid).ok_or_else(|| super::Error {
                    span: expr.span,
                    text: format!("Could not find variable {}", varid),
                    category: super::ErrorCategory::CannotFindName,
                    at_item: self.at_item,
                    containing_item: self.containing_item,
                    relevant_item: self.at_item,
                    hints: vec![],
                })?;
                let cat = ValueCategory::Lvalue(Movability::Movable, def.mt.body);
                let ty = def.ty.body.clone();

                Ok(ThirExpr { inner, cat, ty })
            }
            hir::HirExpr::ConstInt(ity, val) => {
                let ity = *ity;
                let val = *val;
                let inner = ThirExprInner::ConstInt(ity, val);

                let ty = ity.map(|ity| ity.body).map(Type::Int).unwrap_or_else(|| {
                    let id = InferId(self.next_infer.fetch_increment());
                    Type::InferableInt(id)
                });

                Ok(ThirExpr {
                    inner,
                    ty,
                    cat: ValueCategory::Rvalue,
                })
            }
            hir::HirExpr::ConstString(sty, val) => {
                let tyspan = Span::synthetic();
                let innerty = match sty {
                    StringType::Default | StringType::Raw(_) => Type::Str,
                    StringType::Byte | StringType::RawByte(_) => {
                        let len = ConstExpr::IntConst(IntType::usize, val.len() as u128);

                        let bty = Type::Int(IntType::u8);

                        Type::Array(
                            Box::new(Spanned {
                                span: tyspan,
                                body: bty,
                            }),
                            Spanned {
                                body: len,
                                span: tyspan,
                            },
                        )
                    }
                };

                let ty = Type::Reference(
                    Some(Box::new(Spanned {
                        body: SemaLifetime::Static,
                        span: tyspan,
                    })),
                    Spanned {
                        body: Mutability::Const,
                        span: tyspan,
                    },
                    Box::new(Spanned {
                        body: innerty,
                        span: tyspan,
                    }),
                );

                let inner = ThirExprInner::ConstString(*sty, *val);

                Ok(ThirExpr {
                    ty,
                    inner,
                    cat: ValueCategory::Rvalue,
                })
            }
            hir::HirExpr::ConstChar(cty, val) => {
                let ty = match cty {
                    CharType::Byte => Type::Int(IntType::u8),
                    CharType::Default => Type::Char,
                };

                let inner = ThirExprInner::ConstChar(*cty, *val);

                Ok(ThirExpr {
                    ty,
                    inner,
                    cat: ValueCategory::Rvalue,
                })
            }
            hir::HirExpr::Const(defid, generics) => {
                let def = self.defs.definition(*defid);

                let (ty, cat) = match &def.inner.body {
                    super::DefinitionInner::Function(fnty, _) => (
                        Type::FnItem(Box::new(fnty.clone()), *defid, generics.clone()),
                        ValueCategory::Rvalue,
                    ),
                    _ => unreachable!("Not a value"),
                };

                let inner = ThirExprInner::Const(*defid, generics.clone());

                Ok(ThirExpr { ty, inner, cat })
            }
            hir::HirExpr::Unreachable => Ok(ThirExpr {
                ty: Type::Never,
                cat: ValueCategory::Rvalue,
                inner: ThirExprInner::Unreachable,
            }),
            hir::HirExpr::Cast(expr, ty) => {
                let ty = self.convert_syntatic_type(ty.clone())?;
                Ok(ThirExpr {
                    ty: ty.body.clone(),
                    cat: ValueCategory::Rvalue,
                    inner: ThirExprInner::Cast(Box::new(self.convert_rvalue(expr)?), ty),
                })
            }
            hir::HirExpr::Tuple(vals) => {
                let vals = vals
                    .iter()
                    .map(|expr| self.convert_rvalue(expr))
                    .collect::<super::Result<Vec<_>>>()?;

                let ty = vals
                    .iter()
                    .map(|expr| expr.copy_span(|expr| expr.ty.clone()))
                    .collect::<Vec<_>>();

                let ty = Type::Tuple(ty);

                let inner = ThirExprInner::Tuple(vals);

                Ok(ThirExpr {
                    ty,
                    cat: ValueCategory::Rvalue,
                    inner,
                })
            }
            hir::HirExpr::Constructor(ctor) => {
                let defid = ctor.constructor_def;

                let ty = self.defs.type_of_constructor(defid.body);

                let ctor_ty = Type::UserType(ty, GenericArgs::default());

                let fields = self.defs.fields_of(&ctor_ty);

                let mut last_init = HashMap::<_, _>::new();

                let ctor_span = ctor.span;
                let ctor = ctor.try_copy_span(|ctor| {
                    let mut thir_ctor = ThirConstructor {
                        base: defid.body,
                        fields: Vec::new(),
                        rest_init: None,
                    };

                    for (name, expr) in &ctor.fields {
                        let expr = self.convert_rvalue(expr)?;
                        let name = name.copy_span(|&sym| FieldName::Field(sym));

                        if let Some(span) = last_init.insert(name.body.clone(), name.span) {
                            return Err(Error {
                                span: name.span,
                                text: format!("field {} already initialized", name.body),
                                category: ErrorCategory::TycheckError(
                                    TycheckErrorCategory::DuplicateField,
                                ),
                                at_item: self.at_item,
                                containing_item: self.containing_item,
                                relevant_item: defid.body,
                                hints: vec![
                                    SemaHint {
                                        text: format!(
                                            "field {} previously initialized here",
                                            name.body
                                        ),
                                        itemref: self.at_item,
                                        refspan: span,
                                    },
                                    SemaHint {
                                        text: format!("at this constructor expression"),
                                        itemref: defid.body,
                                        refspan: ctor_span,
                                    },
                                ],
                            });
                        }

                        thir_ctor.fields.push((name, expr));
                    }

                    if let Some(rest_init) = &ctor.rest_init {
                        let rest_init = Box::new(self.convert_rvalue(rest_init)?);
                        thir_ctor.rest_init = Some(rest_init);
                    }

                    Ok(thir_ctor)
                })?;
                let inner = ThirExprInner::Ctor(ctor);

                Ok(ThirExpr {
                    ty: ctor_ty,
                    inner,
                    cat: ValueCategory::Rvalue,
                })
            }
            hir::HirExpr::FieldAccess(inner, name) => {
                let val = self.convert_lvalue(inner)?;

                let ty = &val.ty;

                let cat = val.cat;

                let fields = self.defs.fields_of(ty);

                let ty = fields
                    .iter()
                    .filter(|f| &f.name == &name.body)
                    .map(|f| f.ty.clone())
                    .next()
                    .unwrap_or_else(|| {
                        let inferid = InferId(self.next_infer.fetch_increment());
                        Type::Inferable(Some(inferid))
                    });

                let inner = ThirExprInner::MemberAccess(Box::new(val), name.clone());

                Ok(ThirExpr { ty, cat, inner })
            }
            hir::HirExpr::UnaryExpr(op, val) => {
                let val = self.convert_rvalue(val)?;
                let ty = match &val.ty {
                    Type::Int(x) => Type::Int(*x),
                    Type::Float(x) => Type::Float(*x),
                    Type::InferableInt(x) => Type::InferableInt(*x),
                    _ => Type::Inferable(Some(InferId(self.next_infer.fetch_increment()))),
                };
                Ok(ThirExpr {
                    ty,
                    cat: ValueCategory::Rvalue,
                    inner: ThirExprInner::UnaryExpr(*op, Box::new(val)),
                })
            }
            hir::HirExpr::BinaryExpr(op, lhs, rhs) => {
                let lhs = self.convert_rvalue(lhs)?;
                let rhs = self.convert_rvalue(rhs)?;
                let ty = match (&lhs.ty, &rhs.ty) {
                    (Type::Int(x), y) if Type::Int(*x) == *y => Type::Int(*x),
                    (Type::Float(x), y) if Type::Float(*x) == *y => Type::Float(*x),
                    (Type::InferableInt(x), Type::InferableInt(_)) => Type::InferableInt(*x),
                    _ => Type::Inferable(Some(InferId(self.next_infer.fetch_increment()))),
                };
                Ok(ThirExpr {
                    ty,
                    cat: ValueCategory::Rvalue,
                    inner: ThirExprInner::BinaryExpr(*op, Box::new(lhs), Box::new(rhs)),
                })
            }
            hir::HirExpr::Array(elements) => {
                let elements = elements
                    .iter()
                    .map(|x| self.convert_rvalue(x))
                    .collect::<Result<Vec<_>>>()?;
                if elements.len() == 0 {
                    Ok(ThirExpr {
                        ty: Type::Inferable(Some(InferId(self.next_infer.fetch_increment()))),
                        cat: ValueCategory::Rvalue,
                        inner: ThirExprInner::Array(elements),
                    })
                } else {
                    Ok(ThirExpr {
                        ty: elements[0].ty.clone(),
                        cat: ValueCategory::Rvalue,
                        inner: ThirExprInner::Array(elements),
                    })
                }
            }
            hir::HirExpr::Index(base, index) => {
                let base = self.convert_rvalue(base)?;
                Ok(ThirExpr {
                    ty: match &base.ty {
                        Type::Array(ty, _) => ty.body.clone(),
                        _ => Type::Inferable(Some(InferId(self.next_infer.fetch_increment()))),
                    },
                    cat: ValueCategory::Rvalue,
                    inner: ThirExprInner::Index(
                        Box::new(base),
                        Box::new(self.convert_rvalue(index)?),
                    ),
                })
            }
        })
    }

    fn convert_matcher(
        &mut self,
        matcher: &Spanned<hir::HirPatternMatcher>,
        path: Vec<PatternPathFragment>,
        path_ty_map: &mut HashMap<Vec<PatternPathFragment>, Type>,
    ) -> super::Result<Spanned<ThirPatternMatcher>> {
        let matcher = match &matcher.body {
            hir::HirPatternMatcher::Hole => {
                let infer = InferId(self.next_infer.fetch_increment());
                let ty = Type::Inferable(Some(infer));

                Spanned {
                    body: ThirPatternMatcher {
                        inner: ThirMatcherInner::Hole,
                        ty,
                    },
                    span: matcher.span,
                }
            }
            hir::HirPatternMatcher::ConstInt(val, None) => {
                let infer = InferId(self.next_infer.fetch_increment());
                let ty = Type::InferableInt(infer);

                Spanned {
                    body: ThirPatternMatcher {
                        inner: ThirMatcherInner::ConstInt(*val, None),
                        ty,
                    },
                    span: matcher.span,
                }
            }
            hir::HirPatternMatcher::ConstInt(val, Some(intty)) => {
                let ty = Type::Int(**intty);

                Spanned {
                    body: ThirPatternMatcher {
                        inner: ThirMatcherInner::ConstInt(*val, Some(*intty)),
                        ty,
                    },
                    span: matcher.span,
                }
            }
            hir::HirPatternMatcher::Const(defid) => todo!("{}", defid),
        };
        path_ty_map.insert(path, matcher.ty.clone());

        Ok(matcher)
    }

    pub fn convert_pattern(
        &mut self,
        pat: &Spanned<hir::HirPattern>,
    ) -> super::Result<Spanned<ThirPattern>> {
        let mut path_ty_map = HashMap::new();
        let matcher = self.convert_matcher(&pat.matcher, vec![], &mut path_ty_map)?;

        let bindings = pat.bindings.clone();

        for binding in &bindings {
            let var = binding.var;

            let ty = Spanned {
                body: path_ty_map[&binding.path.elements].clone(),
                span: var.span,
            };

            if let Some(mode) = binding.mode {
                todo!("binding mode {}", mode.body)
            }

            let mt = binding.mutability;

            let def = ThirVarDef {
                mt,
                ty,
                debug_name: self.dbgnames.get(&*var).copied(),
            };

            self.var_defs.insert(*var, def);
        }

        Ok(pat.copy_span(|_| ThirPattern { matcher, bindings }))
    }

    pub fn write_statement(&mut self, stmt: &Spanned<hir::HirStatement>) -> super::Result<()> {
        let stmt = self.convert_statement(stmt)?;

        self.stmts.push(stmt);

        Ok(())
    }

    pub fn convert_block(
        &mut self,
        blk: &Spanned<hir::HirBlock>,
    ) -> super::Result<Spanned<ThirBlock>> {
        blk.try_copy_span(|blk| match blk {
            hir::HirBlock::Normal(stmts) => stmts
                .iter()
                .map(|stmt| self.convert_statement(stmt))
                .collect::<super::Result<Vec<_>>>()
                .map(ThirBlock::Normal),
            hir::HirBlock::Unsafe(stmts) => stmts
                .iter()
                .map(|stmt| self.convert_statement(stmt))
                .collect::<super::Result<Vec<_>>>()
                .map(ThirBlock::Unsafe),
            hir::HirBlock::Loop(stmts) => stmts
                .iter()
                .map(|stmt| self.convert_statement(stmt))
                .collect::<super::Result<Vec<_>>>()
                .map(ThirBlock::Loop),
            hir::HirBlock::If {
                cond,
                block,
                elseblock,
            } => {
                let cond = self.convert_expr(cond)?;
                let block = self.convert_block(block)?;
                let elseblock = elseblock
                    .as_ref()
                    .map(|block| self.convert_block(block))
                    .transpose()?;

                Ok(ThirBlock::If {
                    cond,
                    block: Box::new(block),
                    elseblock: elseblock.map(Box::new),
                })
            }
            hir::HirBlock::Match(m) => {
                let discriminee = self.convert_expr(&m.discriminee)?;

                let arms = m
                    .arms
                    .iter()
                    .map(|a| {
                        let pattern = self.convert_pattern(&a.pattern)?;
                        let expansion = a.expansion.try_copy_span(|s| {
                            s.iter()
                                .map(|stmt| self.convert_statement(stmt))
                                .collect::<super::Result<Vec<_>>>()
                        })?;
                        let guard = a
                            .guard
                            .as_ref()
                            .map(|expr| self.convert_expr(expr))
                            .transpose()?;

                        Ok(ThirMatchArm {
                            discrim: pattern,
                            guard,
                            expansion,
                        })
                    })
                    .collect::<super::Result<Vec<_>>>()?;

                let m = ThirMatch { discriminee, arms };

                Ok(ThirBlock::Match(m))
            }
        })
    }

    pub fn convert_statement(
        &mut self,
        stmt: &Spanned<hir::HirStatement>,
    ) -> super::Result<Spanned<ThirStatement>> {
        stmt.try_copy_span(|hirstmt| match hirstmt {
            hir::HirStatement::Assign { dest, val, op } => {
                let dest = self.convert_expr(dest)?;
                let mut val = self.convert_rvalue(val)?;

                let &op = op;

                Ok(ThirStatement::Assign { dest, val, op })
            }
            hir::HirStatement::Define {
                mutability,
                var,
                ty,
            } => {
                let ty = ty
                    .as_ref()
                    .cloned()
                    .map(|ty| self.convert_syntatic_type(ty))
                    .unwrap_or_else(|| {
                        let infer = InferId(self.next_infer.fetch_increment());

                        Ok(Spanned {
                            span: var.span,
                            body: Type::Inferable(Some(infer)),
                        })
                    })?;

                let mutability = *mutability;

                let var = *var;

                self.var_defs.insert(
                    var.body,
                    ThirVarDef {
                        mt: mutability,
                        ty: ty.clone(),
                        debug_name: self.dbgnames.get(&var.body).copied(),
                    },
                );

                Ok(ThirStatement::Define {
                    mutability,
                    var,
                    ty,
                })
            }
            hir::HirStatement::Return(expr) => {
                let expr = self.convert_rvalue(expr)?;

                Ok(ThirStatement::Return(expr))
            }
            hir::HirStatement::Block(b) => Ok(ThirStatement::Block(self.convert_block(b)?)),
            hir::HirStatement::Discard(expr) => {
                let val = self.convert_rvalue(expr)?;

                Ok(ThirStatement::Discard(val))
            }
            hir::HirStatement::Call {
                retplace,
                fnexpr,
                method_name,
                params,
            } => {
                let retplace = retplace
                    .as_ref()
                    .map(|retplace| self.convert_expr(retplace))
                    .transpose()?;

                let fnexpr = self.convert_rvalue(fnexpr)?;

                let method_name = *method_name;

                let params = params
                    .iter()
                    .map(|expr| self.convert_rvalue(expr))
                    .collect::<super::Result<_>>()?;

                Ok(ThirStatement::Call {
                    retplace,
                    fnexpr,
                    method_name,
                    params,
                })
            }
        })
    }

    pub fn into_thir_body(self, span: Span) -> ThirFunctionBody {
        let body = match self.safety {
            Safety::Safe => ThirBlock::Normal(self.stmts),
            Safety::Unsafe => ThirBlock::Unsafe(self.stmts),
        };
        ThirFunctionBody {
            vardefs: self.var_defs,
            body: Spanned { body, span },
            localitems: self.localitems,
        }
    }
}

pub struct Inferer<'a> {
    defs: &'a Definitions,
    curdef: DefId,
    curmod: DefId,
    vardefs: &'a mut HashMap<HirVarId, ThirVarDef>,
    inference_set: HashMap<InferId, Type>,
}

impl<'a> Inferer<'a> {
    pub fn new(
        defs: &'a Definitions,
        curdef: DefId,
        curmod: DefId,
        vardefs: &'a mut HashMap<HirVarId, ThirVarDef>,
    ) -> Self {
        Self {
            defs,
            curdef,
            curmod,
            vardefs,
            inference_set: HashMap::new(),
        }
    }

    pub fn unify_types(
        &mut self,
        left: &mut Type,
        right: &mut Type,
    ) -> super::Result<CyclicOperationStatus> {
        match (&mut *left, &mut *right) {
            (a, b) if a == b => Ok(CyclicOperationStatus::Complete),
            (Type::Inferable(Some(l)), Type::Inferable(Some(r))) => {
                let l = *l;
                let r = *r;

                // Note: We do RTL Propagation here. The left type is typically the assignee and the right type is typically the assigned
                // This *should* narrow the inference set quicker in general
                *left = Type::Inferable(Some(r));

                if let Some(ty) = self.inference_set.get_mut(&l) {
                    let mut ty = core::mem::replace(ty, Type::Never);
                    if let Some(inner_ty) = self.inference_set.get_mut(&r) {
                        let mut inner_ty = core::mem::replace(inner_ty, Type::Never);

                        self.unify_types(&mut ty, &mut inner_ty)?;

                        self.inference_set.insert(l, ty);
                        self.inference_set.insert(r, inner_ty);
                    } else {
                        self.inference_set.insert(r, ty);
                    }
                } else {
                    self.inference_set.insert(l, Type::Inferable(Some(r)));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::InferableInt(l), Type::Inferable(Some(r))) => {
                let l = *l;
                let r = *r;
                //Note: We're doing LTR propagation here because the left type is more specific.
                *right = Type::InferableInt(l);

                if let Some(ty) = self.inference_set.get_mut(&r) {
                    let mut ty = core::mem::replace(ty, Type::Never);
                    if let Some(inner_ty) = self.inference_set.get_mut(&l) {
                        let mut inner_ty = core::mem::replace(inner_ty, Type::Never);

                        self.unify_types(&mut ty, &mut inner_ty)?;

                        self.inference_set.insert(l, ty);
                        self.inference_set.insert(r, inner_ty);
                    } else {
                        self.inference_set.insert(r, ty);
                    }
                } else {
                    self.inference_set.insert(l, Type::Inferable(Some(r)));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::Inferable(Some(l)), Type::InferableInt(r)) => {
                let l = *l;
                let r = *r;

                //Note: We do RTL Propagation here because the right type is more specific.
                *left = Type::Inferable(Some(r));

                if let Some(ty) = self.inference_set.get_mut(&l) {
                    let mut ty = core::mem::replace(ty, Type::Never);
                    if let Some(inner_ty) = self.inference_set.get_mut(&r) {
                        let mut inner_ty = core::mem::replace(inner_ty, Type::Never);

                        self.unify_types(&mut ty, &mut inner_ty)?;

                        self.inference_set.insert(l, ty);
                        self.inference_set.insert(r, inner_ty);
                    } else {
                        let ty = ty.clone();
                        self.inference_set.insert(r, ty);
                    }
                } else {
                    self.inference_set.insert(l, Type::Inferable(Some(r)));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::Inferable(Some(l)), ty) => {
                let l = *l;

                if let Some(gty) = self.inference_set.get_mut(&l) {
                    let mut gty = core::mem::replace(gty, Type::Never);

                    self.unify_types(&mut gty, ty)?;
                    self.inference_set.insert(l, gty);
                } else {
                    self.inference_set.insert(l, ty.clone());
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::InferableInt(l), ty @ Type::Int(_)) => {
                let l = *l;

                if let Some(gty) = self.inference_set.get_mut(&l) {
                    let mut gty = core::mem::replace(gty, Type::Never);

                    self.unify_types(&mut gty, ty)?;
                    self.inference_set.insert(l, gty);
                } else {
                    self.inference_set.insert(l, ty.clone());
                }
                Ok(CyclicOperationStatus::Incomplete)
            }
            (ty @ Type::Int(_), Type::InferableInt(r)) => {
                let r = *r;
                if let Some(gty) = self.inference_set.get_mut(&r) {
                    let mut gty = core::mem::replace(gty, Type::Never);

                    self.unify_types(&mut gty, ty)?;
                    self.inference_set.insert(r, gty);
                } else {
                    self.inference_set.insert(r, ty.clone());
                }
                Ok(CyclicOperationStatus::Incomplete)
            }
            (ty, Type::Inferable(Some(r))) => {
                let r = *r;

                if let Some(gty) = self.inference_set.get_mut(&r) {
                    let mut gty = core::mem::replace(gty, Type::Never);

                    self.unify_types(&mut gty, ty)?;
                    self.inference_set.insert(r, gty);
                } else {
                    self.inference_set.insert(r, ty.clone());
                }
                Ok(CyclicOperationStatus::Incomplete)
            }
            _ => Ok(CyclicOperationStatus::Complete),
        }
    }

    pub fn unify_typed_expr(
        &mut self,
        left: &mut ThirExpr,
        ty: &mut Type,
    ) -> super::Result<CyclicOperationStatus> {
        let status = self.unify_types(&mut left.ty, ty)?;

        return Ok(status & self.unify_single_expr(left)?);
    }

    pub fn unify_single_expr(
        &mut self,
        left: &mut ThirExpr,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = CyclicOperationStatus::Complete;
        match &mut left.inner {
            ThirExprInner::Read(inner) => {
                status &= self.unify_single_expr(inner)?;
                status &= self.unify_types(&mut inner.ty, &mut left.ty)?;
            }
            ThirExprInner::Unreachable
            | ThirExprInner::Var(_)
            | ThirExprInner::Const(_, _)
            | ThirExprInner::ConstInt(_, _)
            | ThirExprInner::ConstString(_, _)
            | ThirExprInner::ConstChar(_, _) => {}
            ThirExprInner::Cast(inner, ty) => {
                status &= self.unify_single_expr(inner)?;

                status &= self.unify_types(ty, &mut left.ty)?;
            }
            ThirExprInner::Tuple(exprs) => match &mut left.ty {
                Type::Tuple(tys) => {
                    if exprs.len() == tys.len() {
                        status = exprs
                            .iter_mut()
                            .map(|expr| &mut expr.ty)
                            .zip(tys)
                            .map(|(left, right)| self.unify_types(left, right))
                            .try_fold(status, |status, val| Ok(status & val?))?;
                    }
                }
                _ => {}
            },
            ThirExprInner::Ctor(ctor) => {
                let base = ctor.base;
                let mut ty =
                    Type::UserType(self.defs.type_of_constructor(base), GenericArgs::default());
                if let Some(rest_init) = ctor.rest_init.as_mut() {
                    status &= self.unify_typed_expr(rest_init, &mut ty)?;
                }

                let fields = self.defs.fields_of(&ty);

                let mut field_tys = fields
                    .iter()
                    .map(|field| (field.name.clone(), field.ty.clone()))
                    .collect::<HashMap<_, _>>();

                for (field, val) in &mut ctor.fields {
                    status &= self.unify_types(
                        field_tys.get_mut(&field.body).ok_or_else(|| Error {
                            span: field.span,
                            text: format!("No such field {}", field.body),
                            category: ErrorCategory::TycheckError(
                                TycheckErrorCategory::NoSuchField,
                            ),
                            at_item: self.curdef,
                            containing_item: self.curmod,
                            relevant_item: base,
                            hints: vec![],
                        })?,
                        &mut ty,
                    )?;
                }
            }
            ThirExprInner::MemberAccess(expr, field) => {
                status &= self.unify_single_expr(expr)?;
            }
            ThirExprInner::BinaryExpr(_, lhs, rhs) => {
                status &= self.unify_single_expr(lhs)?;
                status &= self.unify_single_expr(rhs)?;
                println!("{}, {}", left.ty, lhs.ty);
                status &= self.unify_types(&mut left.ty, &mut lhs.ty)?;
                println!("{}, {}", left.ty, lhs.ty);
                status &= self.unify_types(&mut lhs.ty, &mut rhs.ty)?;
                println!("{}, {}", left.ty, lhs.ty);
            }
            ThirExprInner::Array(_elements) => {
                todo!("array")
            }
            ThirExprInner::Index(base, index) => {
                todo!("index")
            }
            ThirExprInner::UnaryExpr(op, val) => {
                status &= self.unify_types(&mut left.ty, &mut val.ty)?;
                status &= self.unify_single_expr(val)?;
            }
        }

        Ok(status)
    }

    pub fn unify_exprs(
        &mut self,
        left: &mut ThirExpr,
        right: &mut ThirExpr,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = self.unify_types(&mut left.ty, &mut right.ty)?;

        status &= self.unify_single_expr(left)?;
        status &= self.unify_single_expr(right)?;

        Ok(status)
    }

    pub fn unify_single_matcher(
        &mut self,
        left: &mut ThirPatternMatcher,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = CyclicOperationStatus::Complete;

        match &mut left.inner {
            ThirMatcherInner::Hole | ThirMatcherInner::ConstInt(_, None) => {}
            ThirMatcherInner::ConstInt(_, Some(intty)) => {
                let mut ty = Type::Int(**intty);

                status &= self.unify_types(&mut left.ty, &mut ty)?;
            }
            ThirMatcherInner::Const(_) => todo!(),
        }

        Ok(status)
    }

    pub fn propagate_matcher(
        &mut self,
        left: &mut ThirPatternMatcher,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = self.propagate_type(&mut left.ty)?;

        Ok(status)
    }

    pub fn unify_discriminated_expr(
        &mut self,
        left: &mut ThirExpr,
        right: &mut ThirPattern,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = self.unify_types(&mut left.ty, &mut right.matcher.ty)?;

        status &= self.unify_single_expr(left)?;
        status &= self.unify_single_matcher(&mut right.matcher)?;

        Ok(status)
    }

    pub fn unify_statement(
        &mut self,
        stmt: &mut ThirStatement,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match stmt {
            ThirStatement::Assign { ref mut dest, ref mut val, op } => {
                status &= self.unify_exprs(dest, val)?;
            }
            ThirStatement::Block(blk) => status &= self.unify_block(&mut blk.body)?,
            ThirStatement::Call {
                retplace,
                fnexpr,
                method_name,
                params,
            } => {
                status &= self.unify_single_expr(fnexpr)?;

                if method_name.is_some() {
                    todo!("method")
                }

                let span = fnexpr.span;

                let fnty = match &mut fnexpr.ty {
                    Type::FnPtr(ty) => ty,
                    Type::FnItem(ty, _,_) => ty,
                    Type::Inferable(_) => return Ok(status),
                    ty => {
                        return Err(Error {
                            span,
                            text: format!("Cannot use primitive function call on {}", ty),
                            category: ErrorCategory::TycheckError(
                                TycheckErrorCategory::InferenceFailure,
                            ),
                            at_item: self.curdef,
                            containing_item: self.curmod,
                            relevant_item: self.curmod,
                            hints: vec![],
                        })
                    }
                };

                status &= fnty
                    .paramtys
                    .iter_mut()
                    .zip(params)
                    .try_fold(status, |status, (ty, param)| {
                        Ok(status & self.unify_typed_expr(param, ty)?)
                    })?;

                if let Some(retplace) = retplace {
                    status &= self.unify_typed_expr(retplace, &mut fnty.retty)?;
                }
            }
            ThirStatement::Define { .. } => {
                // status &= if ty.body.is_inference() {
                //     Incomplete
                // } else {
                //     Complete
                // }
                // We didn't actually unify an inference var here, so don't say so -
            }
            ThirStatement::Return(expr) => match &self.defs.definition(self.curdef).inner.body {
                DefinitionInner::Function(fnty, _) => {
                    let mut retty = (*fnty.retty).clone();

                    status &= self.unify_typed_expr(expr, &mut retty)?;
                }
                _ => unreachable!("One would hope we'd be typechecking the inside of a function body (or a static/const)")
            },
            ThirStatement::Discard(expr) => {
                status &= self.unify_single_expr(expr)?;
            }
            x => todo!("{:?}", x),
        }
        Ok(status)
    }

    pub fn unify_block(&mut self, blk: &mut ThirBlock) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match blk {
            ThirBlock::Normal(stmts) | ThirBlock::Unsafe(stmts) | ThirBlock::Loop(stmts) => {
                for stmt in stmts {
                    status &= self.unify_statement(stmt)?;
                }
            }
            ThirBlock::If {
                cond,
                block,
                elseblock,
            } => {
                status &= self.unify_typed_expr(cond, &mut Type::Bool)?;
                status &= self.unify_block(block)?;

                if let Some(elseblock) = elseblock {
                    status &= self.unify_block(elseblock)?;
                }
            }
            ThirBlock::Match(m) => {
                for arm in &mut m.arms {
                    status &=
                        self.unify_discriminated_expr(&mut m.discriminee, &mut arm.discrim)?;

                    for stmt in &mut arm.expansion.body {
                        status &= self.unify_statement(stmt)?;
                    }
                }
            }
        }
        Ok(status)
    }

    pub fn propagate_block(&mut self, blk: &mut ThirBlock) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match blk {
            ThirBlock::Normal(stmts) | ThirBlock::Unsafe(stmts) | ThirBlock::Loop(stmts) => {
                for stmt in stmts {
                    status &= self.propagate_statement(stmt)?;
                }
            }
            ThirBlock::If {
                cond,
                block,
                elseblock,
            } => {
                status &= self.propagate_expr(cond)?;
                status &= self.propagate_block(block)?;
                if let Some(elseblock) = elseblock {
                    status &= self.propagate_block(elseblock)?;
                }
            }
            ThirBlock::Match(m) => {
                status &= self.propagate_expr(&mut m.discriminee)?;
                for arm in &mut m.arms {
                    status &= self.propagate_matcher(&mut arm.discrim.matcher)?;
                    for stmt in &mut arm.expansion.body {
                        status &= self.propagate_statement(stmt)?;
                    }
                }
            }
        }
        Ok(status)
    }

    pub fn propagate_statement(
        &mut self,
        stmt: &mut ThirStatement,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match stmt {
            ThirStatement::Assign { dest, val, op } => {
                status &= self.propagate_expr(dest)?;
                status &= self.propagate_expr(val)?;
            }
            ThirStatement::Define {
                mutability,
                var,
                ty,
            } => {
                status &= self.propagate_type(ty)?;
                let mut ty = std::mem::replace(&mut *self.vardefs[var].ty, Type::Never);
                status &= self.propagate_type(&mut ty)?;
                *self.vardefs[var].ty = ty;
            }
            ThirStatement::Return(ret) => status &= self.propagate_expr(ret)?,
            ThirStatement::Block(blk) => status &= self.propagate_block(blk)?,
            ThirStatement::Discard(expr) => status &= self.propagate_expr(expr)?,
            ThirStatement::Call {
                retplace,
                fnexpr,
                method_name,
                params,
            } => {
                if let Some(retplace) = retplace {
                    status &= self.propagate_expr(retplace)?;
                }

                status &= self.propagate_expr(fnexpr)?;

                for param in params {
                    status &= self.propagate_expr(param)?;
                }
            }
        }
        Ok(status)
    }

    pub fn propagate_expr(&mut self, expr: &mut ThirExpr) -> super::Result<CyclicOperationStatus> {
        let mut status = self.propagate_type(&mut expr.ty)?;

        match &mut expr.inner {
            ThirExprInner::Read(inner) => status &= self.propagate_expr(inner)?,
            ThirExprInner::Cast(inner, ty) => {
                status &= self.propagate_type(ty)?;
                status &= self.propagate_expr(inner)?;
            }
            ThirExprInner::Tuple(exprs) => {
                for expr in exprs {
                    status &= self.propagate_expr(expr)?;
                }
            }
            ThirExprInner::Ctor(constructor) => {
                for (_, field) in &mut constructor.fields {
                    status &= self.propagate_expr(field)?;
                }

                if let Some(rest_init) = &mut constructor.rest_init {
                    status &= self.propagate_expr(rest_init)?;
                }
            }
            ThirExprInner::MemberAccess(base, _) => status &= self.propagate_expr(base)?,
            ThirExprInner::BinaryExpr(_, lhs, rhs) => {
                status &= self.propagate_expr(lhs)?;
                status &= self.propagate_expr(rhs)?;
            }
            ThirExprInner::UnaryExpr(_, expr) => {
                status &= self.propagate_expr(expr)?;
            }
            _ => {}
        }
        Ok(status)
    }

    pub fn propagate_type(&mut self, ty: &mut Type) -> super::Result<CyclicOperationStatus> {
        match ty {
            Type::Tuple(tys) => {
                let mut status = Complete;
                for ty in tys {
                    status &= self.propagate_type(ty)?;
                }
                Ok(status)
            }
            Type::FnPtr(fnty) => {
                let mut status = self.propagate_type(&mut fnty.retty)?;

                for ty in &mut fnty.paramtys {
                    status &= self.propagate_type(ty)?;
                }

                Ok(status)
            }
            Type::Pointer(_, pty) => self.propagate_type(pty),
            Type::Array(inner, _) => self.propagate_type(inner),
            Type::Inferable(Some(infer)) | Type::InferableInt(infer) => {
                if let Some(subty) = self.inference_set.get(infer) {
                    *ty = subty.clone();
                    Ok(Incomplete)
                } else {
                    Ok(Complete)
                }
            }
            Type::Reference(_, _, inner) => self.propagate_type(inner),
            _ => Ok(Complete),
        }
    }
}
