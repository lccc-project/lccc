use xlang::abi::collection::HashMap;

use crate::{
    ast::{self, Mutability, Safety, StringType},
    helpers::{CyclicOperationStatus, FetchIncrement, TabPrinter},
    interning::Symbol,
    span::Span,
};

pub use super::ty::Type;

use super::{
    cx::ConstExpr,
    hir::HirVarId,
    ty::{IntType, SemaLifetime},
    DefId, DefinitionInner, Definitions, Error, ErrorCategory, Result, Spanned,
};

use CyclicOperationStatus::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum TycheckErrorCategory {
    InferenceFailure,
    NoConversion,
    FailedToUnify,
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
    Const(DefId),
    ConstInt(Option<Spanned<IntType>>, u128),
    ConstString(StringType, Spanned<Symbol>),
    Cast(Box<Spanned<ThirExpr>>, Spanned<Type>),
    Tuple(Vec<Spanned<ThirExpr>>),
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
            ThirExprInner::Const(def) => def.fmt(f),
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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ThirBlock {
    Normal(Vec<Spanned<ThirStatement>>),
    Unsafe(Vec<Spanned<ThirStatement>>),
    Loop(Vec<Spanned<ThirStatement>>),
}

impl ThirBlock {
    fn inner_mut(&mut self) -> &mut Vec<Spanned<ThirStatement>> {
        match self {
            Self::Normal(x) => x,
            Self::Unsafe(x) => x,
            Self::Loop(x) => x,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ThirFunctionBody {
    pub vardefs: HashMap<HirVarId, ThirVarDef>,
    pub body: Spanned<ThirBlock>,
}

impl ThirFunctionBody {
    pub fn display_body(
        &self,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        let stats = match &self.body.body {
            ThirBlock::Normal(stats) | ThirBlock::Unsafe(stats) => stats,
            _ => unreachable!(),
        };

        for stat in stats {
            self.display_statement(stat, f, tabs)?;
        }

        Ok(())
    }

    fn display_statement(
        &self,
        stat: &ThirStatement,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match stat {
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
            ThirStatement::Block(b) => match &b.body {
                ThirBlock::Normal(stats) => {
                    tabs.fmt(f)?;
                    f.write_str("{\n")?;
                    let nested = tabs.nest();
                    for stat in stats {
                        self.display_statement(stat, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                ThirBlock::Unsafe(stats) => {
                    tabs.fmt(f)?;
                    f.write_str("unsafe {\n")?;
                    let nested = tabs.nest();
                    for stat in stats {
                        self.display_statement(stat, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                ThirBlock::Loop(stats) => {
                    tabs.fmt(f)?;
                    f.write_str("loop {\n")?;
                    let nested = tabs.nest();
                    for stat in stats {
                        self.display_statement(stat, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
            },
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
    mt: Spanned<Mutability>,
    ty: Spanned<Type>,
    debug_name: Option<Spanned<Symbol>>,
}

use super::hir;

pub struct ThirConverter<'a> {
    defs: &'a Definitions,
    inference_set: HashMap<InferId, Type>,
    var_defs: HashMap<HirVarId, ThirVarDef>,
    safety: Safety,
    stats: Vec<Spanned<ThirStatement>>,
    at_item: DefId,
    containing_item: DefId,
    next_infer: u32,
    dbgnames: HashMap<HirVarId, Spanned<Symbol>>,
}

impl<'a> ThirConverter<'a> {
    pub fn new(
        defs: &'a Definitions,
        safety: Safety,
        at_item: DefId,
        containing_item: DefId,
        dbgnames: HashMap<HirVarId, Spanned<Symbol>>,
    ) -> ThirConverter<'a> {
        ThirConverter {
            defs,
            inference_set: HashMap::new(),
            var_defs: HashMap::new(),
            safety: Safety::Safe,
            stats: Vec::new(),
            at_item,
            containing_item,
            next_infer: 0,
            dbgnames,
        }
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
                    let id = InferId(self.next_infer);
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
                    Some(Spanned {
                        body: SemaLifetime::Static,
                        span: tyspan,
                    }),
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
            hir::HirExpr::Const(defid) => {
                let def = self.defs.definition(*defid);

                let (ty, cat) = match &def.inner.body {
                    super::DefinitionInner::Function(fnty, _) => {
                        (Type::FnItem(fnty.clone(), *defid), ValueCategory::Rvalue)
                    }
                    _ => unreachable!("Not a value"),
                };

                let inner = ThirExprInner::Const(*defid);

                Ok(ThirExpr { ty, inner, cat })
            }
            hir::HirExpr::Unreachable => Ok(ThirExpr {
                ty: Type::Never,
                cat: ValueCategory::Rvalue,
                inner: ThirExprInner::Unreachable,
            }),
            hir::HirExpr::Cast(expr, ty) => Ok(ThirExpr {
                ty: ty.body.clone(),
                cat: ValueCategory::Rvalue,
                inner: ThirExprInner::Cast(Box::new(self.convert_rvalue(expr)?), ty.clone()),
            }),
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
        })
    }

    pub fn write_statement(&mut self, stat: &Spanned<hir::HirStatement>) -> super::Result<()> {
        let stat = self.convert_statement(stat)?;

        self.stats.push(stat);

        Ok(())
    }

    pub fn convert_statement(
        &mut self,
        stat: &Spanned<hir::HirStatement>,
    ) -> super::Result<Spanned<ThirStatement>> {
        stat.try_copy_span(|hirstat| match hirstat {
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
                let ty = ty.as_ref().cloned().unwrap_or_else(|| {
                    let infer = InferId(self.next_infer.fetch_increment());

                    Spanned {
                        span: var.span,
                        body: Type::Inferable(infer),
                    }
                });

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
            hir::HirStatement::Block(b) => Ok(ThirStatement::Block(b.try_copy_span(|b| {
                match b {
                    hir::HirBlock::Normal(stats) => Ok(ThirBlock::Normal(
                        stats
                            .iter()
                            .map(|stat| self.convert_statement(stat))
                            .collect::<Result<_>>()?,
                    )),
                    hir::HirBlock::Unsafe(stats) => Ok(ThirBlock::Unsafe(
                        stats
                            .iter()
                            .map(|stat| self.convert_statement(stat))
                            .collect::<Result<_>>()?,
                    )),
                    hir::HirBlock::Loop(stats) => Ok(ThirBlock::Loop(
                        stats
                            .iter()
                            .map(|stat| self.convert_statement(stat))
                            .collect::<Result<_>>()?,
                    )),
                }
            })?)),
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
            Safety::Safe => ThirBlock::Normal(self.stats),
            Safety::Unsafe => ThirBlock::Unsafe(self.stats),
        };
        ThirFunctionBody {
            vardefs: self.var_defs,
            body: Spanned { body, span },
        }
    }
}

pub struct Inferer<'a> {
    defs: &'a Definitions,
    curdef: DefId,
    curmod: DefId,
    inference_set: HashMap<InferId, Type>,
}

impl<'a> Inferer<'a> {
    pub fn new(defs: &'a Definitions, curdef: DefId, curmod: DefId) -> Self {
        Self {
            defs,
            curdef,
            curmod,
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
            (Type::Inferable(l), Type::Inferable(r)) => {
                let l = *l;
                let r = *r;

                // Note: We do RTL Propagation here. The left type is typically the assignee and the right type is typically the assigned
                // This *should* narrow the inference set quicker in general
                *left = Type::Inferable(r);

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
                    self.inference_set.insert(l, Type::Inferable(r));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::InferableInt(l), Type::Inferable(r)) => {
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
                    self.inference_set.insert(l, Type::Inferable(r));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::Inferable(l), Type::InferableInt(r)) => {
                let l = *l;
                let r = *r;

                //Note: We do RTL Propagation here because the right type is more specific.
                *left = Type::Inferable(r);

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
                    self.inference_set.insert(l, Type::Inferable(r));
                }

                Ok(CyclicOperationStatus::Incomplete)
            }
            (Type::Inferable(l), ty) => {
                let l = *l;

                if let Some(gty) = self.inference_set.get_mut(&l) {
                    let mut gty = core::mem::replace(ty, Type::Never);

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
                    let mut gty = core::mem::replace(ty, Type::Never);

                    self.unify_types(&mut gty, ty)?;
                    self.inference_set.insert(l, gty);
                } else {
                    self.inference_set.insert(l, ty.clone());
                }
                Ok(CyclicOperationStatus::Incomplete)
            }
            (ty, Type::Inferable(r)) => {
                let r = *r;

                if let Some(gty) = self.inference_set.get_mut(&r) {
                    let mut gty = core::mem::replace(ty, Type::Never);

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
                status &= self.unify_types(&mut inner.ty, &mut left.ty)?;
                status &= self.unify_single_expr(inner)?;
            }
            ThirExprInner::Unreachable => {}
            ThirExprInner::Var(_) => {}
            ThirExprInner::Const(_) => {}
            ThirExprInner::ConstInt(_, _) => {}
            ThirExprInner::ConstString(_, _) => {}
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

    pub fn unify_statement(
        &mut self,
        stmt: &mut ThirStatement,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match stmt {
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
                    Type::FnItem(ty, _) => ty,
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
            x => todo!("{:?}", x),
        }
        Ok(status)
    }

    pub fn unify_block(&mut self, blk: &mut ThirBlock) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match blk {
            ThirBlock::Normal(stats) | ThirBlock::Unsafe(stats) | ThirBlock::Loop(stats) => {
                for stat in stats {
                    status &= self.unify_statement(stat)?;
                }
            }
        }
        Ok(status)
    }

    pub fn propagate_block(&mut self, blk: &mut ThirBlock) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match blk {
            ThirBlock::Normal(stats) | ThirBlock::Unsafe(stats) | ThirBlock::Loop(stats) => {
                for stat in stats {
                    status &= self.propagate_statement(stat)?;
                }
            }
        }
        Ok(status)
    }

    pub fn propagate_statement(
        &mut self,
        stat: &mut ThirStatement,
    ) -> super::Result<CyclicOperationStatus> {
        let mut status = Complete;
        match stat {
            ThirStatement::Assign { dest, val, op } => {
                status &= self.propagate_expr(dest)?;
                status &= self.propagate_expr(val)?;
            }
            ThirStatement::Define {
                mutability,
                var,
                ty,
            } => status &= self.propagate_type(ty)?,
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
            Type::Inferable(infer) | Type::InferableInt(infer) => {
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
