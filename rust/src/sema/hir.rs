use xlang::abi::collection::HashMap;

use crate::ast::{self, Literal, LiteralKind, Mutability, Safety, StringType};
use crate::helpers::{FetchIncrement, TabPrinter};
use crate::interning::Symbol;
use crate::span::Span;

use super::ty::{IntType, Type};

use super::{DefId, Spanned};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct HirVarId(u32);

impl core::fmt::Display for HirVarId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("_H{}", self.0))
    }
}

impl core::fmt::Debug for HirVarId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("_H{}", self.0))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HirExpr {
    Var(HirVarId),
    ConstInt(Option<Spanned<IntType>>, u128),
    ConstString(StringType, Spanned<Symbol>),
    Const(DefId),
    Unreachable,
    Cast(Box<Spanned<HirExpr>>, Spanned<Type>),
    Tuple(Vec<Spanned<HirExpr>>),
}

impl core::fmt::Display for HirExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            HirExpr::Var(var) => var.fmt(f),
            HirExpr::ConstInt(intty, val) => {
                val.fmt(f)?;
                if let Some(ity) = intty {
                    f.write_str("_")?;
                    ity.fmt(f)?;
                }
                Ok(())
            }
            HirExpr::Const(def) => def.fmt(f),
            HirExpr::Unreachable => f.write_str("unreachable"),
            HirExpr::Cast(inner, ty) => {
                f.write_str("(")?;
                inner.body.fmt(f)?;
                f.write_str(") as ")?;
                ty.body.fmt(f)
            }
            HirExpr::ConstString(_, s) => {
                f.write_str("\"")?;
                s.escape_default().fmt(f)?;
                f.write_str("\"")
            }
            HirExpr::Tuple(v) => {
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
pub enum HirStatement {
    Assign {
        dest: Spanned<HirExpr>,
        val: Spanned<HirExpr>,
        op: Option<Spanned<ast::BinaryOp>>,
    },
    Define {
        mutability: Spanned<Mutability>,
        var: Spanned<HirVarId>,
        ty: Option<Spanned<super::ty::Type>>,
    },
    Return(Spanned<HirExpr>),
    Block(Spanned<HirBlock>),
    Discard(Spanned<HirExpr>),
    Call {
        retplace: Option<Spanned<HirExpr>>,
        fnexpr: Spanned<HirExpr>,
        method_name: Option<Spanned<Symbol>>,
        params: Vec<Spanned<HirExpr>>,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HirBlock {
    Normal(Vec<Spanned<HirStatement>>),
    Unsafe(Vec<Spanned<HirStatement>>),
    Loop(Vec<Spanned<HirStatement>>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirFunctionBody {
    pub vardebugmap: HashMap<HirVarId, Spanned<Symbol>>,
    pub body: Spanned<HirBlock>,
}

impl HirFunctionBody {
    pub fn display_body(
        &self,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        let stats = match &self.body.body {
            HirBlock::Normal(stats) | HirBlock::Unsafe(stats) => stats,
            _ => unreachable!(),
        };

        for stat in stats {
            self.display_statement(stat, f, tabs)?;
        }

        Ok(())
    }

    fn display_statement(
        &self,
        stat: &HirStatement,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match stat {
            HirStatement::Assign { dest, val, op } => {
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
            HirStatement::Return(expr) => {
                f.write_fmt(format_args!("{}return {};\n", tabs, expr.body))
            }
            HirStatement::Block(b) => match &b.body {
                HirBlock::Normal(stats) => {
                    tabs.fmt(f)?;
                    f.write_str("{\n")?;
                    let nested = tabs.nest();
                    for stat in stats {
                        self.display_statement(stat, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                HirBlock::Unsafe(stats) => {
                    tabs.fmt(f)?;
                    f.write_str("unsafe {\n")?;
                    let nested = tabs.nest();
                    for stat in stats {
                        self.display_statement(stat, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                HirBlock::Loop(stats) => {
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
            HirStatement::Discard(expr) => f.write_fmt(format_args!("{}{};\n", tabs, expr.body)),
            HirStatement::Call {
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
            HirStatement::Define {
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

                if let Some(name) = self.vardebugmap.get(&var.body) {
                    f.write_str(" /* ")?;
                    f.write_str(name)?;
                    f.write_str(" */")?;
                }

                if let Some(ty) = ty {
                    f.write_str(": ")?;
                    ty.body.fmt(f)?;
                }

                f.write_str(";\n") // Note: Assignment is on a different line
            }
        }
    }
}

pub struct HirLowerer<'a> {
    defs: &'a super::Definitions,
    varnames: HashMap<Symbol, HirVarId>,
    vardebugmap: &'a mut HashMap<HirVarId, Spanned<Symbol>>,
    atitem: DefId,
    curmod: DefId,
    nexthirvarid: u32,
    stats: Vec<Spanned<HirStatement>>,
}

impl<'a> HirLowerer<'a> {
    pub fn new(
        defs: &'a super::Definitions,
        atitem: DefId,
        curmod: DefId,
        vardebugmap: &'a mut HashMap<HirVarId, Spanned<Symbol>>,
    ) -> Self {
        Self {
            defs,
            atitem,
            curmod,
            varnames: HashMap::new(),
            vardebugmap,
            nexthirvarid: 0,
            stats: Vec::new(),
        }
    }

    pub fn desugar_expr(&mut self, expr: &Spanned<ast::Expr>) -> super::Result<Spanned<HirExpr>> {
        match &expr.body {
            ast::Expr::IdExpr(p) => {
                if let Some(id) = p.as_bare_id() {
                    if let Some(hirvar) = self.varnames.get(&id.body) {
                        Ok(id.copy_span(|_| HirExpr::Var(*hirvar)))
                    } else {
                        self.defs
                            .find_value_in_mod(self.curmod, self.curmod, id, self.atitem)
                            .map(|def| id.copy_span(|_| HirExpr::Const(def)))
                    }
                } else {
                    todo!("complicated path")
                }
            }
            ast::Expr::BinaryExpr(_, _, _) => todo!("binary expr"),
            ast::Expr::UnaryExpr(_, _) => todo!("unary expr"),
            ast::Expr::RangeFull => todo!("range"),
            ast::Expr::FunctionCall {
                base,
                method_name,
                args,
            } => {
                let hirvar = expr.copy_span(|_| HirVarId(self.nexthirvarid.fetch_increment()));
                self.stats.push(expr.copy_span(|_| HirStatement::Define {
                    mutability: expr.copy_span(|_| Mutability::Const),
                    var: hirvar,
                    ty: None,
                }));
                let base = self.desugar_expr(base)?;
                let args = args
                    .iter()
                    .map(|expr| self.desugar_expr(expr))
                    .collect::<super::Result<Vec<_>>>()?;
                self.stats.push(expr.copy_span(|_| HirStatement::Call {
                    retplace: Some(hirvar.copy_span(|hirvar| HirExpr::Var(*hirvar))),
                    fnexpr: base,
                    method_name: *method_name,
                    params: args,
                }));

                Ok(expr.copy_span(|_| HirExpr::Var(*hirvar)))
            }
            ast::Expr::MemberAccess(_, _) => todo!("member access"),
            ast::Expr::Index { base, index } => todo!("index"),
            ast::Expr::AsCast(inner, ty) => {
                let inner = self.desugar_expr(inner)?;

                let ty = ty.try_copy_span(|ty| {
                    super::ty::convert_type(self.defs, self.curmod, self.atitem, ty)
                })?;

                Ok(expr.copy_span(|_| HirExpr::Cast(Box::new(inner), ty)))
            }
            ast::Expr::BlockExpr(_) => todo!("block expr"),
            ast::Expr::Literal(lit) => match lit.body {
                Literal {
                    lit_kind: LiteralKind::Int(suffix),
                    val: sym,
                } => {
                    let ty = suffix
                        .map(|suffix| super::ty::parse_int_suffix(suffix, self.atitem, self.curmod))
                        .transpose()?;

                    let val = sym.parse::<u128>().map_err(|_| super::Error {
                        span: sym.span,
                        text: format!("Invalid integer literal {}", sym.body),
                        category: super::ErrorCategory::InvalidExpression,
                        at_item: self.atitem,
                        containing_item: self.curmod,
                        relevant_item: self.atitem,
                        hints: vec![],
                    })?;

                    Ok(expr.copy_span(|_| HirExpr::ConstInt(ty, val)))
                }
                Literal {
                    lit_kind: LiteralKind::String(skind),
                    val: sym,
                } => Ok(expr.copy_span(|_| HirExpr::ConstString(skind, sym))),
                _ => todo!("literal"),
            },
            ast::Expr::Break(_, _) => todo!("break"),
            ast::Expr::Continue(_, _) => todo!("continue"),
            ast::Expr::Yield(_) => todo!("yield"),
            ast::Expr::ConstBlock(_) => todo!("const block"),
            ast::Expr::AsyncBlock(_) => todo!("async blck"),
            ast::Expr::Closure(_) => todo!("closure"),
            ast::Expr::Yeet(_) => todo!("yeet"),
        }
    }

    pub fn desugar_stat(
        &mut self,
        ret: Option<&mut dyn FnMut(Spanned<HirExpr>) -> HirStatement>,
        stat: &Spanned<ast::Statement>,
    ) -> super::Result<()> {
        match &stat.body {
            ast::Statement::Empty => {} // Yeet it out of existence.
            ast::Statement::DiscardExpr(expr) => {
                let expr = self.desugar_expr(expr)?;
                self.stats
                    .push(stat.copy_span(|_| HirStatement::Discard(expr)));
            }
            ast::Statement::ItemDecl(_) => {}
            ast::Statement::Block(blk) => match &blk.body {
                ast::CompoundBlock::SimpleBlock(blk) => {
                    let mut lowerer =
                        HirLowerer::new(self.defs, self.atitem, self.curmod, self.vardebugmap);

                    lowerer.varnames = self.varnames.clone();

                    lowerer.desugar_block(ret, blk)?;

                    let block = HirBlock::Normal(lowerer.stats);

                    self.stats
                        .push(stat.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                }
                ast::CompoundBlock::If(_) => todo!("if"),
                ast::CompoundBlock::While(_) => todo!("while"),
                ast::CompoundBlock::Loop(blk) => {
                    let mut lowerer =
                        HirLowerer::new(self.defs, self.atitem, self.curmod, self.vardebugmap);

                    lowerer.varnames = self.varnames.clone();

                    lowerer.desugar_block(ret, blk)?;

                    let block = HirBlock::Loop(lowerer.stats);

                    self.stats
                        .push(stat.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                }
                ast::CompoundBlock::Unsafe(blk) => {
                    let mut lowerer =
                        HirLowerer::new(self.defs, self.atitem, self.curmod, self.vardebugmap);

                    lowerer.varnames = self.varnames.clone();

                    lowerer.desugar_block(ret, blk)?;

                    let block = HirBlock::Unsafe(lowerer.stats);

                    self.stats
                        .push(stat.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                }
                ast::CompoundBlock::For(_) => todo!("for"),
            },
        }
        Ok(())
    }

    pub fn desugar_block(
        &mut self,
        ret: Option<&mut dyn FnMut(Spanned<HirExpr>) -> HirStatement>,
        blk: &Spanned<ast::Block>,
    ) -> super::Result<()> {
        let mut stmts = blk.stmts.as_slice();

        let last_stat = if blk.tail_expr.is_none() {
            if let Some((last, stats)) = stmts.split_last() {
                stmts = stats;
                if let ast::Statement::Empty = &last.body {
                    None
                } else {
                    Some(last)
                }
            } else {
                None
            }
        } else {
            None
        };

        for stat in stmts {
            self.desugar_stat(None, stat)?;
        }

        if let Some(stat) = last_stat {
            self.desugar_stat(ret, stat)?;
        } else if let Some(tail_expr) = &blk.tail_expr {
            let expr = self.desugar_expr(tail_expr)?;

            let return_stat = (ret.unwrap_or(&mut |expr| HirStatement::Discard(expr)))(expr);

            self.stats.push(tail_expr.copy_span(|_| return_stat));
        } else {
            if let Some(ret) = ret {
                self.stats
                    .push(blk.copy_span(|_| ret(blk.copy_span(|_| HirExpr::Tuple(vec![])))));
            }
        }
        Ok(())
    }

    pub fn into_fn_body(self, safety: Safety, span: Span) -> HirFunctionBody {
        let body = match safety {
            Safety::Safe => HirBlock::Normal(self.stats),
            Safety::Unsafe => HirBlock::Unsafe(self.stats),
        };

        HirFunctionBody {
            vardebugmap: core::mem::take(self.vardebugmap),
            body: Spanned { body, span },
        }
    }
}
