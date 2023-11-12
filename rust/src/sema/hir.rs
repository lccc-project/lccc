use xlang::abi::collection::HashMap;

use crate::ast::{self, Literal, LiteralKind, Mutability, Safety, StringType, UnaryOp};
use crate::helpers::{FetchIncrement, TabPrinter};
use crate::interning::Symbol;
use crate::span::Span;

use super::ty::{self, FieldName, IntType, Type};

use super::{DefId, Spanned};

pub use crate::ast::BinaryOp;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct HirVarId(pub u32);

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
pub struct HirConstructor {
    pub constructor_def: Spanned<DefId>,
    pub fields: Vec<(Spanned<Symbol>, Spanned<HirExpr>)>,
    pub rest_init: Option<Box<Spanned<HirExpr>>>,
}

impl core::fmt::Display for HirConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.constructor_def.body.fmt(f)?;
        f.write_str("{ ")?;

        let mut sep = "";

        for (field, init) in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            f.write_str(field)?;
            f.write_str(": ")?;
            init.body.fmt(f)?;
        }

        if let Some(rest_init) = &self.rest_init {
            f.write_str(sep)?;
            f.write_str("..")?;
            rest_init.body.fmt(f)?;
        }
        f.write_str(" }")
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HirExpr {
    Var(HirVarId),
    ConstInt(Option<Spanned<IntType>>, u128),
    ConstString(StringType, Spanned<Symbol>),
    Const(DefId),
    #[allow(dead_code)]
    Unreachable,
    Cast(Box<Spanned<HirExpr>>, Spanned<Type>),
    Tuple(Vec<Spanned<HirExpr>>),
    Constructor(Spanned<HirConstructor>),
    FieldAccess(Box<Spanned<HirExpr>>, Spanned<FieldName>),
    BinaryExpr(
        Spanned<BinaryOp>,
        Box<Spanned<HirExpr>>,
        Box<Spanned<HirExpr>>,
    ),
    UnaryExpr(Spanned<UnaryOp>, Box<Spanned<HirExpr>>),
    Array(Vec<Spanned<HirExpr>>),
    Index(Box<Spanned<HirExpr>>, Box<Spanned<HirExpr>>),
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
            HirExpr::Constructor(ctor) => ctor.fmt(f),
            HirExpr::FieldAccess(expr, name) => {
                expr.body.fmt(f)?;
                f.write_str(".")?;
                name.body.fmt(f)
            }
            HirExpr::BinaryExpr(op, lhs, rhs) => {
                write!(f, "({} {} {})", lhs.body, op.body, rhs.body)
            }
            HirExpr::UnaryExpr(op, val) => {
                write!(f, "({}{})", op.body, val.body)
            }
            HirExpr::Array(elements) => {
                write!(f, "[")?;
                let mut sep = "";
                for el in elements {
                    write!(f, "{}{}", sep, el.body)?;
                    sep = ", ";
                }
                write!(f, "]")
            }
            HirExpr::Index(base, index) => {
                write!(f, "{}[{}]", base.body, index.body)
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

type HirSimpleBlock = Vec<Spanned<HirStatement>>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HirBlock {
    Normal(HirSimpleBlock),
    Unsafe(HirSimpleBlock),
    Loop(HirSimpleBlock),
    If {
        cond: Spanned<HirExpr>,
        block: HirSimpleBlock,
        elseifs: Vec<(Spanned<HirExpr>, HirSimpleBlock)>,
        elseblock: HirSimpleBlock,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirFunctionBody {
    pub vardebugmap: HashMap<HirVarId, Spanned<Symbol>>,
    pub body: Spanned<HirBlock>,
    pub localitems: Vec<(Symbol, DefId)>,
}

impl HirFunctionBody {
    pub fn display_body(
        &self,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        let stmts = match &self.body.body {
            HirBlock::Normal(stmts) | HirBlock::Unsafe(stmts) => stmts,
            _ => unreachable!(),
        };

        for stmt in stmts {
            self.display_statement(stmt, f, tabs)?;
        }

        Ok(())
    }

    fn display_statement(
        &self,
        stmt: &HirStatement,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match stmt {
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
                HirBlock::Normal(stmts) => {
                    tabs.fmt(f)?;
                    f.write_str("{\n")?;
                    let nested = tabs.nest();
                    for stmt in stmts {
                        self.display_statement(stmt, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                HirBlock::Unsafe(stmts) => {
                    tabs.fmt(f)?;
                    f.write_str("unsafe {\n")?;
                    let nested = tabs.nest();
                    for stmt in stmts {
                        self.display_statement(stmt, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                HirBlock::Loop(stmts) => {
                    tabs.fmt(f)?;
                    f.write_str("loop {\n")?;
                    let nested = tabs.nest();
                    for stmt in stmts {
                        self.display_statement(stmt, f, nested)?;
                    }
                    tabs.fmt(f)?;
                    f.write_str("}\n")
                }
                HirBlock::If {
                    cond,
                    block,
                    elseifs,
                    elseblock,
                } => {
                    tabs.fmt(f)?;
                    write!(f, "if {} {{\n", cond.body)?;
                    let nested = tabs.nest();
                    for stmt in block {
                        self.display_statement(stmt, f, nested)?;
                    }
                    for (cond, block) in elseifs {
                        write!(f, "}} else if {} {{\n", cond.body)?;
                        for stmt in block {
                            self.display_statement(stmt, f, nested)?;
                        }
                    }
                    if elseblock.len() > 0 {
                        f.write_str("} else {\n")?;
                        for stmt in elseblock {
                            self.display_statement(stmt, f, nested)?;
                        }
                    }
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
    defs: &'a mut super::Definitions,
    pub varnames: HashMap<Symbol, HirVarId>,
    pub vardebugmap: &'a mut HashMap<HirVarId, Spanned<Symbol>>,
    atitem: DefId,
    curmod: DefId,
    pub nexthirvarid: u32,
    stmts: Vec<Spanned<HirStatement>>,
    localvalues: HashMap<Symbol, DefId>,
    localtypes: HashMap<Symbol, DefId>,
    selfty: Option<&'a ty::Type>,
    localitems: &'a mut Vec<(Symbol, DefId)>,
}

impl<'a> HirLowerer<'a> {
    pub fn new(
        defs: &'a mut super::Definitions,
        atitem: DefId,
        curmod: DefId,
        vardebugmap: &'a mut HashMap<HirVarId, Spanned<Symbol>>,
        selfty: Option<&'a ty::Type>,
        localitems: &'a mut Vec<(Symbol, DefId)>,
    ) -> Self {
        Self {
            defs,
            atitem,
            curmod,
            varnames: HashMap::new(),
            vardebugmap,
            nexthirvarid: 0,
            stmts: Vec::new(),
            localvalues: HashMap::new(),
            localtypes: HashMap::new(),
            selfty,
            localitems,
        }
    }

    pub fn desugar_expr(&mut self, expr: &Spanned<ast::Expr>) -> super::Result<Spanned<HirExpr>> {
        match &expr.body {
            ast::Expr::IdExpr(p) => {
                if let Some(id) = p.as_bare_id() {
                    if let Some(hirvar) = self.varnames.get(&id.body) {
                        Ok(id.copy_span(|_| HirExpr::Var(*hirvar)))
                    } else if let Some(val) = self.localvalues.get(&id.body) {
                        Ok(id.copy_span(|_| HirExpr::Const(*val)))
                    } else {
                        self.defs
                            .find_value_in_mod(self.curmod, self.curmod, id, self.atitem)
                            .map(|def| id.copy_span(|_| HirExpr::Const(def)))
                    }
                } else {
                    todo!("complicated path (Add find_value)")
                }
            }
            ast::Expr::BinaryExpr(op, lhs, rhs) => expr.try_copy_span(|_| {
                Ok(HirExpr::BinaryExpr(
                    *op,
                    Box::new(self.desugar_expr(lhs)?),
                    Box::new(self.desugar_expr(rhs)?),
                ))
            }),
            ast::Expr::UnaryExpr(op, val) => expr
                .try_copy_span(|_| Ok(HirExpr::UnaryExpr(*op, Box::new(self.desugar_expr(val)?)))),
            ast::Expr::RangeFull => todo!("range"),
            ast::Expr::FunctionCall {
                base,
                method_name,
                args,
            } => {
                let hirvar = expr.copy_span(|_| HirVarId(self.nexthirvarid.fetch_increment()));
                self.stmts.push(expr.copy_span(|_| HirStatement::Define {
                    mutability: expr.copy_span(|_| Mutability::Const),
                    var: hirvar,
                    ty: None,
                }));
                let base = self.desugar_expr(base)?;
                let args = args
                    .iter()
                    .map(|expr| self.desugar_expr(expr))
                    .collect::<super::Result<Vec<_>>>()?;
                self.stmts.push(expr.copy_span(|_| HirStatement::Call {
                    retplace: Some(hirvar.copy_span(|hirvar| HirExpr::Var(*hirvar))),
                    fnexpr: base,
                    method_name: *method_name,
                    params: args,
                }));

                Ok(expr.copy_span(|_| HirExpr::Var(*hirvar)))
            }
            ast::Expr::MemberAccess(inner, field) => {
                let base = self.desugar_expr(inner)?;

                Ok(expr.copy_span(|_| {
                    HirExpr::FieldAccess(
                        Box::new(base),
                        field.copy_span(|name| FieldName::Field(*name)),
                    )
                }))
            }
            ast::Expr::Index { base, index } => expr.try_copy_span(|_| {
                Ok(HirExpr::Index(
                    Box::new(self.desugar_expr(base)?),
                    Box::new(self.desugar_expr(index)?),
                ))
            }),
            ast::Expr::AsCast(inner, ty) => {
                let inner = self.desugar_expr(inner)?;

                let ty = ty.try_copy_span(|ty| {
                    super::ty::convert_type(self.defs, self.curmod, self.atitem, ty, None)
                    // TODO: This also needs a SelfTy
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
            ast::Expr::Continue(_) => todo!("continue"),
            ast::Expr::Yield(_) => todo!("yield"),
            ast::Expr::ConstBlock(_) => todo!("const block"),
            ast::Expr::AsyncBlock(_) => todo!("async blck"),
            ast::Expr::Closure(_) => todo!("closure"),
            ast::Expr::Yeet(_) => todo!("yeet"),
            ast::Expr::Constructor(ctor) => {
                let ctor = ctor.try_copy_span(|ctor| {
                    let constructor_def = Spanned {
                        body: self.defs.find_constructor(
                            self.curmod,
                            &ctor.ctor_name,
                            self.atitem,
                            None, // TODO: Give this a SelfTy
                        )?,
                        span: ctor.ctor_name.span,
                    };
                    let mut fields = Vec::new();

                    for field in &ctor.fields {
                        let id = field.name;
                        let init = if let Some(val) = &field.val {
                            self.desugar_expr(val)?
                        } else {
                            if let Some(hirvar) = self.varnames.get(&id.body) {
                                id.copy_span(|_| HirExpr::Var(*hirvar))
                            } else {
                                self.defs
                                    .find_value_in_mod(self.curmod, self.curmod, id, self.atitem)
                                    .map(|def| id.copy_span(|_| HirExpr::Const(def)))?
                            }
                        };

                        fields.push((id, init));
                    }

                    let rest_init = ctor
                        .fill
                        .as_ref()
                        .map(|s| self.desugar_expr(s))
                        .transpose()?
                        .map(Box::new);

                    Ok(HirConstructor {
                        constructor_def,
                        fields,
                        rest_init,
                    })
                })?;

                Ok(expr.copy_span(|_| HirExpr::Constructor(ctor)))
            }
            ast::Expr::Await(_) => todo!("await"),
            ast::Expr::Try(_) => todo!("try"),
            ast::Expr::Group(expr) => self.desugar_expr(expr),
            ast::Expr::Tuple(_) => todo!("tuple"),
            ast::Expr::Return(_) => todo!("return"),
            ast::Expr::Array(elements) => expr.try_copy_span(|_| {
                Ok(HirExpr::Array(
                    elements
                        .iter()
                        .map(|x| self.desugar_expr(x))
                        .collect::<Result<_, _>>()?,
                ))
            }),
            ast::Expr::ArrayRepeat { .. } => todo!("array repeat"),
        }
    }

    pub fn desugar_stmt(
        &mut self,
        mut ret: Option<&mut dyn FnMut(Spanned<HirExpr>) -> HirStatement>,
        stmt: &Spanned<ast::Statement>,
    ) -> super::Result<()> {
        match &stmt.body {
            ast::Statement::Empty => {} // Yeet it out of existence.
            ast::Statement::DiscardExpr(expr) => {
                let expr = self.desugar_expr(expr)?;
                self.stmts
                    .push(stmt.copy_span(|_| HirStatement::Discard(expr)));
            }
            ast::Statement::ItemDecl(item) => match &item.item.body {
                ast::ItemBody::Mod(_) => todo!(),
                ast::ItemBody::Value(_) => todo!(),
                ast::ItemBody::ExternCrate { .. } => todo!(),
                ast::ItemBody::Use(_) => todo!(),
                ast::ItemBody::UserType(_) => {}
                ast::ItemBody::Function(itemfn) => {
                    let defid = self.defs.allocate_defid();
                    let mut fnbody = super::collect_function(
                        self.defs,
                        self.curmod,
                        defid,
                        itemfn,
                        None,
                        None,
                        None,
                        None,
                        self.selfty,
                    )?;

                    match &mut fnbody {
                        super::DefinitionInner::Function(_, Some(body)) => {
                            *body = super::FunctionBody::AstBody(
                                itemfn.params.clone(),
                                itemfn.body.body.as_ref().cloned().unwrap(),
                            );
                        }
                        _ => {}
                    }

                    let attrs = item
                        .attrs
                        .iter()
                        .map(|attr| super::attr::parse_meta(attr, defid, self.atitem))
                        .collect::<super::Result<Vec<_>>>()?;

                    let def = self.defs.definition_mut(defid);
                    def.attrs = attrs;
                    def.visible_from = self.atitem;
                    def.parent = self.atitem;
                    def.inner = itemfn.copy_span(|_| fnbody);

                    self.localitems.push((itemfn.name.body, defid));
                    self.localvalues.insert(itemfn.name.body, defid);
                }
                ast::ItemBody::ExternBlock(_) => todo!(),
                ast::ItemBody::MacroRules(_) => panic!("macro rules unreachable from rust"),
                ast::ItemBody::Trait(_) => todo!(),
                ast::ItemBody::ImplBlock(_) => todo!(),
            },
            ast::Statement::LetStatement(stmt) => {
                // TODO: handle patterns correctly
                let (mutability, var) = match stmt.name.body {
                    ast::Pattern::BareId(id) => (
                        Spanned {
                            body: Mutability::Const,
                            span: Span::synthetic(),
                        },
                        id.copy_span(|x| {
                            let next_id = HirVarId(self.nexthirvarid.fetch_increment());
                            self.varnames.insert(*x, next_id);
                            self.vardebugmap.insert(next_id, id);
                            next_id
                        }),
                    ),
                    _ => todo!(),
                };
                let ty = stmt
                    .ty
                    .as_ref()
                    .map(|x| {
                        x.try_copy_span(|x| {
                            ty::convert_type(self.defs, self.curmod, self.atitem, x, None)
                            // TODO: Give this a SelfTy
                        })
                    })
                    .transpose()?;
                self.stmts.push(stmt.copy_span(|_| HirStatement::Define {
                    mutability,
                    var,
                    ty,
                }));
                if let Some(x) = &stmt.val {
                    let val = self.desugar_expr(x)?;
                    self.stmts.push(stmt.copy_span(|_| HirStatement::Assign {
                        dest: var.copy_span(|x| HirExpr::Var(*x)),
                        val,
                        op: None,
                    }));
                }
            }
            ast::Statement::Block(blk) => match &blk.body {
                ast::CompoundBlock::SimpleBlock(blk) => {
                    let mut lowerer = HirLowerer::new(
                        self.defs,
                        self.atitem,
                        self.curmod,
                        self.vardebugmap,
                        self.selfty,
                        self.localitems,
                    );

                    lowerer.nexthirvarid = self.nexthirvarid;
                    lowerer.varnames = self.varnames.clone();
                    lowerer.localtypes = self.localtypes.clone();
                    lowerer.localvalues = self.localvalues.clone();

                    lowerer.desugar_block(ret, blk)?;

                    let block = HirBlock::Normal(lowerer.stmts);

                    self.stmts
                        .push(stmt.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                    self.nexthirvarid = lowerer.nexthirvarid;
                }
                ast::CompoundBlock::If(if_blk) => {
                    let cond = self.desugar_expr(&if_blk.cond)?;

                    let mut lowerer = HirLowerer::new(
                        self.defs,
                        self.atitem,
                        self.curmod,
                        self.vardebugmap,
                        self.selfty,
                        self.localitems,
                    );

                    lowerer.nexthirvarid = self.nexthirvarid;
                    lowerer.varnames = self.varnames.clone();

                    lowerer.desugar_block(
                        if let Some(ret) = &mut ret {
                            Some(ret)
                        } else {
                            None
                        },
                        &if_blk.block,
                    )?;
                    let block = lowerer.stmts;
                    self.nexthirvarid = lowerer.nexthirvarid;

                    let mut elseifs = Vec::new();
                    for elseif in &if_blk.elseifs {
                        let cond = self.desugar_expr(&elseif.cond)?;
                        let mut lowerer = HirLowerer::new(
                            self.defs,
                            self.atitem,
                            self.curmod,
                            self.vardebugmap,
                            self.selfty,
                            self.localitems,
                        );
                        lowerer.nexthirvarid = self.nexthirvarid;
                        lowerer.varnames = self.varnames.clone();
                        lowerer.desugar_block(
                            if let Some(ret) = &mut ret {
                                Some(ret)
                            } else {
                                None
                            },
                            &elseif.block,
                        )?;
                        elseifs.push((cond, lowerer.stmts));
                        self.nexthirvarid = lowerer.nexthirvarid;
                    }

                    let elseblock = if let Some(x) = if_blk.elseblock.as_ref() {
                        let mut lowerer = HirLowerer::new(
                            self.defs,
                            self.atitem,
                            self.curmod,
                            self.vardebugmap,
                            self.selfty,
                            self.localitems,
                        );

                        lowerer.nexthirvarid = self.nexthirvarid;
                        lowerer.varnames = self.varnames.clone();

                        lowerer.desugar_block(
                            if let Some(ret) = &mut ret {
                                Some(ret)
                            } else {
                                None
                            },
                            &x,
                        )?;
                        self.nexthirvarid = lowerer.nexthirvarid;

                        lowerer.stmts
                    } else {
                        Vec::new()
                    };

                    let if_block = HirBlock::If {
                        cond,
                        block,
                        elseifs,
                        elseblock,
                    };
                    self.stmts.push(
                        stmt.copy_span(|_| HirStatement::Block(if_blk.copy_span(|_| if_block))),
                    );
                }
                ast::CompoundBlock::While(_) => todo!("while"),
                ast::CompoundBlock::Loop(blk) => {
                    let mut lowerer = HirLowerer::new(
                        self.defs,
                        self.atitem,
                        self.curmod,
                        self.vardebugmap,
                        self.selfty,
                        self.localitems,
                    );

                    lowerer.nexthirvarid = self.nexthirvarid;
                    lowerer.varnames = self.varnames.clone();
                    lowerer.localtypes = self.localtypes.clone();
                    lowerer.localvalues = self.localvalues.clone();

                    lowerer.desugar_block(None, blk)?; // handle `break` at some point

                    let block = HirBlock::Loop(lowerer.stmts);

                    self.stmts
                        .push(stmt.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                    self.nexthirvarid = lowerer.nexthirvarid;
                }
                ast::CompoundBlock::Unsafe(blk) => {
                    let mut lowerer = HirLowerer::new(
                        self.defs,
                        self.atitem,
                        self.curmod,
                        self.vardebugmap,
                        self.selfty,
                        self.localitems,
                    );

                    lowerer.nexthirvarid = self.nexthirvarid;
                    lowerer.varnames = self.varnames.clone();
                    lowerer.localtypes = self.localtypes.clone();
                    lowerer.localvalues = self.localvalues.clone();

                    lowerer.desugar_block(ret, blk)?;

                    let block = HirBlock::Unsafe(lowerer.stmts);

                    self.stmts
                        .push(stmt.copy_span(|_| HirStatement::Block(blk.copy_span(|_| block))));
                    self.nexthirvarid = lowerer.nexthirvarid;
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

        let last_stmt = if blk.tail_expr.is_none() {
            if let Some((last, rest_stmts)) = stmts.split_last() {
                if let ast::Statement::Block(_) = &last.body {
                    stmts = rest_stmts;
                    Some(last)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        for stmt in stmts {
            self.desugar_stmt(None, stmt)?;
        }

        if let Some(stmt) = last_stmt {
            self.desugar_stmt(ret, stmt)?;
        } else if let Some(tail_expr) = &blk.tail_expr {
            let expr = self.desugar_expr(tail_expr)?;

            let return_stmt = (ret.unwrap_or(&mut |expr| HirStatement::Discard(expr)))(expr);

            self.stmts.push(tail_expr.copy_span(|_| return_stmt));
        } else {
            if let Some(ret) = ret {
                self.stmts
                    .push(blk.copy_span(|_| ret(blk.copy_span(|_| HirExpr::Tuple(vec![])))));
            }
        }

        Ok(())
    }

    pub fn into_fn_body(self, safety: Safety, span: Span) -> super::Result<HirFunctionBody> {
        let body = match safety {
            Safety::Safe => HirBlock::Normal(self.stmts),
            Safety::Unsafe => HirBlock::Unsafe(self.stmts),
        };

        for &(_, defid) in &*self.localitems {
            match &self.defs.definition(defid).inner.body {
                super::DefinitionInner::Function(_, Some(super::FunctionBody::AstBody(_, _))) => {
                    super::desugar_fn(self.defs, self.curmod, defid)?
                }
                _ => {}
            }
        }

        Ok(HirFunctionBody {
            vardebugmap: core::mem::take(self.vardebugmap),
            body: Spanned { body, span },
            localitems: core::mem::take(self.localitems),
        })
    }
}
