use xlang::abi::collection::HashMap;

use crate::ast::{self, CharType, Literal, LiteralKind, Mutability, Safety, StringType};
use crate::helpers::{FetchIncrement, TabPrinter};
use crate::interning::Symbol;
use crate::span::{synthetic, Span};

use super::generics::GenericArgs;
use super::ty::{self, FieldName, IntType, Type};

use super::{DefId, Spanned};

pub use crate::ast::{BinaryOp, UnaryOp};

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

pub fn parse_int_literal(
    val: Spanned<Symbol>,
    at_item: DefId,
    containing_item: DefId,
) -> super::Result<u128> {
    let Spanned { body, span } = val;

    body.parse().map_err(|_| super::Error {
        span,
        text: format!("Integer Literal `{}` is invalid", body),
        category: super::ErrorCategory::InvalidLiteral,
        at_item,
        containing_item,
        relevant_item: at_item,
        hints: vec![super::SemaHint {
            text: format!("The largest value Rust can represent is `{}`", u128::MAX),
            itemref: DefId::ROOT,
            refspan: span,
        }],
    })
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
    ConstChar(CharType, u32),
    Const(DefId, GenericArgs),
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
            HirExpr::Const(def, generics) => f.write_fmt(format_args!("{}{}", def, generics)),
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
            HirExpr::ConstChar(CharType::Default, val) => {
                f.write_str("'")?;
                if let Some(c) = char::from_u32(*val) {
                    c.escape_default().fmt(f)?;
                } else {
                    f.write_fmt(format_args!("\\u{{invalid char: {:04x}}}", val))?;
                }

                f.write_str("'")
            }
            HirExpr::ConstChar(CharType::Byte, val) => {
                f.write_str("'")?;
                match val {
                    &val @ 0x20..=0x7F => (val as u8 as char).fmt(f)?,
                    val => f.write_fmt(format_args!("\\x{:02x}", val))?,
                }
                f.write_str("'")
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
        block: Box<Spanned<HirBlock>>,
        elseblock: Option<Box<Spanned<HirBlock>>>,
    },
    Match(HirMatch),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum HirPatternMatcher {
    Hole,
    ConstInt(u128, Option<Spanned<IntType>>),
    Const(DefId),
}

impl core::fmt::Display for HirPatternMatcher {
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BindingMode {
    Ref,
    Unref,
}

impl core::fmt::Display for BindingMode {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Ref => f.write_str("ref "),
            Self::Unref => f.write_str("&"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PatternPathFragment {
    Field(FieldName),
    SliceElement(u32),
    SliceHead(u32),
    SliceTail(u32),
}

impl core::fmt::Display for PatternPathFragment {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Field(fname) => f.write_fmt(format_args!(".{}", fname)),
            Self::SliceElement(idx) => f.write_fmt(format_args!("[{}]", idx)),
            Self::SliceHead(tail_size) => f.write_fmt(format_args!("[..-{}]", tail_size)),
            Self::SliceTail(head_size) => f.write_fmt(format_args!("[{}..]", head_size)),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirPatternPath {
    pub elements: Vec<PatternPathFragment>,
}

impl core::fmt::Display for HirPatternPath {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for elem in &self.elements {
            elem.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirPatternBinding {
    pub path: HirPatternPath,
    pub mode: Option<Spanned<BindingMode>>,
    pub mutability: Spanned<Mutability>,
    pub var: Spanned<HirVarId>,
}

impl core::fmt::Display for HirPatternBinding {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.path.fmt(f)?;
        f.write_str(": ")?;
        if let Some(mode) = &self.mode {
            mode.body.fmt(f)?;
        }

        if let Mutability::Mut = self.mutability.body {
            f.write_str("mut ")?;
        }
        self.var.body.fmt(f)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirPattern {
    pub matcher: Spanned<HirPatternMatcher>,
    pub bindings: Vec<Spanned<HirPatternBinding>>,
}

impl core::fmt::Display for HirPattern {
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
pub struct HirMatchArm {
    pub pattern: Spanned<HirPattern>,
    pub guard: Option<Spanned<HirExpr>>,
    pub expansion: Spanned<HirSimpleBlock>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HirMatch {
    pub discriminee: Spanned<HirExpr>,
    pub arms: Vec<HirMatchArm>,
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

    fn display_block(
        &self,
        block: &HirBlock,
        f: &mut core::fmt::Formatter,
        tabs: TabPrinter,
    ) -> core::fmt::Result {
        use core::fmt::Display;
        match &block {
            HirBlock::Normal(stmts) => {
                f.write_str("{\n")?;
                let nested = tabs.nest();
                for stmt in stmts {
                    self.display_statement(stmt, f, nested)?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
            HirBlock::Unsafe(stmts) => {
                f.write_str("unsafe {\n")?;
                let nested = tabs.nest();
                for stmt in stmts {
                    self.display_statement(stmt, f, nested)?;
                }
                tabs.fmt(f)?;
                f.write_str("}\n")
            }
            HirBlock::Loop(stmts) => {
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
            HirBlock::Match(m) => {
                let nested = tabs.nest();
                write!(f, "match {} {{\n", m.discriminee.body)?;
                for arm in &m.arms {
                    nested.fmt(f)?;
                    let subnested = nested.nest();
                    arm.pattern.body.fmt(f)?;

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
            HirStatement::Block(b) => {
                tabs.fmt(f)?;
                self.display_block(b, f, tabs)
            }
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
                        Ok(id.copy_span(|_| HirExpr::Const(*val, GenericArgs::default())))
                    } else {
                        self.defs
                            .find_value_in_mod(self.curmod, self.curmod, id, self.atitem)
                            .map(|def| {
                                id.copy_span(|_| HirExpr::Const(def, GenericArgs::default()))
                            })
                    }
                } else if let Some((id, ast_generics)) = p.as_bare_id_with_generics() {
                    let defid = if let Some(val) = self.localvalues.get(&id.body) {
                        *val
                    } else {
                        self.defs
                            .find_value_in_mod(self.curmod, self.curmod, id, self.atitem)?
                    };

                    let mut generics = GenericArgs::default();
                    self.defs.convert_generics_for_def(
                        defid,
                        &mut generics,
                        ast_generics,
                        self.curmod,
                        self.atitem,
                        self.selfty,
                    )?;

                    Ok(p.copy_span(|_| HirExpr::Const(defid, generics)))
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
            ast::Expr::BlockExpr(block) => {
                let hirvar = expr.copy_span(|_| HirVarId(self.nexthirvarid.fetch_increment()));
                self.stmts.push(expr.copy_span(|_| HirStatement::Define {
                    mutability: expr.copy_span(|_| Mutability::Const),
                    var: hirvar,
                    ty: None,
                }));
                self.desugar_stmt(
                    Some(&mut |x| HirStatement::Assign {
                        dest: hirvar.copy_span(|_| HirExpr::Var(*hirvar)),
                        val: x,
                        op: None,
                    }),
                    &block.copy_span(|_| ast::Statement::Block(block.clone())),
                )?;
                Ok(expr.copy_span(|_| HirExpr::Var(*hirvar)))
            }
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
                    lit_kind: LiteralKind::String(sty),
                    val: sym,
                } => Ok(expr.copy_span(|_| HirExpr::ConstString(sty, sym))),
                Literal {
                    lit_kind: LiteralKind::Char(cty),
                    val: sym,
                } => {
                    dbg!(sym);
                    let mut c = sym.chars().peekable();

                    let val = match c.next().expect("missing a character") {
                        '\\' => match c.next().expect("malformed escape sequence") {
                            't' => '\t' as u32,
                            'n' => '\n' as u32,
                            'r' => '\r' as u32,
                            '\'' => '\'' as u32,
                            '"' => '"' as u32,
                            '0' => 0,
                            '\\' => '\\' as u32,
                            'x' => {
                                let mut val = 0;

                                match c
                                    .next()
                                    .expect("Malformed escape sequence - error in lexer")
                                {
                                    c => val = c.to_digit(16).expect("Malformed escape sequence"),
                                }

                                match c
                                    .peek()
                                    .expect("Malformed character literal - error in lexer")
                                {
                                    '\'' => {}
                                    &v => {
                                        c.next();
                                        val <<= 4;
                                        val |= v
                                            .to_digit(16)
                                            .expect("Malformed character literal - error in lexer");
                                    }
                                }

                                val
                            }
                            'u' => {
                                assert_eq!(
                                    c.next(),
                                    Some('{'),
                                    "Malformed escape sequence - error in lexer"
                                );
                                let mut val = 0;
                                for c in &mut c {
                                    match c {
                                        '}' => break,
                                        c => {
                                            val <<= 4;
                                            val |= c.to_digit(16).expect(
                                                "Malformed character literal - error in lexer",
                                            );
                                        }
                                    }
                                }
                                val
                            }
                            val => panic!("Expected an escape sequence, got {}", val),
                        },
                        val => val as u32,
                    };

                    Ok(expr.copy_span(|_| HirExpr::ConstChar(cty, val)))
                }
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
                        body: self
                            .defs
                            .find_constructor(
                                self.curmod,
                                &ctor.ctor_name,
                                self.atitem,
                                None, // TODO: Give this a SelfTy
                            )?
                            .0,
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
                                    .map(|def| {
                                        id.copy_span(|_| {
                                            HirExpr::Const(def, GenericArgs::default())
                                        })
                                    })?
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
            ast::Expr::Group(expr) | ast::Expr::Frag(expr) => self.desugar_expr(expr),
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

    fn build_pattern(
        &mut self,
        pat: &Spanned<ast::Pattern>,
        mut path: Vec<PatternPathFragment>,
        mut bindings: &mut Vec<Spanned<HirPatternBinding>>,
        binding_name_map: &mut HashMap<Symbol, HirVarId>,
    ) -> super::Result<Spanned<HirPatternMatcher>> {
        match &pat.body {
            ast::Pattern::BareId(id) => {
                if let Some(cn) = self.localvalues.get(&id.body) {
                    todo!("const pattern")
                } else if let Ok(cn) =
                    self.defs
                        .find_value_in_mod(self.curmod, self.curmod, *id, self.atitem)
                {
                    todo!("const pattern")
                } else {
                    let var = *binding_name_map.get_or_insert_with_mut(id.body, |_| {
                        HirVarId(self.nexthirvarid.fetch_increment())
                    });
                    let binding = HirPatternBinding {
                        path: HirPatternPath { elements: path },
                        mode: None,
                        mutability: synthetic(Mutability::Const),
                        var: id.copy_span(|_| var),
                    };
                    bindings.push(Spanned {
                        body: binding,
                        span: id.span,
                    });
                    Ok(Spanned {
                        body: HirPatternMatcher::Hole,
                        span: pat.span,
                    })
                }
            }
            ast::Pattern::Const(_) => todo!(),
            ast::Pattern::Binding(_) => todo!(),
            ast::Pattern::Discard => Ok(Spanned {
                body: HirPatternMatcher::Hole,
                span: pat.span,
            }),
            ast::Pattern::Tuple(_) => todo!(),
            ast::Pattern::Ref(_, _) => todo!(),
            ast::Pattern::OrPattern(_) => todo!(),
            ast::Pattern::RangePattern(_, _) => todo!(),
            ast::Pattern::RangeInclusivePattern(_, _) => todo!(),
            ast::Pattern::LiteralPattern(lit) => match lit.lit_kind {
                LiteralKind::String(_) => todo!(),
                LiteralKind::Char(_) => todo!(),
                LiteralKind::Int(intty) => {
                    let intty = intty
                        .map(|sym| ty::parse_int_suffix(sym, self.atitem, self.curmod))
                        .transpose()?;
                    let val = parse_int_literal(lit.val, self.atitem, self.curmod)?;

                    Ok(Spanned {
                        body: HirPatternMatcher::ConstInt(val, intty),
                        span: pat.span,
                    })
                }
                LiteralKind::Float(_) => todo!(),
                LiteralKind::Bool => todo!(),
            },
        }
    }

    pub fn desugar_pattern(
        &mut self,
        pat: &Spanned<ast::Pattern>,
        binding_name_map: &mut HashMap<Symbol, HirVarId>,
    ) -> super::Result<Spanned<HirPattern>> {
        let path = vec![];
        let mut bindings = vec![];
        let matcher = self.build_pattern(pat, path, &mut bindings, binding_name_map)?;

        Ok(Spanned {
            body: HirPattern { matcher, bindings },
            span: pat.span,
        })
    }

    pub fn desugar_stmt<'b>(
        &mut self,
        mut ret: Option<&'b mut dyn FnMut(Spanned<HirExpr>) -> HirStatement>,
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
                    let defid = self.defs.allocate_defid(itemfn.name.body);
                    let (mut fnbody, generics) = super::collect_function(
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
                    def.generics = generics;
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

                    let block = Spanned {
                        body: HirBlock::Normal(lowerer.stmts),
                        span: if_blk.block.span,
                    };

                    self.nexthirvarid = lowerer.nexthirvarid;

                    let mut partial_blocks = Vec::new();
                    for elseif in &if_blk.elseifs {
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
                        let cond = lowerer.desugar_expr(&elseif.cond)?;

                        let preamble_stmts = if lowerer.stmts.is_empty() {
                            None
                        } else {
                            Some(lowerer.stmts)
                        };

                        self.nexthirvarid = lowerer.nexthirvarid;

                        let mut inner_lowerer = HirLowerer::new(
                            self.defs,
                            self.atitem,
                            self.curmod,
                            self.vardebugmap,
                            self.selfty,
                            self.localitems,
                        );

                        inner_lowerer.nexthirvarid = self.nexthirvarid;

                        inner_lowerer.varnames = self.varnames.clone();

                        inner_lowerer.desugar_block(
                            if let Some(ret) = &mut ret {
                                Some(ret)
                            } else {
                                None
                            },
                            &elseif.block,
                        )?;

                        self.nexthirvarid = inner_lowerer.nexthirvarid;

                        partial_blocks.push((
                            preamble_stmts,
                            elseif.copy_span(|_| HirBlock::If {
                                cond: cond,
                                block: Box::new(
                                    elseif
                                        .block
                                        .copy_span(|_| HirBlock::Normal(inner_lowerer.stmts)),
                                ),
                                elseblock: None,
                            }),
                        ))
                    }

                    let mut elseblock = if let Some(x) = if_blk.elseblock.as_ref() {
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

                        Some(x.copy_span(|_| HirBlock::Normal(lowerer.stmts)))
                    } else {
                        None
                    };

                    for (preamble, mut block) in partial_blocks.into_iter().rev() {
                        match &mut block.body {
                            HirBlock::If {
                                elseblock: elseblk, ..
                            } => *elseblk = elseblock.map(Box::new),
                            _ => unreachable!(),
                        }

                        if let Some(mut preamble) = preamble {
                            let span = block.span;
                            let stmt = HirStatement::Block(block);
                            preamble.push(Spanned { body: stmt, span });
                            block = Spanned {
                                body: HirBlock::Normal(preamble),
                                span,
                            };
                        }

                        elseblock = Some(block);
                    }

                    let if_block = HirBlock::If {
                        cond,
                        block: Box::new(block),
                        elseblock: elseblock.map(Box::new),
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
                ast::CompoundBlock::Match(m) => {
                    let expr = self.desugar_expr(&m.discriminee)?;
                    let mut arms = vec![];
                    for arm in &m.arms {
                        let mut binding_name_map = HashMap::new();
                        let pattern = self.desugar_pattern(&arm.discrim, &mut binding_name_map)?;

                        if let Some(guard) = &arm.guard {
                            todo!("match guard")
                        }

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
                        lowerer.varnames.extend(binding_name_map);
                        lowerer.localtypes = self.localtypes.clone();
                        lowerer.localvalues = self.localvalues.clone();

                        match &arm.value.body {
                            ast::MatchArmValue::Expr(e) => {
                                let expr = lowerer.desugar_expr(e)?;
                                let span = expr.span;
                                let stmt = if let Some(ret) = &mut ret {
                                    ret(expr)
                                } else {
                                    HirStatement::Discard(expr)
                                };
                                lowerer.stmts.push(Spanned { body: stmt, span });
                            }
                            ast::MatchArmValue::Block(blk) => {
                                lowerer.desugar_block(
                                    if let Some(ret) = &mut ret {
                                        Some(ret)
                                    } else {
                                        None
                                    },
                                    blk,
                                )?;
                            }
                        }

                        let expansion = Spanned {
                            body: lowerer.stmts,
                            span: arm.value.span,
                        };

                        self.nexthirvarid = lowerer.nexthirvarid;

                        let arm = HirMatchArm {
                            pattern,
                            guard: None,
                            expansion,
                        };

                        arms.push(arm);
                    }
                    let stmt = HirMatch {
                        discriminee: expr,
                        arms,
                    };

                    let match_block = m.copy_span(|_| HirBlock::Match(stmt));

                    self.stmts.push(Spanned {
                        body: HirStatement::Block(match_block),
                        span: m.span,
                    });
                }
            },
        }
        Ok(())
    }

    pub fn desugar_block<'b>(
        &mut self,
        ret: Option<&'b mut dyn FnMut(Spanned<HirExpr>) -> HirStatement>,
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
