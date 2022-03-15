#![allow(clippy::cognitive_complexity, clippy::too_many_lines)] // TODO: You figure it out rdr

pub use crate::lex::StrType; // TODO: `pub use` this in `crate::parse`
use crate::parse::{
    FieldName, FnParam, Item, Mod, Path, PathComponent, Pattern, StructBody, TypeTag,
};
pub use crate::parse::{Meta, Mutability, Safety, SimplePath, Visibility};
use std::collections::hash_map::{Entry, HashMap};
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Identifier {
    Basic {
        mangling: Option<Mangling>,
        name: String,
    },
}

impl Identifier {
    pub fn mangling(&self) -> Option<Mangling> {
        let Self::Basic { mangling, .. } = self;
        mangling.as_ref().cloned()
    }

    pub fn matches(&self, other: &Self) -> bool {
        let Self::Basic { name, .. } = self;
        let Self::Basic {
            name: other_name, ..
        } = other;
        name == other_name
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Identifier::Basic { name, .. } = self;
        write!(f, "{}", name)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    I128,
    Iptr,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Uptr,
    Usize,
}

impl IntType {
    pub const fn is_signed(self) -> bool {
        matches!(
            self,
            IntType::I8
                | IntType::I16
                | IntType::I32
                | IntType::I64
                | IntType::Iptr
                | IntType::Isize
        )
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Constraint {
    Integer { min_bits: u8, signed: bool },
    Type(Type),
}

#[allow(dead_code)]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Array(Box<Self>, usize),
    Boolean,
    Char,
    Float(FloatType),
    Function(FunctionSignature),
    Integer(IntType),
    Never,
    Partial(Vec<Constraint>),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Reference {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Struct {
        name: Identifier,
        fields: Option<Vec<(String, Self)>>,
    },
    Str,
    Tuple(Vec<Self>),
}

#[allow(dead_code)]
impl Type {
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::Boolean => String::from("bool"),
            Self::Char => String::from("char"),
            Self::Float(ty) => String::from(match ty {
                FloatType::F32 => "f32",
                FloatType::F64 => "f64",
            }),
            Self::Integer(ty) => String::from(match ty {
                IntType::I8 => "i8",
                IntType::I16 => "i16",
                IntType::I32 => "i32",
                IntType::I64 => "i64",
                IntType::I128 => "i128",
                IntType::Iptr => "iptr",
                IntType::Isize => "isize",
                IntType::U8 => "u8",
                IntType::U16 => "u16",
                IntType::U32 => "u32",
                IntType::U64 => "u64",
                IntType::U128 => "u128",
                IntType::Uptr => "uptr",
                IntType::Usize => "usize",
            }),
            Self::Never => String::from("!"),
            Self::Partial(_) => String::from("_"),
            Self::Pointer {
                mutability,
                underlying,
            } => {
                let mut result = String::from("*");
                result.push_str(if *mutability == Mutability::Mut {
                    "mut "
                } else {
                    "const "
                });
                result.push_str(&underlying.name());
                result
            }
            Self::Struct { name, .. } => name.to_string(),
            Self::Str => String::from("str"),
            Self::Tuple(types) => {
                let mut result = String::from("(");
                let mut comma = false;
                for ty in types {
                    if comma {
                        result.push_str(", ");
                    }
                    result.push_str(&ty.name());
                    comma = true;
                }
                result.push(')');
                result
            }
            _ => todo!("{:?}", self),
        }
    }

    pub fn is_unit(&self) -> bool {
        if let Self::Tuple(types) = self {
            types.is_empty()
        } else {
            false
        }
    }

    pub fn constrain(&mut self, constraint: Constraint) {
        match (&constraint, self) {
            (_, Self::Partial(constraints)) => constraints.push(constraint),
            (Constraint::Integer { .. }, Self::Integer(_)) => todo!(),
            (Constraint::Type(ty), ty2) if ty == ty2 => {}
            (Constraint::Type(ty), ty2) => {
                panic!("couldn't resolve `{}` as `{}`", ty2, ty);
            }
            (a, b) => todo!("constrain({:?}, {:?})", b, a),
        }
    }

    pub fn resolve(&mut self) {
        match self {
            Self::Partial(constraints) => {
                if constraints.is_empty() {
                    panic!("type annotations needed");
                }
                let mut result = None;
                for constraint in constraints {
                    match constraint {
                        Constraint::Integer { .. } => todo!(),
                        Constraint::Type(ty) => {
                            if let Some(result) = result.as_ref() {
                                if result != ty {
                                    panic!("couldn't resolve `{}` as `{}`", result, ty)
                                }
                            } else {
                                result = Some(ty.clone()); // TODO: wait for borrowck to get better; theoretically, this can move
                            }
                        }
                    }
                }
                *self = result.expect("type annotations needed");
            }
            _ => {}
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Abi {
    C,              // "C"
    CUnwind,        // "C-unwind"
    Fastcall,       // "fastcall"
    FastcallUnwind, // "fastcall-unwind"
    Local,          // Used by local variables
    Rust,           // "Rust"
    RustCall,       // "rust-call"
    RustcallV0,     // "rustcall-v0", not to be confused with "rust-call"
    RustIntrinsic,  // "rust-intrinsic"
    Stdcall,        // "stdcall"
    StdcallUnwind,  // "stdcall-unwind"
}

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mangling {
    C,
    Local,
    Rust,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionSignature {
    pub abi: Abi,
    pub params: Vec<Type>,
    pub return_ty: Box<Type>,
    pub safety: Safety,
    pub visibility: Visibility,
}

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "(")?;
        let comma = false;
        for param in &self.params {
            if comma {
                write!(f, ", ")?;
            }
            param.fmt(f)?;
        }
        write!(f, ") -> {}", self.return_ty)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Declaration {
    Function {
        has_definition: bool,
        name: Identifier,
        sig: FunctionSignature,
    },
    Local {
        name: Identifier,
        ty: Type,
    },
}

impl Declaration {
    #[must_use]
    pub const fn abi(&self) -> Abi {
        if let Declaration::Function {
            sig: FunctionSignature { abi, .. },
            ..
        } = self
        {
            *abi
        } else {
            Abi::Local
        }
    }

    #[must_use]
    pub const fn has_definition(&self) -> bool {
        if let Declaration::Function { has_definition, .. } = self {
            *has_definition
        } else {
            true
        }
    }

    #[must_use]
    pub const fn name(&self) -> &Identifier {
        let (Declaration::Function { name, .. } | Declaration::Local { name, .. }) = self;
        name
    }

    #[must_use]
    pub fn ty(&self) -> Type {
        match self {
            Declaration::Function { sig, .. } => Type::Function(sig.clone()),
            Declaration::Local { ty, .. } => ty.clone(),
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Declaration::Function { name, sig, .. } => writeln!(f, "fn {}{};", name, sig),
            Declaration::Local { name, ty } => writeln!(f, "local {}: {}", name, ty),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expression {
    Cast {
        expr: Box<Expression>,
        target: Type,
    },
    FieldAccess {
        lhs: Box<Expression>,
        name: String,
        ty: Option<Type>,
    },
    FunctionArg(usize),
    FunctionCall {
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Identifier {
        id: Identifier,
        ty: Option<Type>,
    },
    IntegerLiteral {
        val: u128,
        ty: Option<IntType>,
    },
    StringLiteral {
        kind: StrType,
        val: String,
    },
    StructInitializer {
        args: Vec<(Identifier, Self)>,
        ty: Type,
    },
    UnsafeBlock {
        block: Vec<Statement>,
        ty: Option<Type>,
    },
}

impl Expression {
    #[allow(dead_code)]
    pub fn ty(&self) -> Type {
        match self {
            Expression::Cast { target: ty, .. } | Expression::StructInitializer { ty, .. } => {
                ty.clone()
            }
            Expression::FunctionArg(_) => {
                todo!("Expression::FunctionArg(_).ty() currently unsopported")
            }
            Expression::FunctionCall { func, .. } => {
                if let Type::Function(sig) = func.ty() {
                    *sig.return_ty
                } else {
                    panic!("attempted to call a value that isn't a function, uncaught by typeck; this is an ICE");
                }
            }
            Expression::FieldAccess { ty, .. }
            | Expression::Identifier { ty, .. }
            | Expression::UnsafeBlock { ty, .. } => ty
                .as_ref()
                .expect("typeck forgot to resolve type of expression; this is an ICE")
                .clone(),
            Expression::IntegerLiteral { ty, .. } => Type::Integer(
                *ty.as_ref()
                    .expect("typeck forgot to resolve type of expression; this is an ICE"),
            ),
            Expression::StringLiteral { kind, val } => Type::Reference {
                mutability: Mutability::Const,
                underlying: Box::new(match kind {
                    StrType::Byte => Type::Array(Box::new(Type::Integer(IntType::U8)), val.len()),
                    StrType::Default => Type::Str,
                }),
            },
        }
    }
}

impl Display for Expression {
    #[allow(clippy::cast_possible_wrap)] // u128 as i128, deliberate wrapping cast
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Cast { expr, target } => {
                write!(f, "{} as {}", expr, target)
            }
            Expression::FieldAccess { lhs, name, .. } => {
                write!(f, "{}.{}", lhs, name)
            }
            Expression::FunctionArg(id) => {
                write!(f, "<function arg {}>", id)
            }
            Expression::FunctionCall { func, args } => {
                write!(f, "{}(", func)?;
                let mut comma = false;
                for arg in args {
                    if comma {
                        write!(f, ", ")?;
                    }
                    arg.fmt(f)?;
                    comma = true;
                }
                write!(f, ")")
            }
            Expression::Identifier { id, .. } => id.fmt(f),
            Expression::IntegerLiteral { val, ty } => {
                if ty.unwrap().is_signed() {
                    (*val as i128).fmt(f)
                } else {
                    val.fmt(f)
                }
            }
            Expression::StringLiteral { kind, val } => {
                if *kind == StrType::Byte {
                    write!(f, "b")?;
                }
                write!(f, "\"{}\"", val)
            }
            Expression::StructInitializer { args, ty, .. } => {
                write!(f, "{} {{", ty)?;
                let mut comma = false;
                for (name, arg) in args {
                    if comma {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, arg)?;
                    comma = true;
                }
                write!(f, "}}")
            }
            Expression::UnsafeBlock { block, .. } => {
                writeln!(f, "unsafe {{")?;
                for statement in block {
                    writeln!(f, "{}", statement)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Statement {
    Bind {
        target: Identifier,
        value: Expression,
    },
    Discard(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Bind { target, value } => write!(f, "let {} = {};", target, value),
            Statement::Discard(expr) => write!(f, "{};", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Definition {
    Function {
        name: Identifier,
        return_ty: Type,
        body: Vec<Statement>,
    },
}

impl Definition {
    #[allow(dead_code)]
    #[must_use]
    pub fn name(&self) -> Identifier {
        let Definition::Function { name, .. } = self;
        name.clone()
    }
}

impl Display for Definition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Definition::Function {
            name,
            return_ty,
            body,
        } = self;
        writeln!(f, "fn {}() -> {} {{", name, return_ty)?;
        for statement in body {
            writeln!(f, "{}", statement)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[repr(u32)]
pub enum LangItem {
    Main,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub named_types: Vec<Type>,
    pub declarations: Vec<Declaration>,
    pub definitions: Vec<Definition>,
    pub lang_items: HashMap<LangItem, Identifier>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let declarations = self.declarations.iter().filter(|x| !x.has_definition());
        let mut c_declarations = Vec::new();
        let mut other_declarations = Vec::new();
        for declaration in declarations {
            if declaration.abi() == Abi::C {
                c_declarations.push(declaration);
            } else {
                other_declarations.push(declaration);
            }
        }
        if other_declarations.iter().any(|x| x.abi() != Abi::Rust) {
            writeln!(
                f,
                "// Warning: Some function ABIs may be incorrectly printed"
            )?;
        }
        if !c_declarations.is_empty() {
            writeln!(f, "extern \"C\" {{")?;
            for declaration in c_declarations {
                declaration.fmt(f)?;
            }
            writeln!(f, "}}")?;
        }
        if !other_declarations.is_empty() {
            writeln!(f, "extern \"Rust\" {{")?;
            for declaration in other_declarations {
                declaration.fmt(f)?;
            }
            writeln!(f, "}}")?;
        }
        for definition in &self.definitions {
            definition.fmt(f)?;
            writeln!(f)?;
        }
        for (item, id) in &self.lang_items {
            writeln!(f, "// Lang item {:?} = {}", item, id)?;
        }
        Ok(())
    }
}

pub fn resolve_named_type(named_types: &[Type], name: &Identifier) -> Type {
    if let Identifier::Basic { name, .. } = name {
        named_types
            .iter()
            .find(|x| x.name() == *name)
            .unwrap()
            .clone()
    } else {
        todo!();
    }
}

pub fn convert_id(id: &Path) -> Identifier {
    if id.root.is_some() {
        todo!()
    } else if id.components.is_empty() || id.components.len() > 1 {
        todo!()
    } else if let PathComponent::Id(name) = &id.components[0] {
        Identifier::Basic {
            mangling: None,
            name: name.clone(),
        }
    } else {
        todo!()
    }
}

pub fn convert_ty(named_types: &[Type], orig: &crate::parse::Type) -> Type {
    match orig {
        crate::parse::Type::Name(name) => resolve_named_type(named_types, &convert_id(&name)),
        crate::parse::Type::Pointer {
            mutability,
            underlying,
        } => Type::Pointer {
            mutability: *mutability,
            underlying: Box::new(convert_ty(named_types, &**underlying)),
        },
        crate::parse::Type::Never => Type::Never,
        crate::parse::Type::Tuple(_) => todo!("tuple"),
        crate::parse::Type::Wildcard => todo!("_"),
        crate::parse::Type::Reference { .. } => todo!("reference"),
        crate::parse::Type::Slice(_) => todo!("slice"),
        crate::parse::Type::Array(_, _) => todo!("array"),
    }
}

pub fn convert_expr(named_types: &[Type], orig: &crate::parse::Expr) -> Expression {
    match orig {
        crate::parse::Expr::Cast(expr, target) => Expression::Cast {
            expr: Box::new(convert_expr(named_types, expr)),
            target: convert_ty(named_types, target),
        },
        crate::parse::Expr::FunctionCall { func, args } => Expression::FunctionCall {
            func: Box::new(convert_expr(named_types, func)),
            args: args
                .iter()
                .map(|arg| convert_expr(named_types, arg))
                .collect(),
        },
        crate::parse::Expr::Id(id) => Expression::Identifier {
            id: convert_id(id),
            ty: None,
        },
        crate::parse::Expr::IntLiteral(n) => Expression::IntegerLiteral {
            val: *n as u128,
            ty: None,
        },
        crate::parse::Expr::Parentheses(inner) => convert_expr(named_types, inner),
        crate::parse::Expr::StringLiteral(kind, val) => Expression::StringLiteral {
            kind: *kind,
            val: val.clone(),
        },
        crate::parse::Expr::UnsafeBlock(inner) => Expression::UnsafeBlock {
            block: convert_block(named_types, inner),
            ty: None,
        },
        crate::parse::Expr::MacroExpansion { .. } => unreachable!(),
        crate::parse::Expr::StructConstructor(id, args) => Expression::StructInitializer {
            args: args
                .iter()
                .map(|arg| {
                    (
                        Identifier::Basic {
                            mangling: None,
                            name: match &arg.name {
                                FieldName::Id(str) => str.clone(),
                                FieldName::Tuple(id) => format!("${}", id),
                            },
                        },
                        convert_expr(named_types, &arg.expr),
                    )
                })
                .collect(),
            ty: resolve_named_type(named_types, &convert_id(id)),
        },
        crate::parse::Expr::Field(lhs, name) => Expression::FieldAccess {
            lhs: Box::new(convert_expr(named_types, lhs)),
            name: match name {
                FieldName::Id(str) => str.clone(),
                FieldName::Tuple(id) => format!("${}", id),
            },
            ty: None,
        },
        crate::parse::Expr::Await(_) => todo!("await"),
        crate::parse::Expr::Block(_) => todo!("block"),
        crate::parse::Expr::Loop(_) => todo!("loop"),
        crate::parse::Expr::While { .. } => todo!("while"),
        crate::parse::Expr::If { .. } => todo!("if"),
        crate::parse::Expr::LetExpr(_, _, _) => todo!("let"),
        crate::parse::Expr::Return(_) => todo!("return"),
        crate::parse::Expr::Break(_, _) => todo!("break"),
        crate::parse::Expr::Continue(_) => todo!("continue"),
        crate::parse::Expr::Yield(_) => todo!("yield"),
        crate::parse::Expr::BinaryOp(_, _, _) => todo!("binary op"),
        crate::parse::Expr::UnaryOp(_, _) => todo!("unary op"),
        crate::parse::Expr::ArrayIndex { .. } => todo!("array"),
        crate::parse::Expr::TypeAscription(_, _) => todo!("type ascription"),
        crate::parse::Expr::Try(_) => todo!("try"),
        crate::parse::Expr::CharLiteral(_, _) => todo!("char literal"),
    }
}

pub fn convert_block(named_types: &[Type], orig: &[crate::parse::BlockItem]) -> Vec<Statement> {
    let mut result = Vec::new();
    for statement in orig {
        match statement {
            crate::parse::BlockItem::Discard(expr) => {
                result.push(Statement::Discard(convert_expr(named_types, expr)));
            }
            crate::parse::BlockItem::Expr(expr) => {
                result.push(Statement::Expression(convert_expr(named_types, expr)));
            }
            crate::parse::BlockItem::Item(_) => {
                todo!("items in code blocks are currently unsupported");
            }
            crate::parse::BlockItem::Let { pattern, ty, value } => match (pattern, ty, value) {
                (Pattern::Discard, None, None) => {}
                (Pattern::Discard, None, Some(value)) => {
                    result.push(Statement::Discard(convert_expr(named_types, value)))
                }
                (Pattern::Ident(_), None, None) => todo!(),
                (Pattern::Ident(name), None, Some(value)) => {
                    result.push(Statement::Bind {
                        target: Identifier::Basic {
                            mangling: Some(Mangling::Local),
                            name: name.clone(),
                        },
                        value: convert_expr(named_types, value),
                    });
                }
                (pat, ty, expr) => todo!("({:?},{:?},{:?})", pat, ty, expr),
            },
            crate::parse::BlockItem::MacroExpansion { .. } => unreachable!(),
        }
    }
    result
}

fn iter_in_scope<F: FnMut(&Item, Option<&str>)>(items: &[Item], abi: Option<&str>, func: &mut F) {
    for item in items {
        match item {
            Item::ExternBlock {
                attrs,
                abi: new_abi,
                items,
            } => {
                for attr in attrs {
                    todo!("#[{:?}]", attr)
                }
                assert!(abi.is_none(), "`extern` blocks can't be nested");
                iter_in_scope(items, Some(new_abi.as_deref().unwrap_or("C")), func);
            }
            Item::FnDeclaration { .. } => func(item, abi),
            Item::MacroExpansion { .. } => unreachable!("Macros were already expanded"),
            Item::MacroRules { .. } => todo!("macro_rules!"),
            Item::Type(_) => {
                assert!(
                    abi.is_none(),
                    "`struct` is not supported in `extern` blocks"
                );
                func(item, abi);
            }
            Item::Mod { .. } => todo!("mod"),
            Item::Adt { .. } => todo!("enum"),
            Item::TypeAlias { .. } => todo!("type"),
            Item::Trait { .. } => todo!("trait"),
            Item::Impl { .. } => todo!("impl"),
            Item::Const { .. } => todo!("const"),
            Item::Static { .. } => todo!("static"),
        }
    }
}

#[allow(clippy::too_many_lines)] // TODO: refactor
#[allow(unused_mut)]
#[allow(unused_variables)]
pub fn convert(Mod { attrs, items }: &Mod) -> Program {
    for attr in attrs {
        todo!("#[{:?}]", attr)
    }
    // Pass 1: list types
    // Pass 1.1: primitives
    let mut named_types = vec![
        Type::Boolean,
        Type::Char,
        Type::Float(FloatType::F32),
        Type::Float(FloatType::F64),
        Type::Integer(IntType::I8),
        Type::Integer(IntType::I16),
        Type::Integer(IntType::I32),
        Type::Integer(IntType::I64),
        Type::Integer(IntType::I128),
        Type::Integer(IntType::Iptr),
        Type::Integer(IntType::Isize),
        Type::Integer(IntType::U8),
        Type::Integer(IntType::U16),
        Type::Integer(IntType::U32),
        Type::Integer(IntType::U64),
        Type::Integer(IntType::U128),
        Type::Integer(IntType::Uptr),
        Type::Integer(IntType::Usize),
        Type::Str,
    ];

    // Pass 1.2: structures
    iter_in_scope(items, None, &mut |item, abi| match item {
        Item::Type(structure) => {
            assert!(structure.attrs.is_empty());
            assert!(structure.tag == TypeTag::Struct);
            assert!(structure.generics.params.is_empty());
            named_types.push(Type::Struct {
                name: Identifier::Basic {
                    mangling: Some(Mangling::Rust),
                    name: structure.name.clone(),
                },
                fields: None,
            });
        }
        _ => {}
    });

    // Pass 1.3: fields of structures
    iter_in_scope(items, None, &mut |item, abi| match item {
        Item::Type(structure) => {
            let mut converted_fields = Vec::new();
            match &structure.body {
                StructBody::Struct(fields) => {
                    for field in fields {
                        assert!(field.attrs.is_empty());
                        converted_fields
                            .push((field.name.clone(), convert_ty(&named_types, &field.ty)));
                    }
                }
                StructBody::Tuple(_) => todo!(),
                StructBody::Unit => {}
            }
            if let Some(Type::Struct { fields, .. }) = named_types
                .iter_mut()
                .find(|ty| ty.name() == structure.name)
            {
                *fields = Some(converted_fields);
            } else {
                unreachable!();
            }
        }
        _ => {}
    });

    // Pass 2: list declarations and initially fill lang item table
    let mut declarations = Vec::new();
    let mut lang_items = HashMap::new();
    iter_in_scope(items, None, &mut |item, abi| {
        let declaration = match item {
            Item::FnDeclaration {
                attrs,
                visibility,
                is_const: _,
                is_async: _,
                safety,
                abi: _,
                generics: _,
                name,
                params,
                return_ty,
                block,
            } => {
                let name = Identifier::Basic {
                    name: name.clone(),
                    mangling: Some(if abi.is_some() {
                        Mangling::C
                    } else {
                        Mangling::Rust
                    }),
                };
                for attr in attrs {
                    match attr {
                        Meta::KeyValue(path, value) // For the record, deref patterns would make this a million times easier
                            if *path
                                == SimplePath {
                                    root: false,
                                    idents: vec![String::from("lang")],
                                }
                                && **value == Meta::String(String::from("main")) =>
                        {
                            lang_items.insert(LangItem::Main, name.clone());
                        }
                        x => todo!("#[{:?}]", attr),
                    }
                }
                Some(Declaration::Function {
                    has_definition: block.is_some(),
                    name,
                    sig: FunctionSignature {
                        abi: abi.map_or(Abi::Rust, |x| match x {
                            "C" => Abi::C,
                            "Rust" => Abi::Rust,
                            _ => todo!(),
                        }),
                        params: params
                            .iter()
                            .map(|FnParam { ty, .. }| {
                                convert_ty(&named_types, ty.as_ref().unwrap())
                            })
                            .collect(),
                        return_ty: Box::new(return_ty.as_ref().map_or_else(
                            || Type::Tuple(Vec::new()),
                            |ty| convert_ty(&named_types, ty),
                        )),
                        safety: if abi.is_some() {
                            Safety::Unsafe
                        } else {
                            *safety
                        },
                        visibility: *visibility,
                    },
                })
            }
            Item::ExternBlock { .. } => {
                unreachable!("Should have been descended into by calling function");
            }
            Item::MacroExpansion { .. } => unreachable!("Macros should already be expanded"),
            Item::MacroRules { .. } => unreachable!("macro_rules!"),
            Item::Type(_) => None,
            Item::Mod { .. } => todo!("mod"),
            Item::Adt { .. } => todo!("enum"),
            Item::TypeAlias { .. } => todo!("type"),
            Item::Trait { .. } => todo!("trait"),
            Item::Impl { .. } => todo!("impl"),
            Item::Static { .. } => todo!("static"),
            Item::Const { .. } => todo!("const"),
        };
        if let Some(declaration) = declaration {
            // TODO: Check attributes
            declarations.push(declaration);
        }
    });
    if let Entry::Vacant(entry) = lang_items.entry(LangItem::Main) {
        // Pass 2.5: locate `main`, if it exists
        for declaration in &declarations {
            if declaration.name().matches(&Identifier::Basic {
                mangling: None,
                name: String::from("main"),
            }) {
                entry.insert(declaration.name().clone());
                break;
            }
        }
    }
    // Pass 3: convert functions
    let mut definitions = Vec::new();
    iter_in_scope(items, None, &mut |item, abi| {
        if let Item::FnDeclaration {
            safety,
            name,
            params,
            return_ty,
            block: Some(block),
            ..
        } = item
        {
            let mut body = Vec::new();
            for param in params {
                todo!(); // TODO: Support functions with parameters
            }
            body.append(&mut convert_block(&named_types, block));
            if *safety == Safety::Unsafe {
                body = vec![Statement::Expression(Expression::UnsafeBlock {
                    block: body,
                    ty: None,
                })];
            }
            definitions.push(Definition::Function {
                name: Identifier::Basic {
                    name: name.clone(),
                    mangling: Some(if abi.is_some() {
                        Mangling::C
                    } else {
                        Mangling::Rust
                    }),
                },
                return_ty: return_ty
                    .as_ref()
                    .map_or_else(|| Type::Tuple(Vec::new()), |x| convert_ty(&named_types, x)),
                body,
            });
        }
    });
    Program {
        named_types,
        declarations,
        definitions,
        lang_items,
    }
}

#[allow(unused_variables, clippy::option_if_let_else)]
fn typeck_expr(
    declarations: &[Declaration],
    expr: &mut Expression,
    safety: Safety,
    ty: Option<&Type>,
    return_ty: Option<&Type>,
) -> Type {
    match expr {
        Expression::Cast { expr, target } => {
            let expr_ty = typeck_expr(declarations, expr, safety, None, return_ty);
            match (expr_ty.clone(), &target) {
                (
                    Type::Reference {
                        mutability: m1,
                        underlying: ty1,
                    },
                    Type::Pointer {
                        mutability: m2,
                        underlying: ty2,
                    },
                ) if (m1 == Mutability::Mut || *m2 == Mutability::Const) => match *ty1 {
                    // Needed b/c no deref patterns
                    Type::Array(ty1, _) if ty1 == *ty2 => target.clone(),
                    ty1 if ty1 == **ty2 => target.clone(),
                    _ => panic!("can not cast from {:?} to {:?}", expr_ty, target),
                },
                (x, y) => todo!("{:?} to {:?}", x, y),
            }
        }
        Expression::FieldAccess {
            lhs,
            name,
            ty: field_ty,
        } => {
            let lhs_ty = typeck_expr(declarations, lhs, safety, None, return_ty);
            if let Type::Struct { fields, .. } = &lhs_ty {
                if let Some((_, ty)) = fields
                    .as_ref()
                    .map_or(None, |x| x.iter().find(|x| x.0 == *name))
                {
                    *field_ty = Some(ty.clone());
                    ty.clone()
                } else {
                    panic!("type {:?} doesn't have a field named {}", &lhs_ty, name);
                }
            } else {
                todo!();
            }
        }
        Expression::FunctionCall { func, args } => {
            let func_ty = typeck_expr(declarations, func, safety, None, return_ty);
            if let Type::Function(func_sig) = func_ty {
                assert!(
                    func_sig.safety == Safety::Safe || safety == Safety::Unsafe,
                    "unsafe function called in safe context"
                );
                assert!(
                    func_sig.params.len() == args.len(),
                    "provided {} args to a function expecting {}",
                    args.len(),
                    func_sig.params.len()
                );
                for (arg, param) in args.iter_mut().zip(func_sig.params.iter()) {
                    typeck_expr(declarations, arg, safety, Some(param), return_ty);
                }
                if let Some(ty) = ty {
                    assert!(
                        *func_sig.return_ty == *ty,
                        "expected {:?}, got {:?}",
                        ty,
                        func_sig.return_ty
                    );
                }
                *func_sig.return_ty
            } else {
                panic!("tried to call a {:?} as a function", func_ty)
            }
        }
        Expression::Identifier { id, ty: id_ty } => {
            let mut result = None;
            for decl in declarations {
                if decl.name().matches(id) {
                    *id = decl.name().clone();
                    result = Some(decl.ty());
                }
            }
            result.map_or_else(
                || panic!("could not resolve identifier {:?}", id),
                |result| {
                    *id_ty = Some(result.clone());
                    result
                },
            )
        }
        Expression::IntegerLiteral { val, ty: int_ty } => {
            if let Some(ty) = ty {
                if let Some(int_ty) = int_ty {
                    assert!(
                        Type::Integer(*int_ty) == *ty,
                        "tried to infer an already-inferred integer as a different type"
                    );
                } else if let Type::Integer(ty) = ty {
                    *int_ty = Some(*ty);
                } else {
                    panic!("attempt to infer integer as non-integer type");
                }
            } else if *val <= 0xFF {
                *int_ty = Some(IntType::U8);
            } else if *val <= 0xFFFF {
                *int_ty = Some(IntType::U16);
            } else if *val <= 0xFFFFFFFF {
                *int_ty = Some(IntType::U32);
            } else if *val <= 0xFFFFFFFFFFFFFFFF {
                *int_ty = Some(IntType::U64);
            } else {
                *int_ty = Some(IntType::U128);
            }
            Type::Integer(int_ty.unwrap())
        }
        Expression::StringLiteral { kind, val } => Type::Reference {
            mutability: Mutability::Const,
            underlying: Box::new(match kind {
                StrType::Byte => Type::Array(Box::new(Type::Integer(IntType::U8)), val.len()),
                StrType::Default => Type::Str,
            }),
        },
        Expression::StructInitializer { args, ty } => {
            // TODO: check if correct struct fields were initialized.
            // For now, we just run typeck on each arg so that irgen knows the types.
            for (_, arg) in args {
                typeck_expr(declarations, arg, safety, None, return_ty);
            }
            ty.clone()
        }
        Expression::UnsafeBlock {
            block,
            ty: block_ty,
        } => {
            if safety == Safety::Unsafe {
                println!("warning: unsafe block used already unsafe section");
            }
            let new_ty = typeck_block(declarations, block, Safety::Unsafe, ty, return_ty);
            *block_ty = Some(new_ty.clone());
            new_ty
        }
        x => todo!("{:?}", x),
    }
}

fn typeck_statement(
    declarations: &mut Vec<Declaration>,
    statement: &mut Statement,
    safety: Safety,
    ty: Option<&Type>,
    return_ty: Option<&Type>,
) -> Type {
    match statement {
        Statement::Bind { target, value } => {
            let result_ty = typeck_expr(declarations, value, safety, None, return_ty);
            declarations.push(Declaration::Local {
                name: target.clone(),
                ty: result_ty,
            });
            Type::Tuple(Vec::new())
        }
        Statement::Discard(expr) => {
            typeck_expr(declarations, expr, safety, None, return_ty);
            Type::Tuple(Vec::new())
        }
        Statement::Expression(expr) => {
            let result_ty = typeck_expr(declarations, expr, safety, ty, return_ty);
            if let Some(ty) = ty {
                assert!(result_ty == *ty, "expected {:?}, got {:?}", ty, result_ty);
            }
            result_ty
        }
    }
}

fn typeck_block(
    declarations: &[Declaration],
    block: &mut [Statement],
    safety: Safety,
    ty: Option<&Type>,
    return_ty: Option<&Type>,
) -> Type {
    let mut declarations = declarations.iter().map(Declaration::clone).collect();
    let result_ty = if block.is_empty() {
        Type::Tuple(Vec::new())
    } else {
        let (result, bulk) = block.split_last_mut().unwrap(); // Will not panic because we verified that the block is not empty
        for statement in bulk {
            let result = typeck_statement(
                &mut declarations,
                statement,
                safety,
                Some(&Type::Tuple(Vec::new())),
                return_ty,
            );
            assert!(
                result == Type::Tuple(Vec::new()),
                "expected (), got {:?}",
                result
            );
        }
        typeck_statement(&mut declarations, result, safety, ty, return_ty)
    };
    if let Some(ty) = ty {
        assert!(result_ty == *ty, "expected {:?}, got {:?}", ty, result_ty);
    }
    result_ty
}

fn typeck_definition(declarations: &[Declaration], definition: &mut Definition) {
    let Definition::Function {
        return_ty,
        ref mut body,
        ..
    } = definition;
    typeck_block(
        declarations,
        body,
        Safety::Safe,
        Some(return_ty),
        Some(return_ty),
    );
}

pub fn typeck_program(program: &mut Program) {
    for definition in &mut program.definitions {
        typeck_definition(&program.declarations, definition);
    }
}
