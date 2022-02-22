pub use crate::lex::StrType; // TODO: `pub use` this in `crate::parse`
use crate::parse::{FnParam, Item};
pub use crate::parse::{Mutability, Safety, Visibility};
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Identifier {
    Basic {
        mangling: Option<Mangling>,
        name: String,
    },
}

impl Identifier {
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
pub enum Type {
    Array(Box<Self>, usize),
    Boolean,
    Char,
    Float(FloatType),
    Function(FunctionSignature),
    Integer(IntType),
    Pointer {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Reference {
        mutability: Mutability,
        underlying: Box<Self>,
    },
    Str,
    Tuple(Vec<Self>),
}

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
}

impl Declaration {
    #[must_use]
    pub const fn abi(&self) -> Abi {
        let Declaration::Function {
            sig: FunctionSignature { abi, .. },
            ..
        } = self;
        *abi
    }

    #[must_use]
    pub const fn has_definition(&self) -> bool {
        let Declaration::Function { has_definition, .. } = self;
        *has_definition
    }

    #[must_use]
    pub const fn name(&self) -> &Identifier {
        let Declaration::Function { name, .. } = self;
        name
    }

    #[must_use]
    pub fn ty(&self) -> Type {
        let Declaration::Function { sig, .. } = self;
        Type::Function(sig.clone())
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Declaration::Function { name, sig, .. } = self;
        writeln!(f, "fn {}{};", name, sig)
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expression {
    Cast {
        expr: Box<Expression>,
        target: Type,
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
    StringLiteral {
        kind: StrType,
        val: String,
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
            Expression::Cast { target, .. } => target.clone(),
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
            Expression::Identifier { ty, .. } | Expression::UnsafeBlock { ty, .. } => ty
                .as_ref()
                .expect("typeck forgot to resolve type of expression; this is an ICE")
                .clone(),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Cast { expr, target } => {
                write!(f, "{} as {}", expr, target)
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
            Expression::StringLiteral { kind, val } => {
                if *kind == StrType::Byte {
                    write!(f, "b")?;
                }
                write!(f, "\"{}\"", val)
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
    Bind { target: String, value: Expression },
    Discard(Expression),
    Expression(Expression),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Bind { .. } => todo!(),
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

#[derive(Clone, Debug, Hash)]
pub struct Program {
    pub named_types: Vec<Type>,
    pub declarations: Vec<Declaration>,
    pub definitions: Vec<Definition>,
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
        Ok(())
    }
}

pub fn convert_ty(named_types: &[Type], orig: &crate::parse::Type) -> Type {
    match orig {
        crate::parse::Type::Name(name) => {
            for ty in named_types {
                if *name == ty.name() {
                    return ty.clone();
                }
            }
            panic!("Unknown type {}", name);
        }
        crate::parse::Type::Pointer {
            mutability,
            underlying,
        } => Type::Pointer {
            mutability: *mutability,
            underlying: Box::new(convert_ty(named_types, &**underlying)),
        },
    }
}

#[allow(unused_variables)]
pub fn convert_expr(named_types: &[Type], orig: &crate::parse::Expr) -> Expression {
    match orig {
        crate::parse::Expr::Cast(expr, target) => Expression::Cast {
            expr: Box::new(convert_expr(named_types, expr)),
            target: convert_ty(named_types, target),
        },
        crate::parse::Expr::FunctionCall {
            func,
            params, /* TODO: params is erroneously named */
        } => Expression::FunctionCall {
            func: Box::new(convert_expr(named_types, func)),
            args: params
                .iter()
                .map(|arg| convert_expr(named_types, arg))
                .collect(),
        },
        crate::parse::Expr::Id(id) => Expression::Identifier {
            id: Identifier::Basic {
                mangling: None,
                name: id.clone(),
            },
            ty: None,
        },
        crate::parse::Expr::Parentheses(inner) => convert_expr(named_types, inner), // I assume this only exists for lints
        crate::parse::Expr::StringLiteral(kind, val) => Expression::StringLiteral {
            kind: *kind,
            val: val.clone(),
        },
        crate::parse::Expr::UnsafeBlock(inner) => Expression::UnsafeBlock {
            block: convert_block(named_types, inner),
            ty: None,
        },
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
        }
    }
    result
}

fn iter_in_scope<F: FnMut(&Item, Option<&str>)>(items: &[Item], abi: Option<&str>, func: &mut F) {
    for item in items {
        match item {
            Item::ExternBlock {
                abi: new_abi,
                items,
            } => {
                assert!(abi.is_none(), "`extern` blocks can't be nested");
                iter_in_scope(items, Some(new_abi.as_deref().unwrap_or("C")), func);
            }
            Item::FnDeclaration { .. } => func(item, abi),
        }
    }
}

#[allow(clippy::too_many_lines)] // TODO: refactor
#[allow(unused_mut)]
#[allow(unused_variables)]
pub fn convert(items: &[Item]) -> Program {
    // Pass 1: list types
    // Since we don't have a prelude to give us standard types yet, we list them
    // inline.
    let named_types = vec![
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
    // Pass 2: list declarations
    let mut declarations = Vec::new();
    iter_in_scope(items, None, &mut |item, abi| match item {
        Item::FnDeclaration {
            visibility,
            safety,
            name,
            params,
            return_ty,
            block,
        } => {
            declarations.push(Declaration::Function {
                has_definition: block.is_some(),
                name: Identifier::Basic {
                    name: name.clone(),
                    mangling: Some(if abi.is_some() {
                        Mangling::Rust
                    } else {
                        Mangling::C
                    }),
                },
                sig: FunctionSignature {
                    abi: abi.map_or(Abi::Rust, |x| match x {
                        "C" => Abi::C,
                        "Rust" => Abi::Rust,
                        _ => todo!(),
                    }),
                    params: params
                        .iter()
                        .map(|FnParam { ty, .. }| convert_ty(&named_types, ty))
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
            });
        }
        Item::ExternBlock { .. } => {
            unreachable!("Should have been descended into by calling function");
        }
    });
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
                        Mangling::Rust
                    } else {
                        Mangling::C
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
    }
}

#[allow(unused_variables)]
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
        Expression::StringLiteral { kind, val } => Type::Reference {
            mutability: Mutability::Const,
            underlying: Box::new(match kind {
                StrType::Byte => Type::Array(Box::new(Type::Integer(IntType::U8)), val.len()),
                StrType::Default => Type::Str,
            }),
        },
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
        x @ Expression::FunctionArg(_) => todo!("{:?}", x),
    }
}

fn typeck_statement(
    declarations: &[Declaration],
    statement: &mut Statement,
    safety: Safety,
    ty: Option<&Type>,
    return_ty: Option<&Type>,
) -> Type {
    match statement {
        Statement::Bind { .. } => todo!(),
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
    let result_ty = if block.is_empty() {
        Type::Tuple(Vec::new())
    } else {
        let (result, bulk) = block.split_last_mut().unwrap(); // Will not panic because we verified that the block is not empty
        for statement in bulk {
            let result = typeck_statement(
                declarations,
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
        typeck_statement(declarations, result, safety, ty, return_ty)
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
