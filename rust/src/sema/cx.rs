use xlang::{abi::pair::Pair, prelude::v1::HashMap};

use super::{
    generics::{GenericArg, GenericArgs, ParamId},
    hir::HirExpr,
    mir::MirFunctionBody,
    ty::{FieldName, IntType},
    DefId, Spanned,
};

pub mod eval;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstExpr {
    #[allow(dead_code)]
    HirVal(Box<HirExpr>),
    MirVal(Box<MirFunctionBody>),
    IntConst(IntType, u128),
    Const(DefId, GenericArgs),
    Param(ParamId),
    Constructor(ConstExprConstructor),
}

impl core::fmt::Display for ConstExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ConstExpr::HirVal(val) => f.write_fmt(format_args!("const {{ {} }}", val)),
            ConstExpr::MirVal(val) => f.write_fmt(format_args!("/* mir const */")),
            ConstExpr::IntConst(ity, val) => f.write_fmt(format_args!("{}_{}", val, ity)),
            ConstExpr::Const(def, generics) => f.write_fmt(format_args!("")),
            ConstExpr::Param(par) => par.fmt(f),
            ConstExpr::Constructor(ctor) => ctor.fmt(f),
        }
    }
}

impl ConstExpr {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        match self {
            Self::HirVal(val) => panic!("Expand HIR Vals before substituting"),
            Self::MirVal(body) => Self::MirVal(Box::new(body.substitute_generics(args))),
            Self::IntConst(ity, val) => Self::IntConst(*ity, *val),
            Self::Const(defid, generics) => Self::Const(*defid, generics.substitute_generics(args)),
            Self::Param(id) => match args.get(*id) {
                Some(GenericArg::Const(cx)) => cx.clone(),
                _ => panic!("Expected a const for {id}"),
            },
            Self::Constructor(ctor) => {
                let ctor_id = ctor.ctor_id;
                let generics = ctor.generics.substitute_generics(args);
                let fields = ctor
                    .fields
                    .iter()
                    .map(|Pair(field, cx)| {
                        (*field, cx.copy_span(|cx| cx.substitute_generics(args)))
                    })
                    .collect();

                Self::Constructor(ConstExprConstructor {
                    ctor_id,
                    generics,
                    fields,
                })
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConstExprConstructor {
    pub ctor_id: DefId,
    pub generics: GenericArgs,
    pub fields: HashMap<FieldName, Spanned<ConstExpr>>,
}

impl core::fmt::Display for ConstExprConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ctor_id.fmt(f)?;
        self.generics.fmt(f)?;
        f.write_str("{")?;

        let mut sep = "";

        for Pair(k, v) in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            k.fmt(f)?;
            f.write_str(": ")?;
            v.body.fmt(f)?;
        }
        f.write_str("}")
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstEvalError {
    EvaluatorError,
}
