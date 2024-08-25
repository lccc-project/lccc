use std::ops::Index;

use super::{cx, ty, DefId};

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct ParamId(pub(crate) u32);

impl ParamId {
    #[doc(hidden)]
    pub const fn __new_unchecked(val: u32) -> Self {
        Self(val)
    }
}

impl core::fmt::Display for ParamId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%{}", self))
    }
}

impl core::fmt::Debug for ParamId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("%{}", self))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArg {
    Const(cx::ConstExpr),
    Type(ty::Type),
    Lifetime(ty::SemaLifetime),
    Wildcard, // Fill this in later
}

impl GenericArg {
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        match self {
            Self::Const(cx) => Self::Const(cx.substitute_generics(args)),
            Self::Type(ty) => Self::Type(ty.substitute_generics(args)),
            Self::Lifetime(life) => Self::Lifetime(life.substitute_generics(args)),
            Self::Wildcard => panic!("Wildcard left too late"),
        }
    }
}

impl core::fmt::Display for GenericArg {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Const(c) => c.fmt(f),
            Self::Type(ty) => ty.fmt(f),
            Self::Lifetime(life) => life.fmt(f),
            Self::Wildcard => f.write_str("_"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct GenericArgs {
    pub associated_types: Vec<(DefId, ty::Type)>,
    pub trait_self: Option<Box<ty::Type>>,
    pub params: Vec<GenericArg>,
}

impl Index<ParamId> for GenericArgs {
    type Output = GenericArg;
    fn index(&self, id: ParamId) -> &GenericArg {
        &self.params[id.0 as usize]
    }
}

impl GenericArgs {
    pub fn get(&self, id: ParamId) -> Option<&GenericArg> {
        self.params.get(id.0 as usize)
    }
    pub fn substitute_generics(&self, args: &GenericArgs) -> Self {
        Self {
            associated_types: Vec::new(),
            trait_self: self
                .trait_self
                .as_ref()
                .map(|trait_self| Box::new(trait_self.substitute_generics(args))),
            params: self
                .params
                .iter()
                .map(|arg| arg.substitute_generics(args))
                .collect(),
        }
    }
}

impl core::fmt::Display for GenericArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";
        f.write_str("<")?;

        if let Some(trait_self) = &self.trait_self {
            sep = ", ";
            f.write_str("Self = ")?;
            trait_self.fmt(f)?;
        }

        for (assoc, ty) in &self.associated_types {
            f.write_str(sep)?;
            sep = ", ";
            f.write_fmt(format_args!("{} = {}", assoc, ty))?;
        }

        for param in &self.params {
            f.write_str(sep)?;
            sep = ", ";
            param.fmt(f)?;
        }

        f.write_str(">")
    }
}
