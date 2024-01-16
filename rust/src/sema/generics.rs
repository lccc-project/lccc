use super::{cx, ty, DefId};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArg {
    Const(cx::ConstExpr),
    Type(ty::Type),
    Lifetime(ty::SemaLifetime),
    Wildcard, // Fill this in later
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
    pub params: Vec<GenericArg>,
}

impl core::fmt::Display for GenericArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";
        f.write_str("<")?;

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
