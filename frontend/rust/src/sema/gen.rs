use super::{generics::GenericArgs, ty::Type, DefId, FunctionBody, Mutability};

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CaptureMode {
    MoveByVal,
    Ref(Mutability),
    CopyOrRef(Mutability),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AutoTraits {
    Inherit,
    ForcePin,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AnonType {
    pub auto_traits: AutoTraits,
    pub def_trait: DefId,
    pub def_trait_generics: GenericArgs,
    pub late_bound_lifetime_count: u32,
    pub body: AnonTypeBody,
}


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnonTypeBody{
    SimpleClosure(FunctionBody),
    
}
