use xlang::{
    abi::collection::HashMap, abi::string::StringView, targets::properties::TargetProperties,
    vec::Vec,
};

use crate::{
    mangle::mangle_itanium,
    ssa::{OpaqueLocation, SsaInstruction},
    ty::TypeInformation,
};

use arch_ops::traits::InsnWrite;

use std::io::Result;

pub trait Machine {
    type Assignments;
    type BlockClobbers;
    fn matches_target(&self, targ: StringView) -> bool;
    fn init_from_target(&mut self, targ: &TargetProperties);
    fn new_assignments(&self) -> Self::Assignments;
    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[SsaInstruction],
        incoming: &[OpaqueLocation],
        which: u32,
        incoming_set: &HashMap<u32, Vec<OpaqueLocation>>,
        tys: &TypeInformation,
    ) -> Self::BlockClobbers;
    fn codegen_prologue<W: InsnWrite>(
        &self,
        assignments: &Self::Assignments,
        out: &mut W,
    ) -> std::io::Result<()>;
    fn codegen_block<W: InsnWrite, F: Fn(u32) -> String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        out: &mut W,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> std::io::Result<()>;
    fn mangle(&self, path: &[xlang::ir::PathComponent]) -> String {
        mangle_itanium(path)
    }
}
