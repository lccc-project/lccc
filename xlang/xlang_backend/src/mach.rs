use mce::MceWriter;
use xlang::{
    abi::collection::HashMap, abi::string::StringView, ir, targets::properties::TargetProperties,
    vec::Vec,
};

use crate::{mangle::mangle_itanium, ssa::OpaqueLocation, ty::TypeInformation};

/// A Code Generator that supports Machine-code level Optimizations and Legalizing Transformation
pub mod mce;

/// A trait for representing the properties of a machine, and for generating code for the machine from `R` (a type of instruction)
pub trait Machine<R>: MceWriter {
    /// The type used per-function to track where each opaque location is placed.
    type Assignments;
    /// The type used per-basic block to track where values need to be spilled or moved to a new assignment
    type BlockClobbers;

    /// Determines whether the given named `targ` matches the current [`Machine`]
    fn matches_target(&self, targ: StringView) -> bool;
    /// Initializes the machine from the properties specified in `targ`.
    /// It may be assumed by the implementation that all functions, other than [`Machine::matches_target`], are called only after calling this function and that this function is called at most one.
    fn init_from_target(&mut self, targ: &TargetProperties);
    /// Constructs a new (empty) state for [`Machine::Assignments`].
    /// A single return value is used for only one XIR function, and is not reused by multiple functions.
    fn new_assignments(&self) -> Self::Assignments;

    /// Assigns locations for incoming locations from the calling convention of the function
    fn assign_call_conv(
        &self,
        assignments: &mut Self::Assignments,
        incoming: &[OpaqueLocation],
        fnty: &ir::FnType,
        tys: &TypeInformation,
        which: u32,
    );

    /// Assigns the locations for a basic block, specifying the instructions the make up the lowered basic block, and the list of locations incoming to the basic block.
    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[R],
        incoming: &[OpaqueLocation],
        which: u32,
        incoming_set: &HashMap<u32, Vec<OpaqueLocation>>,
        tys: &TypeInformation,
    ) -> Self::BlockClobbers;
    /// Writes the prologue instructions for a function to the given [`InsnWrite`]
    fn codegen_prologue(
        &self,
        assignments: &Self::Assignments,
    ) -> Vec<mce::MceInstruction<Self::Instruction>>;
    /// Writes the machine instructions corresponding to the [`SsaInstruction`]s that make up a basic block.
    ///
    /// The function may generate a label string from it's basic block id by using the `label_sym` callback.
    fn codegen_block<F: Fn(u32) -> String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[R],
        block_clobbers: Self::BlockClobbers,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> Vec<mce::MceInstruction<Self::Instruction>>;

    /// Mangles the components of a complex XIR [`Path`][xlang::ir::Path]
    /// (Does not solely consistent of an optional [`Root`][xlang::ir::PathComponent::Root] followed by a single bare [`Text`][xlang::ir::PathComponent::Text])
    fn mangle(&self, path: &[xlang::ir::PathComponent]) -> String {
        mangle_itanium(path)
    }
}
