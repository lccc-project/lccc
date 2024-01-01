use core::hash::Hash;
use std::num::NonZeroU64;

/// A set of registers available for use by regalloc
pub trait RegSet {
    /// The register type to use
    type Register: Clone + PartialEq + Hash;

    /// All of the registers in the set
    fn all_registers(&self) -> &[Self::Register];

    /// The maximum size of values that can be stored in the register
    fn max_size(&self) -> u64;

    /// The maximum alignment of values that can be stored in the register.
    ///
    /// Typically, this is the same as [`RegSet::max_size`], and [`None`] means this value.
    /// This may be smaller if the register is a synthetic one placed in memory and does not have the required alignment.
    ///
    /// Returns a power of 2 less than [`RegSet::max_size`]
    fn max_align(&self) -> Option<NonZeroU64>;
}
