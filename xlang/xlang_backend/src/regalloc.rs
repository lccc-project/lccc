use core::hash::Hash;

use xlang::abi::{collection::HashMap, pair::Pair, vec::Vec};

/// A Trait for groups of registers on a platform
pub trait RegGroup: Hash + Eq + Clone {
    /// The type of Register
    type Register: Hash + Eq + Clone;
    /// All Registers that belong to the group
    fn registers(&self) -> impl IntoIterator<Item = Self::Register> + '_;
}

/// Trait for codegen specific assignment types
pub trait Assignment {
    /// The type of Register the assignment can belong to
    type Register;

    /// The register the assignment currently occupies, if any
    fn current_owned_register(&self) -> Option<Self::Register>;
    /// Indicates that the assignment should move to `reg` at the specified mc instruction
    fn move_to_register(&mut self, reg: Self::Register, at: usize);
    /// Indicates that the assignment should move to memory at the specified mc instruction.
    fn move_to_memory(&mut self, at: usize);
}

/// A Generalized Register allocator which can be used as [`Machine::BlockClobbers`][crate::mach::Machine::BlockClobbers]
#[derive(Clone, Debug)]
pub struct RegisterAllocator<R, G> {
    inverse_reg_map: HashMap<R, G>,
    reg_groups_lru: HashMap<G, Vec<R>>,
    reg_owners: HashMap<R, u32>,
    clobbered_at: HashMap<u32, usize>,
    saved_regs: HashMap<R, bool>,
}

impl<R, G> RegisterAllocator<R, G> {
    /// Constructs a new [`RegisterAllocator`] in an empty state.
    pub fn new() -> Self {
        Self {
            reg_groups_lru: HashMap::new(),
            reg_owners: HashMap::new(),
            clobbered_at: HashMap::new(),
            inverse_reg_map: HashMap::new(),
            saved_regs: HashMap::new(),
        }
    }
}

impl<R, G> Default for RegisterAllocator<R, G> {
    fn default() -> Self {
        Self::new()
    }
}

impl<G: RegGroup> RegisterAllocator<G::Register, G> {
    /// Constructs a new [`RegisterAllocator`] that is initialized to use each of the register `groups` specified.
    ///
    /// This is a convience over constructing with [`RegisterAllocator::new`] followed by calling [`RegisterAllocator::add_groups`].
    pub fn from_groups<I: IntoIterator<Item = G>>(groups: I) -> Self {
        let mut ret = Self::new();
        ret.add_groups(groups);
        ret
    }

    /// Indicates that the following registers must be saved by the calling covnention.
    pub fn save_registers<I: IntoIterator<Item = G::Register>>(&mut self, regs: I) {
        self.saved_regs.extend(regs.into_iter().map(|x| (x, false)))
    }

    /// Lists the calling-convention saved registers that have been clobbered by the block
    pub fn clobbered_registers(&self) -> impl IntoIterator<Item = G::Register> + '_ {
        self.saved_regs
            .iter()
            .filter_map(|Pair(v, x)| if *x { Some(v) } else { None })
            .cloned()
    }

    /// Adds the specified group `regs` to list of available registers.
    pub fn add_group(&mut self, regs: G) {
        let values = regs.registers().into_iter().collect::<Vec<_>>();
        for v in &values {
            self.inverse_reg_map.insert(v.clone(), regs.clone());
        }
        self.reg_groups_lru.insert(regs, values);
    }
    /// Adds each group in `groups` to the list of available registers.
    /// This is equivalent to repeatedly calling [`RegisterAllocator::add_group`] on each element yielded by `groups`.
    pub fn add_groups<I: IntoIterator<Item = G>>(&mut self, groups: I) {
        for group in groups {
            self.add_group(group);
        }
    }

    fn reg_used(&mut self, reg: &G::Register) {
        let group = &self.inverse_reg_map[reg];

        let cache = self.reg_groups_lru.get_mut(group).unwrap();

        for (i, r) in cache.iter().enumerate() {
            if reg == r {
                let val = cache.remove(i);
                cache.push(val);
                break;
            }
        }
    }

    /// Allocates a register that belongs to the specified `group`.
    ///
    /// ## Panics
    ///
    /// Panics if `group` has not been added via [`RegisterAllocator::add_group`].
    pub fn alloc_reg(&mut self, group: &G, at: usize) -> G::Register {
        let cache = self.reg_groups_lru.get_mut(group).unwrap();

        let val = cache.remove(0);
        cache.push(val.clone());
        self.mark_clobbered(val.clone(), None, at);
        val
    }

    /// Marks that `val` starts owning register `reg` at the specified mc instruction.
    ///
    /// ## Panics
    ///
    /// Panics if `reg` does not belong to any register group previously added by [`RegisterAllocator::add_group`].
    pub fn mark_owning(&mut self, val: u32, reg: G::Register, at: usize) {
        self.mark_clobbered(reg.clone(), Some(val), at);
        self.reg_used(&reg);
        self.reg_owners.insert(reg, val);
    }

    /// Marks that `reg` has been clobbered at the specified mc instruction.
    ///
    /// `for_val` is either `None` or the [`OpaqueLocation`][crate::ssa::OpaqueLocation] number of the appropriate value.
    ///
    /// ## Panics
    ///
    /// Panics if `reg` does not belong to any register group previously added by [`RegisterAllocator::add_group`].
    pub fn mark_clobbered(&mut self, reg: G::Register, for_val: Option<u32>, at: usize) {
        if let Some(v) = self.saved_regs.get_mut(&reg) {
            *v = true;
        }
        if let Some(Pair(_, val)) = self.reg_owners.remove(&reg) {
            if for_val == Some(val) {
                self.reg_owners.insert(reg, val);
            } else {
                self.clobbered_at.insert(val, at);
            }
        }
    }

    /// Marks the specified `val` as used, spilling it to memory if necessary.
    pub fn mark_used<A: Assignment>(&mut self, val: u32, assign: &mut A) {
        if let Some(Pair(_, at)) = self.clobbered_at.remove(&val) {
            assign.move_to_memory(at);
        }
    }
}

/// A convience alias for [`RegisterAllocator`] that only specifies the register group.
pub type RegAllocClobbers<G> = RegisterAllocator<<G as RegGroup>::Register, G>;
