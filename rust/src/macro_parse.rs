#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct MacroDefId(u64);

impl core::fmt::Debug for MacroDefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0))
    }
}

impl core::fmt::Display for MacroDefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0))
    }
}

pub struct Macros {
    nextdefid: MacroDefId,
}

pub mod decl;
pub mod proc;
