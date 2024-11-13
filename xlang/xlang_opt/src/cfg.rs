use core::option::Option::Some;
use xlang::abi::slice::SplitOut as _;
use xlang::ir::{meta::block::ReachableFrom, FunctionBody, Terminator};
use xlang::ir::{BranchCondition, Switch};
use xlang::prelude::v1::*;

pub struct ControlFlow<'a> {
    body: &'a mut FunctionBody,
}

impl<'a> ControlFlow<'a> {
    fn can_branch_to(t: &Terminator, target: u32) -> bool {
        match t {
            Terminator::BranchIndirect => false, // Handle this otherwise
            Terminator::Asm(a) => a.targets.iter().any(|targ| targ.target == target),
            Terminator::Branch(BranchCondition::Always, a, _) => a.target == target,
            Terminator::Branch(BranchCondition::Never, _, b) => b.target == target,
            Terminator::Branch(_, a, b) => a.target == target || b.target == target,
            Terminator::Jump(targ) => targ.target == target,
            Terminator::Switch(switch) => {
                switch.default().target == target
                    || switch.case_targets().any(|targ| targ.target == target)
            }
            Terminator::Call(_, _, next) => next.target == target,
            Terminator::Empty
            | Terminator::Unreachable
            | Terminator::Exit(_)
            | Terminator::Tailcall(_, _) => false,
        }
    }
    pub fn get_or_build_reachability_for(&mut self, id: u32) -> &ReachableFrom {
        if let Ok(idx) = self.body.blocks.binary_search_by_key(&id, |blk| blk.target) {
            let (start, blk, end) = self.body.blocks.split_out_mut(idx);
            if let Some(meta) = blk.meta.get() {
                // SAFETY:
                // nll again
                unsafe { &*(meta as *const _) }
            } else {
                let mut meta = ReachableFrom::new();
                for remote_block in start.iter().chain(&*end) {
                    if Self::can_branch_to(&remote_block.term, id) {
                        meta.reachable.push(remote_block.target);
                    }
                }
                blk.meta.push(meta);

                blk.meta.get().unwrap()
            }
        } else {
            panic!("Invalid block {id}")
        }
    }
}
