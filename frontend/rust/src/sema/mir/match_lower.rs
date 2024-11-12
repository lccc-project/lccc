use crate::span::Spanned;

use crate::sema::{
    ty::{self, IntType},
    tyck,
};
use crate::sema::{Definitions, Error, Result};

use super::{MirExpr, MirJumpInfo, MirSwitchIntInfo, MirTerminator};

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PatternMode {
    ConstInt(u128, IntType), // This matches integer types, but also variant discriminants
    Default,                 // Match a hole here
                             // write_statement is responsible for bindings
}

pub struct MatchBuilder<'a> {
    defs: &'a Definitions,
    arms: Vec<PatternMode>,
    is_switchable: bool,
}

impl<'a> MatchBuilder<'a> {
    pub fn new(defs: &'a Definitions) -> Self {
        Self {
            defs,
            arms: vec![],
            is_switchable: true,
        }
    }

    pub fn write_arm(&mut self, arm: Spanned<tyck::ThirPatternMatcher>) -> Result<()> {
        match arm.inner {
            tyck::ThirMatcherInner::Hole => self.arms.push(PatternMode::Default),
            tyck::ThirMatcherInner::ConstInt(val, Some(intty)) => {
                self.arms.push(PatternMode::ConstInt(val, intty.body))
            }
            tyck::ThirMatcherInner::ConstInt(val, None) => {
                let intty = match &arm.ty {
                    ty::Type::Int(intty) => *intty,
                    ty => panic!("Invalid type for ConstInt pattern `{}`", ty),
                };
                self.arms.push(PatternMode::ConstInt(val, intty))
            }
            tyck::ThirMatcherInner::Const(_) => todo!("const"),
        }

        Ok(())
    }

    pub fn build_match_term(
        self,
        expr: Spanned<MirExpr>,
        branches: Vec<MirJumpInfo>,
    ) -> MirTerminator {
        if self.arms.is_empty() {
            MirTerminator::Unreachable
        } else if self.arms.len() == 1 {
            MirTerminator::Jump(branches.into_iter().next().unwrap())
        } else if self.is_switchable {
            let mut arms = self.arms;
            let mut switch = MirSwitchIntInfo {
                expr,
                ty: IntType::usize, // WIll be filled by `mode`, all of the `IntType`s
                default: None,
                cases: vec![],
            };
            for (mode, jmp) in arms.into_iter().zip(branches) {
                match mode {
                    PatternMode::ConstInt(val, intty) => {
                        switch.ty = intty;
                        switch.cases.push((val, jmp))
                    }
                    PatternMode::Default => {
                        switch.default = Some(jmp);
                        break;
                    }
                }
            }

            switch.cases.sort_by_key(|(val, _)| *val);
            switch.cases.dedup_by_key(|(val, _)| *val);

            MirTerminator::SwitchInt(switch)
        } else {
            todo!("complex switch")
        }
    }
}
