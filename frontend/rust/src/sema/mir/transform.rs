use super::{MirBasicBlock, MirFunctionBody, MirStatement, MirTerminator};

use crate::sema::Result;

pub enum MirPassType {
    /// A Mandatory pass (IE. Drop elab or borrowck)
    ///
    /// Value is the order value (higher order passes run last)
    Mandatory(u32),
    /// Optional Pass (IE. an optimization)
    ///
    /// Value is the required optimization val
    Optional(u32),
}

/// A pass on input mir
pub trait MirFunctionPass {
    fn pass_type(&self) -> MirPassType;
    fn accept_function(&self, targ: &mut super::MirFunctionBody) -> Result<()>;
}

pub trait MirBasicBlockPass {
    fn pass_type(&self) -> MirPassType;
    fn accept_basic_block(&self, targ: &mut super::MirBasicBlock) -> Result<()>;
}

pub trait MirStatementPass {
    fn pass_type(&self) -> MirPassType;
    fn accept_statement(&self, targ: &mut super::MirStatement) -> Result<()>;
}

impl<P: MirBasicBlockPass> MirFunctionPass for P {
    fn pass_type(&self) -> MirPassType {
        <P as MirBasicBlockPass>::pass_type(self)
    }

    fn accept_function(&self, targ: &mut super::MirFunctionBody) -> Result<()> {
        for bb in &mut targ.bbs {
            self.accept_basic_block(bb)?;
        }
        Ok(())
    }
}

impl<P: MirStatementPass> MirBasicBlockPass for P {
    fn pass_type(&self) -> MirPassType {
        <P as MirStatementPass>::pass_type(self)
    }

    fn accept_basic_block(&self, targ: &mut super::MirBasicBlock) -> Result<()> {
        for stmt in &mut targ.stmts {
            self.accept_statement(stmt)?;
        }
        Ok(())
    }
}

mod fallthrough;
mod remove_discard;
mod unreachable;

pub const REQ_PASSES: &[&(dyn MirFunctionPass + Sync)] = &[];
pub const OPT_PASSES: &[&(dyn MirFunctionPass + Sync)] = &[
    &fallthrough::InferFallthrough,
    &unreachable::PropagateUnreachable,
];
