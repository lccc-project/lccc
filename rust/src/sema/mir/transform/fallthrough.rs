use crate::sema::mir;

use super::MirBasicBlockPass;

pub struct InferFallthrough;

impl InferFallthrough {
    fn accept_jump(&self, cur_bb: mir::BasicBlockId, jmp: &mut mir::MirJumpInfo) {
        if cur_bb.is_successor(jmp.targbb) {
            jmp.fallthrough = true;
        }
    }
}

impl MirBasicBlockPass for InferFallthrough {
    fn pass_type(&self) -> super::MirPassType {
        super::MirPassType::Optional(64)
    }

    fn accept_basic_block(
        &self,
        targ: &mut crate::sema::mir::MirBasicBlock,
    ) -> crate::sema::Result<()> {
        let id = targ.id;

        match &mut targ.term.body {
            crate::sema::mir::MirTerminator::Call(call) => {
                self.accept_jump(id, &mut call.next);
                Ok(())
            }
            crate::sema::mir::MirTerminator::DropInPlace(drop) => {
                self.accept_jump(id, &mut drop.next);
                Ok(())
            }
            crate::sema::mir::MirTerminator::Branch(br) => {
                self.accept_jump(id, &mut br.if_block);
                self.accept_jump(id, &mut br.else_block);
                Ok(())
            }
            crate::sema::mir::MirTerminator::SwitchInt(switch) => {
                for (_, jmp) in &mut switch.cases {
                    self.accept_jump(id, jmp);
                }
                if let Some(default) = &mut switch.default {
                    self.accept_jump(id, default);
                }
                Ok(())
            }
            crate::sema::mir::MirTerminator::Jump(jmp) => {
                self.accept_jump(id, jmp);
                Ok(())
            }
            crate::sema::mir::MirTerminator::Return(_)
            | crate::sema::mir::MirTerminator::Tailcall(_)
            | crate::sema::mir::MirTerminator::Unreachable
            | crate::sema::mir::MirTerminator::Resume => Ok(()),
        }
    }
}
