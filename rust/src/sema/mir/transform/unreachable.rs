use super::{MirBasicBlockPass, MirPassType, Result};

use crate::sema::mir::{MirExpr, MirStatement, MirTerminator};

pub struct PropagateUnreachable;

impl PropagateUnreachable {
    fn expr_contains_unreachable(expr: &MirExpr) -> bool {
        match expr {
            MirExpr::Unreachable => true,
            MirExpr::Uninit(_) => false,
            MirExpr::Var(_) => false,
            MirExpr::Read(val) => Self::expr_contains_unreachable(val),
            MirExpr::Alloca(_, _, expr) => Self::expr_contains_unreachable(expr),
            MirExpr::AllocaDrop(_, _) => false,
            MirExpr::ConstInt(_, _) => false,
            MirExpr::ConstString(_, _) => false,
            MirExpr::Const(_, _) => false,
            MirExpr::Retag(_, _, expr) => Self::expr_contains_unreachable(expr),
            MirExpr::Cast(expr, _) => Self::expr_contains_unreachable(expr),
            MirExpr::Tuple(exprs) => exprs.iter().any(|e| Self::expr_contains_unreachable(e)),
            MirExpr::Intrinsic(_, _) => false,
            MirExpr::FieldProject(val, _) => Self::expr_contains_unreachable(expr),
            MirExpr::GetSubobject(val, _) => Self::expr_contains_unreachable(expr),
            MirExpr::Ctor(ctor) => ctor
                .fields
                .iter()
                .map(|(_, expr)| expr)
                .chain(ctor.rest_init.as_deref())
                .any(|e| Self::expr_contains_unreachable(e)),
            MirExpr::BinaryExpr(_, l, r) => {
                Self::expr_contains_unreachable(l) || Self::expr_contains_unreachable(r)
            }
            MirExpr::UnaryExpr(_, u) => Self::expr_contains_unreachable(u),
            MirExpr::GetSymbol(_) => false,
            MirExpr::ConstChar(_, _) => false,
            MirExpr::InlineConst(_) => false, // ish
        }
    }
}

impl MirBasicBlockPass for PropagateUnreachable {
    fn pass_type(&self) -> MirPassType {
        MirPassType::Optional(1024)
    }

    fn accept_basic_block(&self, targ: &mut super::MirBasicBlock) -> Result<()> {
        if targ.term.body != MirTerminator::Unreachable {
            for stat in &mut targ.stmts {
                match &mut stat.body {
                    MirStatement::Write(dest, expr) => {
                        if Self::expr_contains_unreachable(dest)
                            || Self::expr_contains_unreachable(expr)
                        {
                            targ.term.body = MirTerminator::Unreachable;
                            break;
                        }
                    }
                    MirStatement::Declare { init, .. } => {
                        if Self::expr_contains_unreachable(init) {
                            targ.term.body = MirTerminator::Unreachable;
                            break;
                        }
                    }
                    MirStatement::Discard(val) => {
                        if Self::expr_contains_unreachable(val) {
                            targ.term.body = MirTerminator::Unreachable;
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }

        match &mut targ.term.body {
            MirTerminator::Unreachable => {
                targ.stmts.clear();
            }
            MirTerminator::SwitchInt(sw) => {
                if Self::expr_contains_unreachable(&sw.expr) {
                    targ.stmts.clear();
                    targ.term.body = MirTerminator::Unreachable;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
