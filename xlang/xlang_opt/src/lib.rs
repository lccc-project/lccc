use xlang::ir::{Block, Expr, FunctionBody};

pub mod cfg;
pub mod expr;
pub mod provenance;

pub trait Analyzer {
    type Output;
    fn analyze_expr(&mut self, expr: &Expr);
    fn analyze_block(&mut self, block: &Block);
    fn analyze_fnbody(&mut self, fnbody: &FunctionBody);

    fn output(self) -> Self::Output;
}
