use crate::interning::Symbol;
use crate::sema::{DefId, Definitions};

use self::visitor::{ModVisitor, TypeDefVisitor, ValueDefVisitor};

use xlang::abi::collection::HashMap;
use xlang::ir;

pub mod visitor;

struct NameModVisitor {
    names: HashMap<DefId, String>,
}

impl ModVisitor for NameModVisitor {
    fn visit_defid(&mut self, defid: DefId) {
        todo!()
    }

    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>> {
        todo!()
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>> {
        todo!()
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        todo!()
    }
}

pub fn irgen(defs: &mut Definitions, _file: &mut ir::File) {
    let names = HashMap::new();
    defs.visit_all_crates(NameModVisitor { names });
}
