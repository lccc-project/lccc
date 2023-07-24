use xlang::ir;

use crate::sema::DefId;

use super::{
    visitor::{ModVisitor, TypeDefVisitor, ValueDefVisitor},
    NameMap,
};

pub struct XirModVisitor<'a> {
    names: &'a NameMap,
    file: &'a mut ir::File,
}

impl<'a> XirModVisitor<'a> {
    pub fn new(names: &'a NameMap, file: &'a mut ir::File) -> Self {
        Self { names, file }
    }
}

impl<'a> ModVisitor for XirModVisitor<'a> {
    fn visit_defid(&mut self, _: DefId) {}

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
