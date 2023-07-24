use xlang::ir;

use crate::{interning::Symbol, sema::DefId};

use super::{
    visitor::{AttrVisitor, FunctionDefVisitor, ModVisitor, TypeDefVisitor, ValueDefVisitor},
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
        Some(Box::new(XirValueDefVisitor::new(self.names, self.file)))
    }
}

pub struct XirValueDefVisitor<'a> {
    names: &'a NameMap,
    file: &'a mut ir::File,
    name: Option<Symbol>,
}

impl<'a> XirValueDefVisitor<'a> {
    pub fn new(names: &'a NameMap, file: &'a mut ir::File) -> Self {
        Self {
            names,
            file,
            name: None,
        }
    }
}

impl<'a> ValueDefVisitor for XirValueDefVisitor<'a> {
    fn visit_defid(&mut self, defid: DefId) {
        self.name = Some(self.names[&defid]);
    }

    fn visit_name(&mut self, _: &[Symbol]) {
        // already covered by pre-mangling
    }

    fn visit_attr(&mut self) -> Option<Box<dyn AttrVisitor + '_>> {
        // don't care yet
        None
    }

    fn visit_function(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>> {
        todo!()
    }
}
