use crate::interning::Symbol;
use crate::sema::{ty::AbiTag, DefId, Definitions};

use self::visitor::{
    FunctionBodyVisitor, FunctionDefVisitor, FunctionTyVisitor, ModVisitor, TypeDefVisitor,
    TypeVisitor, ValueDefVisitor,
};

use xlang::abi::{collection::HashMap, string::String, vec::Vec};
use xlang::ir;

pub mod visitor;

struct NameModVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
}

impl<'a> NameModVisitor<'a> {
    fn new(names: &'a mut HashMap<DefId, String>) -> Self {
        Self { names }
    }
}

impl<'a> ModVisitor for NameModVisitor<'a> {
    fn visit_defid(&mut self, _defid: DefId) {}

    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>> {
        Some(Box::new(NameModVisitor::new(self.names)))
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor>> {
        todo!()
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(NameValueDefVisitor::new(self.names)))
    }
}

struct NameValueDefVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    defid: Option<DefId>,
    name: Option<Vec<Symbol>>,
}

impl<'a> NameValueDefVisitor<'a> {
    fn new(names: &'a mut HashMap<DefId, String>) -> Self {
        Self {
            names,
            defid: None,
            name: None,
        }
    }
}

impl<'a> ValueDefVisitor for NameValueDefVisitor<'a> {
    fn visit_defid(&mut self, defid: DefId) {
        self.defid = Some(defid);
    }

    fn visit_name(&mut self, name: &[Symbol]) {
        self.name = Some(name.iter().copied().collect());
    }

    fn visit_function(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>> {
        Some(Box::new(NameFunctionDefVisitor::new(
            self.names,
            self.defid.take().unwrap(),
            self.name.as_deref().take().unwrap(),
        )))
    }
}

struct NameFunctionDefVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    defid: DefId,
    name: &'a [Symbol],
}

impl<'a> NameFunctionDefVisitor<'a> {
    fn new(names: &'a mut HashMap<DefId, String>, defid: DefId, name: &'a [Symbol]) -> Self {
        Self { names, defid, name }
    }
}

impl<'a> FunctionDefVisitor for NameFunctionDefVisitor<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(NameFunctionTyVisitor::new(
            self.names, self.defid, self.name,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        None
    }
}

struct NameFunctionTyVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    defid: DefId,
    name: &'a [Symbol],
    abi: AbiTag,
}

impl<'a> NameFunctionTyVisitor<'a> {
    fn new(names: &'a mut HashMap<DefId, String>, defid: DefId, name: &'a [Symbol]) -> Self {
        Self {
            names,
            defid,
            name,
            abi: AbiTag::Rust,
        }
    }
}

impl<'a> FunctionTyVisitor for NameFunctionTyVisitor<'a> {
    fn visit_tag(&mut self, abi: AbiTag) {
        self.abi = abi;
    }

    fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        None
    }

    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        todo!()
    }

    fn visit_cvarargs(&mut self) {
        todo!()
    }
}

impl Drop for NameFunctionTyVisitor<'_> {
    fn drop(&mut self) {
        let mut mangled = String::from("_ZN");
        for component in self.name {
            mangled += component.len().to_string();
            mangled += &*component;
        }
        mangled.push('E');
        // TODO: params and cvarargs
        // until then, all parameter lists take void
        mangled.push('v');
        self.names.insert(self.defid, mangled);
    }
}

pub fn irgen(defs: &mut Definitions, _file: &mut ir::File) {
    let mut names = HashMap::new();
    defs.visit_all_crates(NameModVisitor::new(&mut names));
}
