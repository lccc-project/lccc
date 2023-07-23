use crate::interning::Symbol;
use crate::sema::{ty::AbiTag, DefId};

use super::visitor::{
    FunctionBodyVisitor, FunctionDefVisitor, FunctionTyVisitor, ModVisitor, PointerTyVisitor,
    TupleTyVisitor, TypeDefVisitor, TypeVisitor, ValueDefVisitor,
};

use xlang::abi::{collection::HashMap, string::String, vec::Vec};

use super::IntMangler;

pub struct NameModVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    int_mangler: &'a IntMangler,
}

impl<'a> NameModVisitor<'a> {
    pub fn new(names: &'a mut HashMap<DefId, String>, int_mangler: &'a IntMangler) -> Self {
        Self { names, int_mangler }
    }
}

impl<'a> ModVisitor for NameModVisitor<'a> {
    fn visit_defid(&mut self, _defid: DefId) {}

    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>> {
        Some(Box::new(NameModVisitor::new(self.names, self.int_mangler)))
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor>> {
        todo!()
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(NameValueDefVisitor::new(
            self.names,
            self.int_mangler,
        )))
    }
}

struct NameValueDefVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    int_mangler: &'a IntMangler,
    defid: Option<DefId>,
    name: Option<Vec<Symbol>>,
}

impl<'a> NameValueDefVisitor<'a> {
    fn new(names: &'a mut HashMap<DefId, String>, int_mangler: &'a IntMangler) -> Self {
        Self {
            names,
            int_mangler,
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
            self.int_mangler,
            self.defid.take().unwrap(),
            self.name.as_deref().take().unwrap(),
        )))
    }
}

struct NameFunctionDefVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    int_mangler: &'a IntMangler,
    defid: DefId,
    name: &'a [Symbol],
}

impl<'a> NameFunctionDefVisitor<'a> {
    fn new(
        names: &'a mut HashMap<DefId, String>,
        int_mangler: &'a IntMangler,
        defid: DefId,
        name: &'a [Symbol],
    ) -> Self {
        Self {
            names,
            int_mangler,
            defid,
            name,
        }
    }
}

impl<'a> FunctionDefVisitor for NameFunctionDefVisitor<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(NameFunctionTyVisitor::new(
            self.names,
            self.int_mangler,
            self.defid,
            self.name,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        None
    }
}

struct NameFunctionTyVisitor<'a> {
    names: &'a mut HashMap<DefId, String>,
    int_mangler: &'a IntMangler,
    defid: DefId,
    name: &'a [Symbol],
    abi: AbiTag,
    params: Vec<String>,
}

impl<'a> NameFunctionTyVisitor<'a> {
    fn new(
        names: &'a mut HashMap<DefId, String>,
        int_mangler: &'a IntMangler,
        defid: DefId,
        name: &'a [Symbol],
    ) -> Self {
        Self {
            names,
            int_mangler,
            defid,
            name,
            abi: AbiTag::Rust,
            params: Vec::new(),
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
        if matches!(self.abi, AbiTag::C { .. }) {
            None
        } else {
            let idx = self.params.len();
            self.params.push(String::new());
            Some(Box::new(NameTypeVisitor::new(
                self.names,
                self.int_mangler,
                &mut self.params[idx],
            )))
        }
    }

    fn visit_cvarargs(&mut self) {
        todo!()
    }
}

impl Drop for NameFunctionTyVisitor<'_> {
    fn drop(&mut self) {
        match self.abi {
            AbiTag::Rust => {
                let mut mangled = String::from("_ZN");
                for component in self.name {
                    mangled += component.len().to_string();
                    mangled += &*component;
                }
                mangled.push('E');
                // TODO: cvarargs
                if self.params.is_empty() {
                    mangled.push('v');
                } else {
                    for param in &self.params {
                        mangled += param;
                    }
                }
                self.names.insert(self.defid, mangled);
            }
            AbiTag::C { .. } => {
                self.names
                    .insert(self.defid, self.name.last().unwrap().into());
            }
            _ => todo!(),
        }
    }
}

#[allow(dead_code)]
struct NameTypeVisitor<'a, 'b> {
    names: &'a mut HashMap<DefId, String>,
    int_mangler: &'a IntMangler,
    name_out: &'b mut String,
}

impl<'a, 'b> NameTypeVisitor<'a, 'b> {
    fn new(
        names: &'a mut HashMap<DefId, String>,
        int_mangler: &'a IntMangler,
        name_out: &'b mut String,
    ) -> Self {
        Self {
            names,
            int_mangler,
            name_out,
        }
    }
}

impl<'a, 'b> TypeVisitor for NameTypeVisitor<'a, 'b> {
    fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>> {
        todo!()
    }

    fn visit_pointer(&mut self) -> Option<Box<dyn PointerTyVisitor + '_>> {
        todo!()
    }
}
