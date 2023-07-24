use xlang::abi::vec::Vec;
use xlang::ir;

use crate::{
    interning::Symbol,
    sema::{ty::AbiTag, DefId},
};

use super::visitor::{
    AttrVisitor, FunctionBodyVisitor, FunctionDefVisitor, FunctionTyVisitor, ModVisitor,
    PointerTyVisitor, TupleTyVisitor, TypeDefVisitor, TypeVisitor, ValueDefVisitor,
};
use super::NameMap;

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
        Some(Box::new(XirFunctionDefVisitor::new(
            self.names,
            self.file,
            self.name
                .expect("name should have been set from visit_defid prior"),
        )))
    }
}

pub struct XirFunctionDefVisitor<'a> {
    names: &'a NameMap,
    file: &'a mut ir::File,
    name: Symbol,
    ty: ir::FnType,
}

impl<'a> XirFunctionDefVisitor<'a> {
    fn new(names: &'a NameMap, file: &'a mut ir::File, name: Symbol) -> Self {
        Self {
            names,
            file,
            name,
            ty: ir::FnType::default(),
        }
    }
}

impl<'a> FunctionDefVisitor for XirFunctionDefVisitor<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.names,
            &mut self.ty,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        todo!()
    }
}

pub struct XirFunctionTyVisitor<'a> {
    names: &'a NameMap,
    fnty: &'a mut ir::FnType,
}

impl<'a> XirFunctionTyVisitor<'a> {
    fn new(names: &'a NameMap, fnty: &'a mut ir::FnType) -> Self {
        Self { names, fnty }
    }
}

impl<'a> FunctionTyVisitor for XirFunctionTyVisitor<'a> {
    fn visit_tag(&mut self, _: AbiTag) {
        self.fnty.tag = ir::Abi::C; // TODO: handle unwind and fastcall
    }

    fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.names,
            &mut self.fnty.ret,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        todo!()
    }

    fn visit_cvarargs(&mut self) {
        todo!()
    }
}

pub struct XirTypeVisitor<'a> {
    names: &'a NameMap,
    ty: &'a mut ir::Type,
}

impl<'a> XirTypeVisitor<'a> {
    fn new(names: &'a NameMap, ty: &'a mut ir::Type) -> Self {
        Self { names, ty }
    }
}

impl<'a> TypeVisitor for XirTypeVisitor<'a> {
    fn visit_pointer(&mut self) -> Option<Box<dyn PointerTyVisitor + '_>> {
        todo!()
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>> {
        *self.ty = ir::Type::Product(Vec::new());
        // TODO: is there a less-ugly way to do this?
        if let ir::Type::Product(tuple) = self.ty {
            Some(Box::new(XirTupleTyVisitor::new(self.names, tuple)))
        } else {
            unreachable!()
        }
    }
}

pub struct XirTupleTyVisitor<'a> {
    names: &'a NameMap,
    tuple: &'a mut Vec<ir::Type>,
}

impl<'a> XirTupleTyVisitor<'a> {
    fn new(names: &'a NameMap, tuple: &'a mut Vec<ir::Type>) -> Self {
        Self { names, tuple }
    }
}

impl<'a> TupleTyVisitor for XirTupleTyVisitor<'a> {
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        let index = self.tuple.len();
        self.tuple.push(ir::Type::default());
        Some(Box::new(XirTypeVisitor::new(
            self.names,
            &mut self.tuple[index],
        )))
    }
}
