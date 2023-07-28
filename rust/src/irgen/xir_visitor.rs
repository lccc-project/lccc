use std::convert::TryInto;

use xlang::abi::option::{None as XLangNone, Some as XLangSome};
use xlang::ir::{self, ScalarType, ScalarTypeKind};
use xlang::prelude::v1::HashMap;
use xlang::targets::properties::TargetProperties;
use xlang::{abi::vec::Vec, vec};

use crate::sema::mir::SsaVarId;
use crate::sema::ty;
use crate::{
    interning::Symbol,
    sema::{mir::BasicBlockId, ty::AbiTag, DefId},
};

use super::visitor::{
    AttrVisitor, BasicBlockVisitor, CallVisitor, ConstIntVisitor, ExprVisitor, FunctionBodyVisitor,
    FunctionDefVisitor, FunctionTyVisitor, IntTyVisitor, JumpVisitor, ModVisitor, PointerTyVisitor,
    StatementVisitor, TailcallVisitor, TerminatorVisitor, TupleTyVisitor, TypeDefVisitor,
    TypeVisitor, ValueDefVisitor,
};
use super::NameMap;

pub struct XirModVisitor<'a> {
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirModVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            deftys,
            file,
            properties,
        }
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
        Some(Box::new(XirValueDefVisitor::new(
            self.names,
            self.deftys,
            self.file,
            self.properties,
        )))
    }
}

pub struct XirModTypeGatherer<'a> {
    names: &'a NameMap,
    deftys: &'a mut HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirModTypeGatherer<'a> {
    pub fn new(
        names: &'a NameMap,
        deftys: &'a mut HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            deftys,
            properties,
        }
    }
}

impl<'a> ModVisitor for XirModTypeGatherer<'a> {
    fn visit_defid(&mut self, _: DefId) {}

    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>> {
        todo!()
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>> {
        todo!()
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(XirValueDefTypeGatherer::new(
            self.names,
            self.deftys,
            self.properties,
        )))
    }
}

pub struct XirValueDefTypeGatherer<'a> {
    names: &'a NameMap,
    deftys: &'a mut HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
    defid: Option<DefId>,
}

impl<'a> XirValueDefTypeGatherer<'a> {
    pub fn new(
        names: &'a NameMap,
        deftys: &'a mut HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            deftys,
            properties,
            defid: None,
        }
    }
}

impl<'a> ValueDefVisitor for XirValueDefTypeGatherer<'a> {
    fn visit_defid(&mut self, defid: DefId) {
        self.defid = Some(defid);
    }

    fn visit_name(&mut self, name: &[Symbol]) {
        // don't care
    }

    fn visit_attr(&mut self) -> Option<Box<dyn AttrVisitor + '_>> {
        None
    }

    fn visit_function(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>> {
        let defid = self.defid.expect("Must have already visited the defid");

        let mut ty = self.deftys.get_or_insert_mut(
            defid,
            ir::Type::FnType(xlang::abi::boxed::Box::new(ir::FnType::default())),
        );

        match ty {
            ir::Type::FnType(fnty) => Some(Box::new(XirFunctionTypeGatherer::new(
                self.names,
                fnty,
                self.properties,
            ))),
            _ => unreachable!(),
        }
    }
}

pub struct XirValueDefVisitor<'a> {
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    name: Option<Symbol>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirValueDefVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            deftys,
            file,
            name: None,
            properties,
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
        let path = ir::Path {
            components: vec![ir::PathComponent::Text(
                (&*self.name.expect("name should have been set previously")).into(),
            )],
        };

        self.file.root.members.insert(
            path.clone(),
            ir::ScopeMember {
                annotations: ir::AnnotatedElement::default(),
                vis: ir::Visibility::Public,
                member_decl: ir::MemberDeclaration::Function(ir::FunctionDeclaration::default()),
            },
        );

        let def = match &mut self.file.root.members.get_mut(&path).unwrap().member_decl {
            ir::MemberDeclaration::Function(fndef) => fndef,
            _ => unreachable!(),
        };

        Some(Box::new(XirFunctionDefVisitor::new(
            self.names,
            self.deftys,
            def,
            self.properties,
        )))
    }
}

pub struct XirFunctionTypeGatherer<'a> {
    names: &'a NameMap,
    fnty: &'a mut ir::FnType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionTypeGatherer<'a> {
    fn new(
        names: &'a NameMap,
        fnty: &'a mut ir::FnType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            fnty,
            properties,
        }
    }
}

impl<'a> FunctionDefVisitor for XirFunctionTypeGatherer<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.names,
            self.fnty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        None
    }
}

pub struct XirFunctionDefVisitor<'a> {
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    fndef: &'a mut ir::FunctionDeclaration,
    ty: ir::FnType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionDefVisitor<'a> {
    fn new(
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        fndef: &'a mut ir::FunctionDeclaration,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            deftys,
            fndef,
            ty: ir::FnType::default(),
            properties,
        }
    }
}

impl<'a> FunctionDefVisitor for XirFunctionDefVisitor<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.names,
            &mut self.ty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        Some(Box::new(XirFunctionBodyVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            &mut self.ty,
            self.fndef.body.insert(ir::FunctionBody::default()),
        )))
    }
}

pub struct XirFunctionTyVisitor<'a> {
    names: &'a NameMap,
    fnty: &'a mut ir::FnType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionTyVisitor<'a> {
    fn new(
        names: &'a NameMap,
        fnty: &'a mut ir::FnType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            fnty,
            properties,
        }
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
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.names,
            self.fnty.params.push_mut(ir::Type::Null),
            self.properties,
        )))
    }

    fn visit_cvarargs(&mut self) {
        todo!()
    }
}

pub struct XirTypeVisitor<'a> {
    names: &'a NameMap,
    ty: &'a mut ir::Type,
    properties: &'a TargetProperties<'a>,
}

const NEVER: ir::Type = ir::Type::Scalar(ScalarType {
    kind: ScalarTypeKind::Integer {
        signed: false,
        min: XLangNone,
        max: XLangNone,
    },
    header: ir::ScalarTypeHeader {
        bitsize: 0,
        vectorsize: XLangNone,
        validity: ir::ScalarValidity::NONZERO,
    },
});

impl<'a> XirTypeVisitor<'a> {
    fn new(names: &'a NameMap, ty: &'a mut ir::Type, properties: &'a TargetProperties<'a>) -> Self {
        Self {
            names,
            ty,
            properties,
        }
    }
}

impl<'a> TypeVisitor for XirTypeVisitor<'a> {
    fn visit_int(&mut self) -> Option<Box<dyn IntTyVisitor + '_>> {
        *self.ty = ir::Type::Scalar(ScalarType::default());

        if let ir::Type::Scalar(sty) = self.ty {
            Some(Box::new(XirIntTyVisitor::new(sty, self.properties)))
        } else {
            unreachable!()
        }
    }

    fn visit_pointer(&mut self) -> Option<Box<dyn PointerTyVisitor + '_>> {
        todo!()
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>> {
        *self.ty = ir::Type::Product(Vec::new());
        // TODO: is there a less-ugly way to do this?
        if let ir::Type::Product(tuple) = self.ty {
            Some(Box::new(XirTupleTyVisitor::new(
                self.names,
                tuple,
                self.properties,
            )))
        } else {
            unreachable!()
        }
    }

    fn visit_never(&mut self) {
        *self.ty = NEVER;
    }
}

pub struct XirIntTyVisitor<'a> {
    ity: &'a mut ScalarType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirIntTyVisitor<'a> {
    fn new(ity: &'a mut ScalarType, properties: &'a TargetProperties<'a>) -> Self {
        Self { ity, properties }
    }
}

impl<'a> IntTyVisitor for XirIntTyVisitor<'a> {
    fn visit_type(&mut self, int_type: &ty::IntType) {
        self.ity.header.bitsize = match int_type.width {
            ty::IntWidth::Bits(bits) => bits.get(),
            ty::IntWidth::Size => self.properties.primitives.ptrbits,
        };

        self.ity.kind = ScalarTypeKind::Integer {
            signed: int_type.signed,
            min: XLangNone,
            max: XLangNone,
        }
    }
}

pub struct XirTupleTyVisitor<'a> {
    names: &'a NameMap,
    tuple: &'a mut Vec<ir::Type>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirTupleTyVisitor<'a> {
    fn new(
        names: &'a NameMap,
        tuple: &'a mut Vec<ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            names,
            tuple,
            properties,
        }
    }
}

impl<'a> TupleTyVisitor for XirTupleTyVisitor<'a> {
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        let index = self.tuple.len();
        self.tuple.push(ir::Type::default());
        Some(Box::new(XirTypeVisitor::new(
            self.names,
            &mut self.tuple[index],
            self.properties,
        )))
    }
}

pub struct XirFunctionBodyVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    fndecl: &'a mut ir::FunctionBody,
    targs: HashMap<BasicBlockId, Vec<ir::Type>>,
}

impl<'a> XirFunctionBodyVisitor<'a> {
    fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        fndecl: &'a mut ir::FunctionBody,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            fndecl,
            cur_fnty,
            targs: HashMap::new(),
        }
    }
}

impl<'a> FunctionBodyVisitor for XirFunctionBodyVisitor<'a> {
    fn visit_basic_block(&mut self) -> Option<Box<dyn BasicBlockVisitor + '_>> {
        Some(Box::new(XirBasicBlockVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.fndecl,
            &mut self.targs,
        )))
    }
}

pub struct XirBasicBlockVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: HashMap<SsaVarId, u32>,
    stack_height: u32,
}

impl<'a> XirBasicBlockVisitor<'a> {
    fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            var_heights: HashMap::new(),
            stack_height: 0u32,
        }
    }
}

impl<'a> BasicBlockVisitor for XirBasicBlockVisitor<'a> {
    fn visit_id(&mut self, bb_id: BasicBlockId) {
        self.targs.get_or_insert_mut(bb_id, Vec::new());
        self.body.block.items.push(ir::BlockItem::Target {
            num: bb_id.id(),
            stack: vec![],
        })
    }

    fn visit_incoming_vars(&mut self, incoming: &[SsaVarId]) {
        for (depth, &var) in incoming.iter().enumerate() {
            self.var_heights.insert(var, depth as u32);
        }

        self.stack_height = incoming
            .len()
            .try_into()
            .expect("Cannot have more than u32::MAX vars, what?");
    }

    fn visit_stmt(&mut self) -> Option<Box<dyn StatementVisitor + '_>> {
        todo!()
    }

    fn visit_term(&mut self) -> Option<Box<dyn TerminatorVisitor + '_>> {
        Some(Box::new(XirTerminatorVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.body,
            self.targs,
            &mut self.var_heights,
            &mut self.stack_height,
        )))
    }
}

pub struct XirTerminatorVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
}

impl<'a> XirTerminatorVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            var_heights,
            stack_height,
        }
    }
}

impl<'a> TerminatorVisitor for XirTerminatorVisitor<'a> {
    fn visit_call(&mut self) -> Option<Box<dyn CallVisitor + '_>> {
        Some(Box::new(XirCallVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_tailcall(&mut self) -> Option<Box<dyn TailcallVisitor + '_>> {
        Some(Box::new(XirTailcallVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_jump(&mut self) -> Option<Box<dyn JumpVisitor + '_>> {
        Some(Box::new(XirJumpVisitor::new(
            self.names,
            self.properties,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }
    fn visit_return(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirReturnVisitor(XirExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        ))))
    }
}

pub struct XirReturnVisitor<'a>(XirExprVisitor<'a>);

impl<'a> ExprVisitor for XirReturnVisitor<'a> {
    fn visit_unreachable(&mut self) {
        self.0.visit_unreachable()
    }

    fn visit_const_int(&mut self) -> Option<Box<dyn ConstIntVisitor + '_>> {
        self.0.visit_const_int()
    }

    fn visit_const(&mut self, defid: DefId) {
        self.0.visit_const(defid)
    }
}

impl<'a> Drop for XirReturnVisitor<'a> {
    fn drop(&mut self) {
        self.0
            .body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Exit { values: 1 }))
    }
}

pub struct XirTailcallVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    fnty: Option<ir::FnType>,
}

impl<'a> XirTailcallVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            names,
            body,
            deftys,
            cur_fnty,
            targs,
            var_heights,
            stack_height,
            fnty: None,
            properties,
        }
    }
}

impl<'a> TailcallVisitor for XirTailcallVisitor<'a> {
    fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.names,
            self.fnty.insert(ir::FnType::default()),
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }
}

impl<'a> Drop for XirTailcallVisitor<'a> {
    fn drop(&mut self) {
        let fnty = self.fnty.take().expect("visit_fnty must have been called");

        let is_never = fnty.ret == NEVER;

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::CallFunction(fnty)));

        if is_never {
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Pop(1)));
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Invalid(
                    self.cur_fnty.ret.clone(),
                ))));
        }
        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Exit { values: 1 }))
    }
}

pub struct XirCallVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    retplace: Option<SsaVarId>,
    fnty: Option<ir::FnType>,
}

impl<'a> XirCallVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            names,
            body,
            deftys,
            targs,
            var_heights,
            stack_height,
            retplace: None,
            fnty: None,
            properties,
        }
    }
}

impl<'a> CallVisitor for XirCallVisitor<'a> {
    fn visit_retplace(&mut self, retplace: SsaVarId) {
        self.retplace = Some(retplace);
    }

    fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.names,
            self.fnty.insert(ir::FnType::default()),
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_next(&mut self) -> Option<Box<dyn JumpVisitor + '_>> {
        todo!()
    }
}

pub struct XirJumpVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    targ: u32,
}

impl<'a> XirJumpVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            names,
            properties,
            body,
            targs,
            var_heights,
            stack_height,
            targ: !0,
        }
    }
}

impl<'a> JumpVisitor for XirJumpVisitor<'a> {
    fn visit_target_bb(&mut self, targbb: BasicBlockId) {
        self.targ = targbb.id()
    }

    fn visit_remap(&mut self, src: SsaVarId, _: SsaVarId) {
        todo!()
    }
}

pub struct XirExprVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
}

impl<'a> Drop for XirExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height += 1;
    }
}

impl<'a> XirExprVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
        }
    }
}

impl<'a> ExprVisitor for XirExprVisitor<'a> {
    fn visit_unreachable(&mut self) {
        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Invalid(
                NEVER,
            ))));
    }

    fn visit_const_int(&mut self) -> Option<Box<dyn ConstIntVisitor + '_>> {
        match self
            .body
            .block
            .items
            .push_mut(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Integer {
                ty: ScalarType::default(),
                val: 0,
            }))) {
            ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Integer { ty, val })) => {
                Some(Box::new(XirConstIntVisitor::new(
                    self.names,
                    self.properties,
                    self.deftys,
                    self.targs,
                    self.var_heights,
                    self.stack_height,
                    ty,
                    val,
                )))
            }
            _ => unreachable!(),
        }
    }

    fn visit_const(&mut self, defid: DefId) {
        let sym = self.names[&defid];

        let path = ir::Path {
            components: vec![ir::PathComponent::Text((&sym).into())],
        };

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Const(
                ir::Value::GlobalAddress {
                    ty: self.deftys[&defid].clone(),
                    item: path,
                },
            )))
    }
}

pub struct XirConstIntVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    ty: &'a mut ir::ScalarType,
    val: &'a mut u128,
}

impl<'a> XirConstIntVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::Type>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
        ty: &'a mut ir::ScalarType,
        val: &'a mut u128,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            targs,
            var_heights,
            stack_height,
            ty,
            val,
        }
    }
}

impl<'a> ConstIntVisitor for XirConstIntVisitor<'a> {
    fn visit_intty(&mut self) -> Option<Box<dyn IntTyVisitor + '_>> {
        Some(Box::new(XirIntTyVisitor::new(self.ty, self.properties)))
    }

    fn visit_value(&mut self, val: u128) {
        *self.val = val;
    }
}
