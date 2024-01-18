use core::convert::TryInto;
use core::fmt::Write;

use xlang::ir::{
    self, AggregateCtor, AggregateField, AggregateFieldSpecifier, AnnotatedElement, ScalarTypeKind,
    ScalarValidity,
};
use xlang::prelude::v1::{HashMap, Pair};
use xlang::targets::properties::TargetProperties;
use xlang::{abi::string::String as XLangString, abi::vec::Vec, vec};
use xlang::{
    abi::{
        self,
        option::{None as XLangNone, Some as XLangSome},
    },
    ir::PathComponent,
};

use crate::sema::{cx, hir::BinaryOp, mir::SsaVarId};
use crate::sema::{generics, ty, UserTypeKind};
use crate::sema::{mir::UnaryOp, ty::Mutability};
use crate::{
    interning::Symbol,
    lang::LangItem,
    sema::{intrin::IntrinsicDef, mir::BasicBlockId, ty::AbiTag, DefId},
    span,
};
use crate::{lex::StringType, sema::Definitions};

use super::visitor::{
    ArrayTyVisitor, AttrVisitor, BasicBlockVisitor, BinaryExprVisitor, BranchArmVisitor,
    BranchVisitor, CallVisitor, CastVisitor, ConstIntVisitor, ConstStringVisitor,
    ConstructorDefVisitor, ConstructorVisitor, ExprVisitor, FieldAccessVisitor, FieldInitVisitor,
    FieldVisitor, FunctionBodyVisitor, FunctionDefVisitor, FunctionTyVisitor, IntTyVisitor,
    JumpVisitor, LetStatementVisitor, ModVisitor, PointerTyVisitor, ReferenceTyVisitor,
    StatementVisitor, TailcallVisitor, TerminatorVisitor, TupleExprVisitor, TupleTyVisitor,
    TypeDefVisitor, TypeVisitor, UnaryExprVisitor, ValueDefVisitor,
};
use super::NameMap;

pub fn into_path(name: Symbol) -> ir::Path {
    ir::Path {
        components: vec![PathComponent::Text((&name).into())],
    }
}

pub struct XirModVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirModVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
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
        Some(Box::new(self))
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>> {
        Some(Box::new(XirTypeDefVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            &mut self.file.root,
            self.properties,
        )))
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(XirValueDefVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            self.file,
            self.properties,
        )))
    }
}

pub struct XirModTypeGatherer<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a mut HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirModTypeGatherer<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a mut HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            properties,
        }
    }
}

impl<'a> ModVisitor for XirModTypeGatherer<'a> {
    fn visit_defid(&mut self, _: DefId) {}

    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>> {
        Some(Box::new(XirModTypeGatherer::new(
            self.defs,
            self.names,
            self.deftys,
            self.properties,
        )))
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>> {
        None
    }

    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(XirValueDefTypeGatherer::new(
            self.defs,
            self.names,
            self.deftys,
            self.properties,
        )))
    }
}

pub struct XirValueDefTypeGatherer<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a mut HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
    defid: Option<DefId>,
}

impl<'a> XirValueDefTypeGatherer<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a mut HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
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

        let ty = self.deftys.get_or_insert_mut(
            defid,
            ir::Type::FnType(xlang::abi::boxed::Box::new(ir::FnType::default())),
        );

        match ty {
            ir::Type::FnType(fnty) => Some(Box::new(XirFunctionTypeGatherer::new(
                self.defs,
                self.names,
                fnty,
                self.properties,
            ))),
            _ => unreachable!(),
        }
    }
}

pub struct XirValueDefVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    name: Option<Symbol>,
    scope_member: Option<ir::ScopeMember>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirValueDefVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            file,
            name: None,
            scope_member: None,
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
        self.scope_member = Some(ir::ScopeMember {
            annotations: ir::AnnotatedElement::default(),
            vis: ir::Visibility::Public,
            member_decl: ir::MemberDeclaration::Function(ir::FunctionDeclaration::default()),
        });

        let def: &mut ir::FunctionDeclaration =
            match &mut self.scope_member.as_mut().unwrap().member_decl {
                ir::MemberDeclaration::Function(fndef) => fndef,
                _ => unreachable!(),
            };

        Some(Box::new(XirFunctionDefVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            self.file,
            def,
            self.properties,
        )))
    }
}

impl<'a> Drop for XirValueDefVisitor<'a> {
    fn drop(&mut self) {
        let name = self.name.expect("defid should already be set");
        let path = ir::Path {
            components: vec![ir::PathComponent::Text((&name).into())],
        };
        self.file.root.members.insert(
            path,
            self.scope_member
                .take()
                .expect("this was supposed to be visited"),
        );
    }
}

pub struct XirFunctionTypeGatherer<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    fnty: &'a mut ir::FnType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionTypeGatherer<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        fnty: &'a mut ir::FnType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            fnty,
            properties,
        }
    }
}

impl<'a> FunctionDefVisitor for XirFunctionTypeGatherer<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.defs,
            self.names,
            self.fnty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        None
    }
}

pub struct XirTypeDefVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    scopedef: &'a mut ir::Scope,
    properties: &'a TargetProperties<'a>,
    defid: DefId,
    kind: ir::AggregateKind,
}

impl<'a> XirTypeDefVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        scopedef: &'a mut ir::Scope,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            scopedef,
            properties,
            defid: DefId::ROOT,
            kind: ir::AggregateKind::Struct,
        }
    }
}

impl<'a> TypeDefVisitor for XirTypeDefVisitor<'a> {
    fn visit_defid(&mut self, defid: DefId) {
        self.defid = defid;
    }

    fn visit_name(&mut self, name: &[Symbol]) {}

    fn visit_attr(&mut self) -> Option<Box<dyn AttrVisitor + '_>> {
        None
    }

    fn visit_struct(&mut self) -> Option<Box<dyn ConstructorDefVisitor + '_>> {
        let def = self.scopedef.members.get_or_insert_mut(
            into_path(self.names[&self.defid]),
            ir::ScopeMember {
                vis: ir::Visibility::Public,
                member_decl: ir::MemberDeclaration::AggregateDefinition(ir::AggregateDefinition {
                    annotations: Default::default(),
                    kind: self.kind,
                    fields: Vec::new(),
                }),
                ..Default::default()
            },
        );

        match &mut def.member_decl {
            ir::MemberDeclaration::AggregateDefinition(def) => {
                Some(Box::new(XirStructDefVisitor::new(
                    self.defs,
                    self.names,
                    self.deftys,
                    self.properties,
                    self.defid,
                    def,
                )))
            }
            _ => unreachable!(),
        }
    }

    fn visit_kind(&mut self, kind: UserTypeKind) {
        if let UserTypeKind::Union = kind {
            self.kind = ir::AggregateKind::Union;
        }
    }
}

pub struct XirStructDefVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
    def: &'a mut ir::AggregateDefinition,
    defid: DefId,
    fields: Vec<(Option<ty::FieldName>, ir::Type)>,
}

impl<'a> XirStructDefVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
        defid: DefId,
        def: &'a mut ir::AggregateDefinition,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            properties,
            def,
            defid,
            fields: Vec::new(),
        }
    }
}

impl<'a> Drop for XirStructDefVisitor<'a> {
    fn drop(&mut self) {
        let layout = self.defs.layout_of(
            &ty::Type::UserType(self.defid, Default::default()),
            self.defid,
            self.defid,
        );
        let mut fields = Vec::new();
        let mut offsets = Vec::new();

        for (field, ty) in core::mem::take(&mut self.fields) {
            let field = field.unwrap();
            let name = xlang::abi::format!("{}", field);

            let pos = offsets.insert_sorted(layout.field_offsets[&field]);
            fields.insert(
                AggregateField {
                    annotations: ir::AnnotatedElement::default(),
                    specifiers: AggregateFieldSpecifier::empty(),
                    name,
                    ty,
                    bitfield_width: ir::Value::Empty,
                },
                pos,
            );
        }

        self.def.fields = fields;
    }
}

impl<'a> ConstructorDefVisitor for XirStructDefVisitor<'a> {
    fn visit_field(&mut self) -> Option<Box<dyn FieldVisitor + '_>> {
        let (name, ty) = self.fields.push_mut((None, ir::Type::Null));
        Some(Box::new(XirConstructorDefFieldVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            self.properties,
            name,
            ty,
        )))
    }
}

pub struct XirConstructorDefFieldVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    properties: &'a TargetProperties<'a>,
    name: &'a mut Option<ty::FieldName>,
    ty: &'a mut ir::Type,
}

impl<'a> XirConstructorDefFieldVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        properties: &'a TargetProperties<'a>,
        name: &'a mut Option<ty::FieldName>,
        ty: &'a mut ir::Type,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            properties,
            name,
            ty,
        }
    }
}

impl<'a> FieldVisitor for XirConstructorDefFieldVisitor<'a> {
    fn visit_name(&mut self, name: &ty::FieldName) {
        *self.name = Some(*name);
    }

    fn visit_ty(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            self.ty,
            self.properties,
        )))
    }
}

pub struct XirFunctionDefVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    fndef: &'a mut ir::FunctionDeclaration,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionDefVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        fndef: &'a mut ir::FunctionDeclaration,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            deftys,
            file,
            fndef,
            properties,
        }
    }
}

impl<'a> FunctionDefVisitor for XirFunctionDefVisitor<'a> {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.defs,
            self.names,
            &mut self.fndef.ty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>> {
        Some(Box::new(XirFunctionBodyVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.file,
            &mut self.fndef.ty,
            self.fndef.body.insert(ir::FunctionBody::default()),
        )))
    }
}

pub struct XirFunctionTyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    fnty: &'a mut ir::FnType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirFunctionTyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        fnty: &'a mut ir::FnType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            fnty,
            properties,
        }
    }
}

impl<'a> FunctionTyVisitor for XirFunctionTyVisitor<'a> {
    fn visit_tag(&mut self, _: AbiTag) {
        self.fnty.tag = self.properties.default_tag_name.into(); // TODO: fastcall
    }

    fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.fnty.ret,
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
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
    defs: &'a Definitions,
    names: &'a NameMap,
    ty: &'a mut ir::Type,
    properties: &'a TargetProperties<'a>,
}

const NEVER: ir::Type = ir::Type::Scalar(ir::ScalarType {
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
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        ty: &'a mut ir::Type,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            ty,
            properties,
        }
    }
}

impl<'a> TypeVisitor for XirTypeVisitor<'a> {
    fn visit_array(&mut self) -> Option<Box<dyn ArrayTyVisitor + '_>> {
        *self.ty = ir::Type::Array(abi::boxed::Box::new(ir::ArrayType {
            ty: ir::Type::default(),
            len: ir::Value::Invalid(ir::Type::default()),
        }));
        if let ir::Type::Array(aty) = self.ty {
            Some(Box::new(XirArrayTyVisitor::new(
                self.defs,
                self.names,
                aty,
                self.properties,
            )))
        } else {
            unreachable!()
        }
    }

    fn visit_int(&mut self) -> Option<Box<dyn IntTyVisitor + '_>> {
        *self.ty = ir::Type::Scalar(ir::ScalarType::default());

        if let ir::Type::Scalar(sty) = self.ty {
            Some(Box::new(XirIntTyVisitor::new(sty, self.properties)))
        } else {
            unreachable!()
        }
    }

    fn visit_pointer(&mut self) -> Option<Box<dyn PointerTyVisitor + '_>> {
        *self.ty = ir::Type::Pointer(ir::PointerType::default());
        if let ir::Type::Pointer(pty) = self.ty {
            Some(Box::new(XirPointerTyVisitor::new(
                self.defs,
                self.names,
                pty,
                self.properties,
            )))
        } else {
            unreachable!()
        }
    }

    fn visit_reference(&mut self) -> Option<Box<dyn ReferenceTyVisitor + '_>> {
        *self.ty = ir::Type::Pointer(ir::PointerType::default());
        if let ir::Type::Pointer(pty) = self.ty {
            Some(Box::new(XirReferenceTyVisitor::new(
                self.defs,
                self.names,
                pty,
                self.properties,
            )))
        } else {
            unreachable!()
        }
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>> {
        *self.ty = ir::Type::Product(Vec::new());
        // TODO: is there a less-ugly way to do this?
        if let ir::Type::Product(tuple) = self.ty {
            Some(Box::new(XirTupleTyVisitor::new(
                self.defs,
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

    fn visit_user_type(&mut self, defid: DefId) {
        *self.ty = ir::Type::Named(into_path(self.names[&defid]));
    }
}

pub struct XirArrayTyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    aty: &'a mut ir::ArrayType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirArrayTyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        aty: &'a mut ir::ArrayType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            aty,
            properties,
        }
    }
}

impl<'a> ArrayTyVisitor for XirArrayTyVisitor<'a> {
    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.aty.ty,
            self.properties,
        )))
    }

    fn visit_len(&mut self, expr: &cx::ConstExpr) {
        let ty = ir::ScalarType {
            header: ir::ScalarTypeHeader {
                bitsize: self.properties.primitives.sizebits,
                vectorsize: XLangNone,
                validity: ScalarValidity::all(),
            },
            kind: ir::ScalarTypeKind::Integer {
                signed: false,
                min: XLangNone,
                max: XLangNone,
            },
        };
        self.aty.len = ir::Value::Integer {
            ty,
            val: match expr {
                cx::ConstExpr::HirVal(_) => todo!(),
                cx::ConstExpr::IntConst(_, val) => *val,
                cx::ConstExpr::Const(_) => todo!("const item"),
            },
        };
    }
}

pub struct XirIntTyVisitor<'a> {
    ity: &'a mut ir::ScalarType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirIntTyVisitor<'a> {
    fn new(ity: &'a mut ir::ScalarType, properties: &'a TargetProperties<'a>) -> Self {
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

pub struct XirPointerTyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    ty: &'a mut ir::PointerType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirPointerTyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        ty: &'a mut ir::PointerType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            ty,
            properties,
        }
    }
}

impl<'a> PointerTyVisitor for XirPointerTyVisitor<'a> {
    fn visit_mutability(&mut self, mutability: ty::Mutability) {
        if mutability == ty::Mutability::Const {
            self.ty.decl |= ir::PointerDeclarationType::CONST;
        }
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.ty.inner,
            self.properties,
        )))
    }
}

pub struct XirReferenceTyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    ty: &'a mut ir::PointerType,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirReferenceTyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        ty: &'a mut ir::PointerType,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
            names,
            ty,
            properties,
        }
    }
}

impl<'a> ReferenceTyVisitor for XirReferenceTyVisitor<'a> {
    fn visit_lifetime(&mut self, _: &ty::SemaLifetime) {}

    fn visit_mutability(&mut self, mutability: ty::Mutability) {
        if mutability == ty::Mutability::Const {
            self.ty.decl |= ir::PointerDeclarationType::CONST;
            // todo: dereferenceable, align
        } else {
            todo!(); // noalias
        }
    }

    fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.ty.inner,
            self.properties,
        )))
    }
}

pub struct XirTupleTyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    tuple: &'a mut Vec<ir::Type>,
    properties: &'a TargetProperties<'a>,
}

impl<'a> XirTupleTyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        tuple: &'a mut Vec<ir::Type>,
        properties: &'a TargetProperties<'a>,
    ) -> Self {
        Self {
            defs,
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
            self.defs,
            self.names,
            &mut self.tuple[index],
            self.properties,
        )))
    }
}

pub struct XirFunctionBodyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    file: &'a mut ir::File,
    cur_fnty: &'a mut ir::FnType,
    fndecl: &'a mut ir::FunctionBody,
    targs: HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: HashMap<SsaVarId, u32>,
    ssa_tys: HashMap<SsaVarId, ir::Type>,
    stack_height: u32,
}

impl<'a> XirFunctionBodyVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        file: &'a mut ir::File,
        cur_fnty: &'a mut ir::FnType,
        fndecl: &'a mut ir::FunctionBody,
    ) -> Self {
        let mut result = Self {
            stack_height: cur_fnty.params.len() as u32,
            defs,
            names,
            properties,
            deftys,
            file,
            fndecl,
            cur_fnty,
            targs: HashMap::new(),
            var_heights: HashMap::new(),
            ssa_tys: HashMap::new(),
        };

        for (idx, ty) in result.cur_fnty.params.iter().enumerate() {
            let id = SsaVarId(idx as u32);
            result.var_heights.insert(id, idx as u32);
            result.ssa_tys.insert(id, ty.clone());
        }

        result
    }
}

impl<'a> Drop for XirFunctionBodyVisitor<'a> {
    fn drop(&mut self) {
        for stat in self.fndecl.block.items.iter_mut() {
            if let ir::BlockItem::Target { num, stack } = stat {
                *stack = core::mem::take(
                    self.targs
                        .get_mut(num)
                        .expect("must have visited the basic block first"),
                );
            }
        }
    }
}

impl<'a> FunctionBodyVisitor for XirFunctionBodyVisitor<'a> {
    fn visit_basic_block(&mut self) -> Option<Box<dyn BasicBlockVisitor + '_>> {
        Some(Box::new(XirBasicBlockVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.fndecl,
            &mut self.targs,
            &mut self.var_heights,
            &mut self.ssa_tys,
            self.stack_height,
        )))
    }

    fn visit_inner_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>> {
        Some(Box::new(XirValueDefVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            self.file,
            self.properties,
        )))
    }
}

pub struct XirBasicBlockVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: u32,
}

impl<'a> XirBasicBlockVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
        }
    }
}

impl<'a> BasicBlockVisitor for XirBasicBlockVisitor<'a> {
    fn visit_id(&mut self, bb_id: BasicBlockId) {
        eprintln!("Entering Basic Block {}", bb_id);
        self.targs.get_or_insert_mut(bb_id, Vec::new());
        self.body.block.items.push(ir::BlockItem::Target {
            num: bb_id.id(),
            stack: vec![],
        })
    }

    fn visit_incoming_var(&mut self, incoming: SsaVarId) -> Option<Box<dyn TypeVisitor + '_>> {
        let mut height = self.stack_height;
        self.var_heights.insert(incoming, height);
        self.stack_height += 1;

        let ty = self.ssa_tys.get_or_insert_mut(incoming, ir::Type::Null);

        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            ty,
            self.properties,
        )))
    }

    fn visit_stmt(&mut self) -> Option<Box<dyn StatementVisitor + '_>> {
        Some(Box::new(XirStatementVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.body,
            self.targs,
            &mut self.var_heights,
            &mut self.ssa_tys,
            &mut self.stack_height,
        )))
    }

    fn visit_term(&mut self) -> Option<Box<dyn TerminatorVisitor + '_>> {
        Some(Box::new(XirTerminatorVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.body,
            self.targs,
            &mut self.var_heights,
            &mut self.ssa_tys,
            &mut self.stack_height,
        )))
    }
}

pub struct XirTerminatorVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
}

impl<'a> XirTerminatorVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            ssa_tys,
            var_heights,
            stack_height,
        }
    }
}

impl<'a> TerminatorVisitor for XirTerminatorVisitor<'a> {
    fn visit_call(&mut self) -> Option<Box<dyn CallVisitor + '_>> {
        Some(Box::new(XirCallVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.ssa_tys,
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
            self.ssa_tys,
            self.stack_height,
            ir::BranchCondition::Always,
        )))
    }

    fn visit_return(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirReturnVisitor(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        ))))
    }

    fn visit_branch(&mut self) -> Option<Box<dyn BranchVisitor + '_>> {
        Some(Box::new(XirBranchVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.ssa_tys,
            self.stack_height,
        )))
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

    fn visit_cast(&mut self) -> Option<Box<dyn CastVisitor + '_>> {
        self.0.visit_cast()
    }

    fn visit_const_string(&mut self) -> Option<Box<dyn ConstStringVisitor + '_>> {
        self.0.visit_const_string()
    }

    fn visit_var(&mut self, var: SsaVarId) {
        self.0.visit_var(var)
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn super::visitor::TupleExprVisitor + '_>> {
        self.0.visit_tuple()
    }

    fn visit_ctor(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>> {
        self.0.visit_ctor()
    }

    fn visit_field_subobject(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        self.0.visit_field_subobject()
    }

    fn visit_field_project(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        self.0.visit_field_project()
    }

    fn visit_binary_expr(&mut self) -> Option<Box<dyn BinaryExprVisitor + '_>> {
        self.0.visit_binary_expr()
    }

    fn visit_unary_expr(&mut self) -> Option<Box<dyn UnaryExprVisitor + '_>> {
        self.0.visit_unary_expr()
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

pub struct XirCallVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    retplace: Option<SsaVarId>,
    fnty: Option<ir::FnType>,
    late_invoke_intrin: Option<(IntrinsicDef, generics::GenericArgs)>,
}

impl<'a> XirCallVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            body,
            deftys,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
            retplace: None,
            fnty: None,
            properties,
            late_invoke_intrin: None,
        }
    }
}

impl<'a> CallVisitor for XirCallVisitor<'a> {
    fn visit_retplace(&mut self, retplace: SsaVarId) {
        self.retplace = Some(retplace);
    }

    fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
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
            self.defs,
            self.names,
            self.fnty.insert(ir::FnType::default()),
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_tailcall(&mut self) {
        let fnty = self.fnty.take().expect("visit_fnty must have been called");

        let is_never = fnty.ret == NEVER;

        if let Some((intrin, generics)) = self.late_invoke_intrin.take() {
            // Handle late bound intrinsics here
            match intrin {
                IntrinsicDef::__builtin_unreachable => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Invalid(
                            fnty.ret,
                        ))));
                }
                IntrinsicDef::impl_id => todo!("impl_id"),
                IntrinsicDef::type_id => todo!("type_id"),
                IntrinsicDef::type_name => todo!("type_name"),
                IntrinsicDef::destroy_at => todo!("destroy_at"),
                IntrinsicDef::discriminant => todo!("discriminant"),

                IntrinsicDef::construct_in_place => todo!("construct_in_place"),
                IntrinsicDef::__builtin_read => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_read_freeze => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Freeze,
                        )));
                }
                IntrinsicDef::__builtin_read_volatile => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Volatile,
                        )));
                }
                IntrinsicDef::__builtin_write => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Pivot(1, 1)));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Assign(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_write_volatile => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Pivot(1, 1)));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Assign(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_size_of => todo!(),
                IntrinsicDef::__builtin_align_of => todo!(),
                IntrinsicDef::__builtin_size_of_val => todo!(),
                IntrinsicDef::__builtin_align_of_val => todo!(),

                IntrinsicDef::__builtin_abort
                | IntrinsicDef::__builtin_allocate
                | IntrinsicDef::__builtin_deallocate
                | IntrinsicDef::transmute
                | IntrinsicDef::black_box
                | IntrinsicDef::__builtin_likely
                | IntrinsicDef::__builtin_unlikely => {
                    unreachable!("These are handled like regular functions")
                }
            }
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Exit { values: 1 }));
        } else {
            // Either we're compatible or we're `!` and don't return
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Tailcall(fnty)));
        }
    }

    fn visit_next(&mut self) -> Option<Box<dyn JumpVisitor + '_>> {
        let fnty = self.fnty.take().expect("visit_fnty must be called first");
        let retty = fnty.ret.clone();
        *self.stack_height -= fnty.params.len() as u32;

        if let Some((intrin, generics)) = self.late_invoke_intrin.take() {
            // Handle late bound intrinsics here
            // Assume that params are correct - yes, this will bork irgen if the params are wrong
            match intrin {
                IntrinsicDef::__builtin_unreachable => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Invalid(
                            retty.clone(),
                        ))));
                }
                IntrinsicDef::impl_id => todo!("impl_id"),
                IntrinsicDef::type_id => todo!("type_id"),
                IntrinsicDef::type_name => todo!("type_name"),
                IntrinsicDef::destroy_at => todo!("destroy_at"),
                IntrinsicDef::discriminant => todo!("discriminant"),

                IntrinsicDef::construct_in_place => todo!("construct_in_place"),
                IntrinsicDef::__builtin_read => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_read_freeze => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Freeze,
                        )));
                }
                IntrinsicDef::__builtin_read_volatile => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::AsRValue(
                            ir::AccessClass::Volatile,
                        )));
                }
                IntrinsicDef::__builtin_write => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Pivot(1, 1)));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Assign(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_write_volatile => {
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Pivot(1, 1)));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Indirect));
                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Assign(
                            ir::AccessClass::Normal,
                        )));
                }
                IntrinsicDef::__builtin_size_of => {
                    let ty = match generics.params.into_iter().next().unwrap() {
                        generics::GenericArg::Type(ty) => ty,
                        _ => unreachable!(),
                    };

                    let layout = self.defs.layout_of(&ty, DefId::ROOT, DefId::ROOT);

                    let size = layout
                        .size
                        .expect("__builtin_size_of requires a Sized type");

                    let intty = match &retty {
                        ir::Type::Scalar(sty) => *sty,
                        _ => unreachable!("__builtin_size_of returns `usize`"),
                    };

                    self.body
                        .block
                        .items
                        .push(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::Integer {
                            ty: intty,
                            val: size as u128,
                        })));
                }
                IntrinsicDef::__builtin_align_of => todo!(),
                IntrinsicDef::__builtin_size_of_val => todo!(),
                IntrinsicDef::__builtin_align_of_val => todo!(),

                IntrinsicDef::__builtin_abort
                | IntrinsicDef::__builtin_allocate
                | IntrinsicDef::__builtin_deallocate
                | IntrinsicDef::transmute
                | IntrinsicDef::black_box
                | IntrinsicDef::__builtin_likely
                | IntrinsicDef::__builtin_unlikely => unreachable!(),
            }
        } else {
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::CallFunction(fnty)));
        }

        Some(Box::new(XirNextBlockVisitor {
            inner: XirJumpVisitor::new(
                self.names,
                self.properties,
                self.body,
                self.targs,
                self.var_heights,
                self.ssa_tys,
                self.stack_height,
                ir::BranchCondition::Always,
            ),
            retty,
            targ: None,
        }))
    }

    fn visit_intrinsic(&mut self, intrin: IntrinsicDef, generics: &generics::GenericArgs) {
        let (item, fnty) = match intrin {
            // Rust sym calls
            func @ (IntrinsicDef::__builtin_allocate | IntrinsicDef::__builtin_deallocate) => {
                let layout_ty = self
                    .defs
                    .get_lang_item(LangItem::LayoutTy)
                    .expect("lang item `layout` is required to use `__builtin_alloc`");
                let u8_ptr = ty::Type::Pointer(
                    span::synthetic(Mutability::Mut),
                    Box::new(span::synthetic(ty::Type::Int(ty::IntType::u8))),
                );
                let (sym, sig) = match func {
                    IntrinsicDef::__builtin_allocate => {
                        let alloc_sym = self.defs.get_lang_item(LangItem::AllocSym).expect(
                            "lang item `alloc_symbol` is required to use `__builtin_alloc`",
                        );

                        let fnty = ty::FnType {
                            safety: span::synthetic(ty::Safety::Unsafe),
                            constness: span::synthetic(ty::Mutability::Mut),
                            asyncness: span::synthetic(ty::AsyncType::Normal),
                            tag: span::synthetic(ty::AbiTag::LCRust(None)),
                            retty: Box::new(span::synthetic(u8_ptr)),
                            paramtys: std::vec![span::synthetic(ty::Type::UserType(
                                layout_ty,
                                Default::default()
                            ))],
                            iscvarargs: span::synthetic(false),
                        };

                        (alloc_sym, fnty)
                    }
                    IntrinsicDef::__builtin_deallocate => {
                        let dealloc_sym = self.defs.get_lang_item(LangItem::DeallocSym).expect(
                            "lang item `deaalloc_symbol` is required to use `__builtin_alloc`",
                        );

                        let fnty = ty::FnType {
                            safety: span::synthetic(ty::Safety::Unsafe),
                            constness: span::synthetic(ty::Mutability::Mut),
                            asyncness: span::synthetic(ty::AsyncType::Normal),
                            tag: span::synthetic(ty::AbiTag::LCRust(None)),
                            retty: Box::new(span::synthetic(ty::Type::UNIT)),
                            paramtys: std::vec![
                                span::synthetic(ty::Type::UserType(layout_ty, Default::default())),
                                span::synthetic(u8_ptr),
                            ],
                            iscvarargs: span::synthetic(false),
                        };

                        (dealloc_sym, fnty)
                    }
                    _ => unreachable!(),
                };

                let name = self.names[&sym];
                let mut ir_fnty = ir::FnType::default();
                let fnty_vis =
                    XirFunctionTyVisitor::new(self.defs, self.names, &mut ir_fnty, self.properties);

                super::visitor::visit_fnty(fnty_vis, &sig, &self.defs);

                let path = ir::Path {
                    components: vec![ir::PathComponent::Text((&name).into())],
                };

                (path, ir_fnty)
            }

            // xlang intrinsics
            xlang_intrin @ (IntrinsicDef::__builtin_abort
            | IntrinsicDef::transmute
            | IntrinsicDef::black_box
            | IntrinsicDef::__builtin_likely
            | IntrinsicDef::__builtin_unlikely) => {
                let path = match xlang_intrin {
                    IntrinsicDef::__builtin_abort => {
                        ir::simple_path!(__lccc::intrinsics::C::__builtin_trap)
                    }
                    IntrinsicDef::transmute => {
                        ir::simple_path!(__lccc::intrinsics::Rust::__builtin_transmute)
                    }
                    IntrinsicDef::black_box => ir::simple_path!(__lccc::xlang::deoptimize),
                    IntrinsicDef::__builtin_likely => ir::simple_path!(__lccc::xlang::likely),
                    IntrinsicDef::__builtin_unlikely => ir::simple_path!(__lccc::xlang::unlikely),
                    _ => unreachable!(),
                };

                (
                    path,
                    self.fnty
                        .as_ref()
                        .cloned()
                        .expect("visit_fnty must have been called first"),
                )
            }

            // late bound intrinsics (Few of these should survive to irgen?)
            intrin => return self.late_invoke_intrin = Some((intrin, generics.clone())),
        };

        let ty = ir::Type::FnType((xlang::abi::boxed::Box::new(fnty)));

        let value = ir::Value::GlobalAddress { ty, item };

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Const(value)));
    }
}

pub struct XirNextBlockVisitor<'a> {
    inner: XirJumpVisitor<'a>,
    retty: ir::Type,
    targ: Option<BasicBlockId>,
}

impl<'a> JumpVisitor for XirNextBlockVisitor<'a> {
    fn visit_target_bb(&mut self, targbb: BasicBlockId) {
        self.targ = Some(targbb);
        self.inner.visit_target_bb(targbb)
    }

    fn visit_remap(&mut self, src: SsaVarId, targ: SsaVarId) {
        self.inner.visit_remap(src, targ)
    }
}

impl<'a> Drop for XirNextBlockVisitor<'a> {
    fn drop(&mut self) {
        self.inner
            .targs
            .get_or_insert_with_mut(
                self.targ.expect("visit_target_bb must have been called"),
                |_| Vec::new(),
            )
            .push(ir::StackItem {
                ty: core::mem::take(&mut self.retty),
                kind: ir::StackValueKind::RValue,
            });
    }
}

pub struct XirJumpVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    targ: Option<BasicBlockId>,
    cond: ir::BranchCondition,
}

impl<'a> XirJumpVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        cond: ir::BranchCondition,
    ) -> Self {
        Self {
            names,
            properties,
            body,
            targs,
            var_heights,
            stack_height,
            ssa_tys,
            targ: None,
            cond,
        }
    }
}

impl<'a> JumpVisitor for XirJumpVisitor<'a> {
    fn visit_target_bb(&mut self, targbb: BasicBlockId) {
        self.targ = Some(targbb);
    }

    fn visit_remap(&mut self, src: SsaVarId, dest: SsaVarId) {
        let targets = self
            .targs
            .get_or_insert_with_mut(self.targ.expect("wrong visit order"), |_| Vec::new());
        targets.push(ir::StackItem {
            ty: self.ssa_tys[&src].clone(),
            kind: ir::StackValueKind::RValue,
        });
    }
}

impl<'a> Drop for XirJumpVisitor<'a> {
    fn drop(&mut self) {
        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Branch {
                cond: ir::BranchCondition::Always,
                target: self.targ.unwrap().id(),
            }))
    }
}

pub struct XirBranchVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
}

impl<'a> XirBranchVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
        }
    }
}

impl<'a> BranchVisitor for XirBranchVisitor<'a> {
    fn visit_branch_arm(&mut self) -> Option<Box<dyn BranchArmVisitor + '_>> {
        Some(Box::new(XirBranchArmVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.ssa_tys,
            self.stack_height,
        )))
    }

    fn visit_else(&mut self) -> Option<Box<dyn JumpVisitor + '_>> {
        Some(Box::new(XirJumpVisitor::new(
            self.names,
            self.properties,
            self.body,
            self.targs,
            self.var_heights,
            self.ssa_tys,
            self.stack_height,
            ir::BranchCondition::Always,
        )))
    }
}

pub struct XirBranchArmVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
}

impl<'a> XirBranchArmVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
        }
    }
}

impl<'a> BranchArmVisitor for XirBranchArmVisitor<'a> {
    fn visit_cond(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
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
            self.ssa_tys,
            self.stack_height,
            ir::BranchCondition::NotEqual,
        )))
    }
}

pub struct XirExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
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
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
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
                ty: ir::ScalarType::default(),
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

    fn visit_cast(&mut self) -> Option<Box<dyn CastVisitor + '_>> {
        Some(Box::new(XirCastVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_const_string(&mut self) -> Option<Box<dyn ConstStringVisitor + '_>> {
        match self
            .body
            .block
            .items
            .push_mut(ir::BlockItem::Expr(ir::Expr::Const(ir::Value::String {
                encoding: ir::StringEncoding::Utf8,
                utf8: XLangString::new(),
                ty: ir::Type::Pointer(ir::PointerType {
                    inner: xlang::abi::boxed::Box::new(ir::Type::Scalar(ir::ScalarType {
                        header: ir::ScalarTypeHeader {
                            bitsize: 8,
                            vectorsize: XLangNone,
                            validity: ScalarValidity::empty(),
                        },
                        kind: ScalarTypeKind::Integer {
                            signed: false,
                            min: XLangNone,
                            max: XLangNone,
                        },
                    })),
                    ..Default::default()
                }),
            }))) {
            ir::BlockItem::Expr(ir::Expr::Const(ir::Value::String { utf8, .. })) => {
                Some(Box::new(XirConstStringVisitor::new(
                    self.names,
                    self.properties,
                    self.deftys,
                    self.targs,
                    self.var_heights,
                    self.stack_height,
                    utf8,
                )))
            }
            _ => unreachable!(),
        }
    }

    fn visit_var(&mut self, var: SsaVarId) {
        let depth = *self.stack_height - self.var_heights[&var];
        eprintln!(
            "Depth of {}: {} (var_height: {})",
            var, depth, self.var_heights[&var]
        );

        if depth != 0 {
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Pivot(1, depth)));
        }

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Dup(1)));
        if depth != 0 {
            self.body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Pivot(depth + 1, 1)));
        }
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn TupleExprVisitor + '_>> {
        Some(Box::new(XirTupleExprVisitor::new(
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_ctor(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>> {
        Some(Box::new(XirConstructorExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_field_subobject(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        Some(Box::new(XirFieldAccessVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
            ir::Expr::Member,
        )))
    }

    fn visit_field_project(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        Some(Box::new(XirFieldAccessVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
            ir::Expr::MemberIndirect,
        )))
    }

    fn visit_binary_expr(&mut self) -> Option<Box<dyn BinaryExprVisitor + '_>> {
        Some(Box::new(XirBinaryExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_unary_expr(&mut self) -> Option<Box<dyn UnaryExprVisitor + '_>> {
        Some(Box::new(XirUnaryExprVisitor::new(
            self.defs,
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

pub struct XirConstructorExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    fields: Vec<XLangString>,
    ctor_ty: DefId,
}

impl<'a> XirConstructorExprVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            fields: Vec::new(),
            ctor_ty: DefId::ROOT,
        }
    }
}

impl<'a> Drop for XirConstructorExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= self.fields.len() as u32;
        let ty = ir::Type::Named(into_path(self.names[&self.ctor_ty]));

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Aggregate(
                ir::AggregateCtor {
                    ty,
                    fields: core::mem::take(&mut self.fields),
                },
            )));
    }
}

impl<'a> ConstructorVisitor for XirConstructorExprVisitor<'a> {
    fn visit_ctor_def(&mut self, defid: DefId) {
        self.ctor_ty = defid;
        // TODO: Handle Enum constructors, which need to be nested.
    }

    fn visit_field(&mut self) -> Option<Box<dyn FieldInitVisitor + '_>> {
        let field = self.fields.push_mut(XLangString::new());
        Some(Box::new(XirConstructorFieldVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
            field,
        )))
    }

    fn visit_init(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        todo!()
    }
}

pub struct XirConstructorFieldVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    field: &'a mut XLangString,
}

impl<'a> XirConstructorFieldVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
        field: &'a mut XLangString,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            field,
        }
    }
}

impl<'a> FieldInitVisitor for XirConstructorFieldVisitor<'a> {
    fn visit_field(&mut self, field_name: &ty::FieldName) {
        let _ = write!(self.field, "{}", field_name);
    }

    fn visit_value(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
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

pub struct XirFieldAccessVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    ctor: fn(XLangString) -> ir::Expr,
    field: XLangString,
}

impl<'a> XirFieldAccessVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
        ctor: fn(XLangString) -> ir::Expr,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            ctor,
            field: XLangString::new(),
        }
    }
}

impl<'a> Drop for XirFieldAccessVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= 1;
        self.body
            .block
            .items
            .push(ir::BlockItem::Expr((self.ctor)(core::mem::take(
                &mut self.field,
            ))))
    }
}

impl<'a> FieldAccessVisitor for XirFieldAccessVisitor<'a> {
    fn visit_base(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }
    fn visit_field(&mut self, field_name: &ty::FieldName) {
        // TODO: variant fields
        let _ = write!(self.field, "{}", field_name);
    }
}

pub struct XirBinaryExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    op: Option<BinaryOp>,
}

impl<'a> XirBinaryExprVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            op: None,
        }
    }
}

impl<'a> BinaryExprVisitor for XirBinaryExprVisitor<'a> {
    fn visit_op(&mut self, op: BinaryOp) {
        self.op = Some(op);
    }

    fn visit_lhs(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_rhs(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
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

impl<'a> Drop for XirBinaryExprVisitor<'a> {
    fn drop(&mut self) {
        match self.op {
            Some(BinaryOp::Add) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::Add,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Sub) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::Sub,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Mul) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::Mul,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Div) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::Div,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Rem) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::Mod,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Less) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::CmpLt,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Greater) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::CmpGt,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            Some(BinaryOp::Equal) => {
                *self.stack_height -= 1;
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::BinaryOp(
                        ir::BinaryOp::CmpEq,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            None => panic!("operator wasn't set"),
            x => todo!("{:?}", x),
        }
    }
}

pub struct XirUnaryExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    op: Option<UnaryOp>,
}

impl<'a> XirUnaryExprVisitor<'a> {
    fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            op: None,
        }
    }
}

impl<'a> UnaryExprVisitor for XirUnaryExprVisitor<'a> {
    fn visit_op(&mut self, op: UnaryOp) {
        self.op = Some(op);
    }

    fn visit_lhs(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
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

impl<'a> Drop for XirUnaryExprVisitor<'a> {
    fn drop(&mut self) {
        match self.op {
            Some(UnaryOp::Neg) => {
                self.body
                    .block
                    .items
                    .push(ir::BlockItem::Expr(ir::Expr::UnaryOp(
                        ir::UnaryOp::Minus,
                        ir::OverflowBehaviour::Wrap,
                    )));
            }
            None => panic!("operator wasn't set"),
            x => todo!("{:?}", x),
        }
    }
}

pub struct XirTupleExprVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    next_field: u32,
    fields: Vec<XLangString>,
    elem_tys: Vec<ir::Type>,
}

impl<'a> XirTupleExprVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
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
            next_field: 0,
            fields: Vec::new(),
            elem_tys: Vec::new(),
        }
    }
}

impl<'a> TupleExprVisitor for XirTupleExprVisitor<'a> {
    fn visit_elem(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        todo!()
    }
}

impl<'a> Drop for XirTupleExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= self.next_field;
        let ty = ir::Type::Product(core::mem::take(&mut self.elem_tys));

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Aggregate(
                ir::AggregateCtor {
                    ty,
                    fields: core::mem::take(&mut self.fields),
                },
            )))
    }
}

pub struct XirConstIntVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
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
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
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

pub struct XirConstStringVisitor<'a> {
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    string: &'a mut XLangString,
}

impl<'a> XirConstStringVisitor<'a> {
    pub fn new(
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
        string: &'a mut XLangString,
    ) -> Self {
        Self {
            names,
            properties,
            deftys,
            targs,
            var_heights,
            stack_height,
            string,
        }
    }
}

impl<'a> ConstStringVisitor for XirConstStringVisitor<'a> {
    fn visit_string_type(&mut self, st: StringType) {}

    fn visit_value(&mut self, val: Symbol) {
        *self.string = (&*val).into();
    }
}

pub struct XirCastVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    stack_height: &'a mut u32,
    as_ty: Option<ir::Type>,
}

impl<'a> XirCastVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            body,
            targs,
            var_heights,
            stack_height,
            as_ty: None,
        }
    }
}

impl<'a> CastVisitor for XirCastVisitor<'a> {
    fn visit_inner(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.body,
            self.targs,
            self.var_heights,
            self.stack_height,
        )))
    }

    fn visit_cast_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            self.as_ty.insert(ir::Type::Null),
            self.properties,
        )))
    }
}

impl<'a> Drop for XirCastVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= 1;

        self.body
            .block
            .items
            .push(ir::BlockItem::Expr(ir::Expr::Convert(
                ir::ConversionStrength::Reinterpret,
                self.as_ty
                    .take()
                    .expect("visit_cast_type must have been called"),
            )));
    }
}

pub struct XirStatementVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
}

impl<'a> XirStatementVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
        }
    }
}

impl<'a> StatementVisitor for XirStatementVisitor<'a> {
    fn visit_let(&mut self) -> Option<Box<dyn LetStatementVisitor + '_>> {
        Some(Box::new(XirLetVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.body,
            self.targs,
            self.var_heights,
            self.ssa_tys,
            self.stack_height,
        )))
    }

    fn visit_store_dead(&mut self, _: SsaVarId) {
        // Be more intelligent in the future
    }

    fn visit_discard(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirDiscardVisitor(
            XirExprVisitor::new(
                self.defs,
                self.names,
                self.properties,
                self.deftys,
                self.body,
                self.targs,
                self.var_heights,
                self.stack_height,
            ),
            true,
        )))
    }
}

pub struct XirDiscardVisitor<'a>(XirExprVisitor<'a>, bool);

impl<'a> ExprVisitor for XirDiscardVisitor<'a> {
    fn visit_unreachable(&mut self) {
        self.0.visit_unreachable()
    }

    fn visit_const_int(&mut self) -> Option<Box<dyn ConstIntVisitor + '_>> {
        self.0.visit_const_int()
    }

    fn visit_const(&mut self, defid: DefId) {
        self.0.visit_const(defid)
    }

    fn visit_cast(&mut self) -> Option<Box<dyn CastVisitor + '_>> {
        self.0.visit_cast()
    }

    fn visit_const_string(&mut self) -> Option<Box<dyn ConstStringVisitor + '_>> {
        self.0.visit_const_string()
    }

    fn visit_var(&mut self, var: crate::sema::mir::SsaVarId) {
        // We can honestly no-op, but destructor currently pops *something*
        self.1 = false;
    }

    fn visit_tuple(&mut self) -> Option<Box<dyn super::visitor::TupleExprVisitor + '_>> {
        self.0.visit_tuple()
    }

    fn visit_ctor(&mut self) -> Option<Box<dyn ConstructorVisitor + '_>> {
        self.0.visit_ctor()
    }

    fn visit_field_subobject(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        self.0.visit_field_subobject()
    }

    fn visit_field_project(&mut self) -> Option<Box<dyn FieldAccessVisitor + '_>> {
        self.0.visit_field_project()
    }

    fn visit_binary_expr(&mut self) -> Option<Box<dyn BinaryExprVisitor + '_>> {
        self.0.visit_binary_expr()
    }

    fn visit_unary_expr(&mut self) -> Option<Box<dyn UnaryExprVisitor + '_>> {
        self.0.visit_unary_expr()
    }
}

impl<'a> Drop for XirDiscardVisitor<'a> {
    fn drop(&mut self) {
        if self.1 {
            *self.0.stack_height -= 1;
            self.0
                .body
                .block
                .items
                .push(ir::BlockItem::Expr(ir::Expr::Pop(1)));
        }
    }
}

pub struct XirLetVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    body: &'a mut ir::FunctionBody,
    targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    varid: Option<SsaVarId>,
}

impl<'a> XirLetVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        body: &'a mut ir::FunctionBody,
        targs: &'a mut HashMap<BasicBlockId, Vec<ir::StackItem>>,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            body,
            targs,
            var_heights,
            ssa_tys,
            stack_height,
            varid: None,
        }
    }
}

impl<'a> LetStatementVisitor for XirLetVisitor<'a> {
    fn visit_var(&mut self, var: SsaVarId) {
        self.varid = Some(var);
    }

    fn visit_var_ty(&mut self) -> Option<Box<dyn TypeVisitor + '_>> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            self.ssa_tys
                .get_or_insert_mut(self.varid.unwrap(), ir::Type::default()),
            self.properties,
        )))
    }

    fn visit_init(&mut self) -> Option<Box<dyn ExprVisitor + '_>> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
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

impl<'a> Drop for XirLetVisitor<'a> {
    fn drop(&mut self) {
        let varid = self.varid.take().expect("visit_var must have been called");
        self.var_heights.insert(varid, *self.stack_height);
    }
}
