use core::fmt::Write;

use xlang::ir::{
    self, AggregateCtor, AggregateField, AggregateFieldSpecifier, AnnotatedElement, ScalarTypeKind,
    ScalarValidity,
};
use xlang::prelude::v1::{HashMap, Pair};
use xlang::targets::properties::TargetProperties;
use xlang::{abi::boxed::Box as XLangBox, abi::string::String as XLangString, abi::vec::Vec, vec};
use xlang::{
    abi::{
        self,
        option::{None as XLangNone, Some as XLangSome},
    },
    ir::PathComponent,
};

use crate::lex::CharType;
use crate::sema::mir;
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
    ArrayTyVisitor, AttrVisitor, BasicBlockVisitor, BinaryExprVisitor, BranchVisitor, CallVisitor,
    CastVisitor, ConstCharVisitor, ConstIntVisitor, ConstStringVisitor, ConstructorDefVisitor,
    ConstructorVisitor, ExprVisitor, FieldAccessVisitor, FieldInitVisitor, FieldVisitor,
    FunctionBodyVisitor, FunctionDefVisitor, FunctionTyVisitor, IntTyVisitor, JumpVisitor,
    LetStatementVisitor, ModVisitor, PointerTyVisitor, ReferenceTyVisitor, StatementVisitor,
    TerminatorVisitor, TupleExprVisitor, TupleTyVisitor, TypeDefVisitor, TypeVisitor,
    UnaryExprVisitor, ValueDefVisitor,
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

    fn visit_submodule(&mut self) -> Option<impl ModVisitor + '_> {
        Some(self)
    }

    fn visit_type(&mut self) -> Option<impl TypeDefVisitor + '_> {
        Some(Box::new(XirTypeDefVisitor::new(
            self.defs,
            self.names,
            self.deftys,
            &mut self.file.root,
            self.properties,
        )))
    }

    fn visit_value(&mut self) -> Option<impl ValueDefVisitor + '_> {
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

    fn visit_submodule(&mut self) -> Option<impl ModVisitor + '_> {
        Some(self)
    }

    fn visit_type(&mut self) -> Option<impl TypeDefVisitor + '_> {
        None::<()>
    }

    fn visit_value(&mut self) -> Option<impl ValueDefVisitor + '_> {
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

    fn visit_attr(&mut self) -> Option<impl AttrVisitor + '_> {
        None::<()>
    }

    fn visit_function(&mut self) -> Option<impl FunctionDefVisitor + '_> {
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

    fn visit_attr(&mut self) -> Option<impl AttrVisitor + '_> {
        // don't care yet
        None::<()>
    }

    fn visit_function(&mut self) -> Option<impl FunctionDefVisitor + '_> {
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
    fn visit_fnty(&mut self) -> Option<impl FunctionTyVisitor + '_> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.defs,
            self.names,
            self.fnty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<impl FunctionBodyVisitor + '_> {
        None::<()>
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

    fn visit_attr(&mut self) -> Option<impl AttrVisitor + '_> {
        None::<()>
    }

    fn visit_struct(&mut self) -> Option<impl ConstructorDefVisitor + '_> {
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
    fn visit_field(&mut self) -> Option<impl FieldVisitor + '_> {
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

    fn visit_ty(&mut self) -> Option<impl TypeVisitor + '_> {
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
    fn visit_fnty(&mut self) -> Option<impl FunctionTyVisitor + '_> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.defs,
            self.names,
            &mut self.fndef.ty,
            self.properties,
        )))
    }

    fn visit_fnbody(&mut self) -> Option<impl FunctionBodyVisitor + '_> {
        Some(Box::new(XirFunctionBodyVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
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

    fn visit_return(&mut self) -> Option<impl TypeVisitor + '_> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.fnty.ret,
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<impl TypeVisitor + '_> {
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

const CHAR: ir::Type = ir::Type::Scalar(ir::ScalarType {
    kind: ScalarTypeKind::Char {
        flags: ir::CharFlags::UNICODE,
    },
    header: ir::ScalarTypeHeader {
        bitsize: 32,
        vectorsize: XLangNone,
        validity: ir::ScalarValidity::empty(),
    },
});

const BOOL: ir::ScalarType = ir::ScalarType {
    kind: ir::ScalarTypeKind::Integer {
        signed: false,
        min: XLangNone,
        max: XLangNone,
    },
    header: ir::ScalarTypeHeader {
        bitsize: 1,
        vectorsize: XLangNone,
        validity: ir::ScalarValidity::empty(),
    },
};

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
    fn visit_array(&mut self) -> Option<impl ArrayTyVisitor + '_> {
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

    fn visit_int(&mut self) -> Option<impl IntTyVisitor + '_> {
        *self.ty = ir::Type::Scalar(ir::ScalarType::default());

        if let ir::Type::Scalar(sty) = self.ty {
            Some(Box::new(XirIntTyVisitor::new(sty, self.properties)))
        } else {
            unreachable!()
        }
    }

    fn visit_pointer(&mut self) -> Option<impl PointerTyVisitor + '_> {
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

    fn visit_reference(&mut self) -> Option<impl ReferenceTyVisitor + '_> {
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

    fn visit_tuple(&mut self) -> Option<impl TupleTyVisitor + '_> {
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
    fn visit_type(&mut self) -> Option<impl TypeVisitor + '_> {
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
                cx::ConstExpr::MirVal(_) => todo!("Evaluate Complex Consts"),
                cx::ConstExpr::Param(_) => panic!("Unexpanded generics"),
                cx::ConstExpr::IntConst(_, val) => *val,
                cx::ConstExpr::Const(_, _) => todo!("const item"),
                cx::ConstExpr::Constructor(_) => todo!("constructor"),
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

    fn visit_type(&mut self) -> Option<impl TypeVisitor + '_> {
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

    fn visit_type(&mut self) -> Option<impl TypeVisitor + '_> {
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
    fn visit_type(&mut self) -> Option<impl TypeVisitor + '_> {
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
    cur_fnty: &'a mut ir::FnType,
    fndecl: &'a mut ir::FunctionBody,
    ssa_tys: HashMap<SsaVarId, ir::Type>,
}

impl<'a> XirFunctionBodyVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        fndecl: &'a mut ir::FunctionBody,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            fndecl,
            ssa_tys: HashMap::new(),
        }
    }
}

impl<'a> FunctionBodyVisitor for XirFunctionBodyVisitor<'a> {
    fn visit_inner_value(&mut self) -> Option<impl ValueDefVisitor + '_> {
        None::<()>
    }

    fn visit_basic_block(&mut self) -> Option<impl BasicBlockVisitor + '_> {
        Some(Box::new(XirBasicBlockVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.fndecl.blocks.push_mut(ir::Block::default()),
            &mut self.fndecl.locals,
            &mut self.ssa_tys,
        )))
    }
}

pub struct XirBasicBlockVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    block: &'a mut ir::Block,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: u32,
    var_heights: HashMap<SsaVarId, u32>,
    var_stack: Vec<SsaVarId>,
}

impl<'a> XirBasicBlockVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        block: &'a mut ir::Block,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            block,
            locals,
            ssa_tys,
            stack_height: 0,
            var_heights: HashMap::new(),
            var_stack: Vec::new(),
        }
    }
}

impl<'a> BasicBlockVisitor for XirBasicBlockVisitor<'a> {
    fn visit_id(&mut self, id: mir::BasicBlockId) {
        self.block.target = id.id();
    }

    fn visit_incoming_var(&mut self, incoming: SsaVarId) -> Option<impl TypeVisitor + '_> {
        let height = self.stack_height;
        self.stack_height += 1;
        self.var_heights.insert(incoming, height);

        self.var_stack.push(incoming);

        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self
                .block
                .incoming_stack
                .push_mut(ir::StackItem {
                    ty: ir::Type::Null,
                    kind: ir::StackValueKind::RValue,
                })
                .ty,
            self.properties,
        )))
    }

    fn visit_stmt(&mut self) -> Option<impl StatementVisitor + '_> {
        Some(Box::new(XirStatementVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            &mut self.stack_height,
            &mut self.var_heights,
            &mut self.var_stack,
        )))
    }

    fn visit_term(&mut self) -> Option<impl TerminatorVisitor + '_> {
        Some(Box::new(XirTerminatorVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.block,
            self.locals,
            self.ssa_tys,
            &mut self.stack_height,
            &mut self.var_heights,
            &mut self.var_stack,
        )))
    }
}

pub struct XirTerminatorVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    block: &'a mut ir::Block,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
}

impl<'a> XirTerminatorVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        block: &'a mut ir::Block,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            block,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
        }
    }
}

impl<'a> TerminatorVisitor for XirTerminatorVisitor<'a> {
    fn visit_branch(&mut self) -> Option<impl BranchVisitor + '_> {
        let (iftarg, elsetarg) = {
            self.block.term = ir::Terminator::Branch(
                ir::BranchCondition::NotEqual,
                ir::JumpTarget {
                    target: !0,
                    flags: ir::JumpTargetFlags::empty(),
                },
                ir::JumpTarget {
                    target: !0,
                    flags: ir::JumpTargetFlags::empty(),
                },
            );

            match &mut self.block.term {
                ir::Terminator::Branch(_, iftarg, elsetarg) => (iftarg, elsetarg),
                _ => unsafe { core::hint::unreachable_unchecked() },
            }
        };

        Some(Box::new(XirBranchVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            iftarg,
            elsetarg,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_call(&mut self) -> Option<impl CallVisitor + '_> {
        Some(Box::new(XirCallVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.block,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_jump(&mut self) -> Option<impl JumpVisitor + '_> {
        let targ = {
            self.block.term = ir::Terminator::Jump(ir::JumpTarget {
                target: !0,
                flags: ir::JumpTargetFlags::empty(),
            });

            match &mut self.block.term {
                ir::Terminator::Jump(targ) => targ,
                _ => unsafe { core::hint::unreachable_unchecked() },
            }
        };

        Some(Box::new(XirJumpVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            targ,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
            0,
            false,
        )))
    }

    fn visit_return(&mut self) -> Option<impl ExprVisitor + '_> {
        self.block.term = ir::Terminator::Exit(1);
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
    fn visit_unreachable(&mut self) {
        self.block.term = ir::Terminator::Unreachable;
    }
}

pub struct XirJumpVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    targ: &'a mut ir::JumpTarget,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    volatile_vals: u32,
    remapped_var_count: u32,
    skip_setting_remaps: bool,
}

impl<'a> XirJumpVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        targ: &'a mut ir::JumpTarget,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
        volatile_vals: u32,
        skip_setting_remaps: bool,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            targ,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            volatile_vals,
            remapped_var_count: 0,
            skip_setting_remaps,
        }
    }
}

impl<'a> JumpVisitor for XirJumpVisitor<'a> {
    fn visit_target_bb(&mut self, targbb: mir::BasicBlockId) {
        self.targ.target = targbb.id();
    }

    fn visit_remap(&mut self, src: mir::SsaVarId, _: mir::SsaVarId) {
        if self.skip_setting_remaps {
            return;
        }
        let height = self.var_heights.remove(&src).unwrap().1;
        let i = self
            .var_stack
            .iter()
            .enumerate()
            .skip_while(|(_, v)| *v != &src)
            .map(|(off, _)| off)
            .next()
            .unwrap();

        self.var_stack.remove(i);

        for var in &self.var_stack[i..] {
            self.var_heights[var] -= 1;
        }

        let depth = (*self.stack_height) - (height + 1);

        if depth != 0 {
            self.exprs.push(ir::Expr::Pivot(1, depth));
        }
        self.remapped_var_count += 1;
    }

    fn visit_fallthrough(&mut self) {
        self.targ.flags |= ir::JumpTargetFlags::FALLTHROUGH;
    }
}

impl<'a> Drop for XirJumpVisitor<'a> {
    fn drop(&mut self) {
        if self.remapped_var_count != 0 && self.volatile_vals != 0 {
            self.exprs
                .push(ir::Expr::Pivot(self.volatile_vals, self.remapped_var_count));
        }
    }
}

pub struct XirBranchVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    iftarg: &'a mut ir::JumpTarget,
    elsetarg: &'a mut ir::JumpTarget,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
}

impl<'a> XirBranchVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        iftarg: &'a mut ir::JumpTarget,
        elsetarg: &'a mut ir::JumpTarget,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            iftarg,
            elsetarg,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
        }
    }
}

impl<'a> BranchVisitor for XirBranchVisitor<'a> {
    fn visit_cond(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
    fn visit_if_arm(&mut self) -> Option<impl JumpVisitor + '_> {
        Some(Box::new(XirJumpVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.iftarg,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
            1,
            false,
        )))
    }
    fn visit_else(&mut self) -> Option<impl JumpVisitor + '_> {
        Some(Box::new(XirJumpVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.elsetarg,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
            1,
            true,
        )))
    }
}

pub struct XirIntrinsicBodyVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    block: &'a mut ir::Block,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: HashMap<SsaVarId, u32>,
    var_stack: Vec<SsaVarId>,
}

impl<'a> XirIntrinsicBodyVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        block: &'a mut ir::Block,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            block,
            locals,
            ssa_tys,
            stack_height,
            var_heights: HashMap::new(),
            var_stack: Vec::new(),
        }
    }

    pub fn set_param_count(&mut self, params: u32) {
        let first_param = *self.stack_height - params;

        for (var, height) in (0..params).zip(first_param..) {
            let varid = SsaVarId::__new_unchecked(var);
            self.var_heights.insert(varid, height);
            self.var_stack.push(varid);
        }
    }
}

impl<'a> BasicBlockVisitor for XirIntrinsicBodyVisitor<'a> {
    fn visit_id(&mut self, _: mir::BasicBlockId) {}
    fn visit_incoming_var(&mut self, _: SsaVarId) -> Option<impl TypeVisitor + '_> {
        None::<()>
    }
    fn visit_stmt(&mut self) -> Option<impl StatementVisitor + '_> {
        Some(Box::new(XirStatementVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            &mut self.var_heights,
            &mut self.var_stack,
        )))
    }

    fn visit_term(&mut self) -> Option<impl TerminatorVisitor + '_> {
        Some(Box::new(self))
    }
}

impl<'a> TerminatorVisitor for XirIntrinsicBodyVisitor<'a> {
    fn visit_return(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            &mut self.var_heights,
            &mut self.var_stack,
        )))
    }

    fn visit_branch(&mut self) -> Option<impl BranchVisitor + '_> {
        None::<()>
    }

    fn visit_call(&mut self) -> Option<impl CallVisitor + '_> {
        None::<()>
    }

    fn visit_jump(&mut self) -> Option<impl JumpVisitor + '_> {
        None::<()>
    }
    fn visit_unreachable(&mut self) {
        self.block.term = ir::Terminator::Unreachable;
    }
}

pub struct XirCallVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    block: &'a mut ir::Block,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    late_bound_intrinsic: Option<IntrinsicDef>,
    fnty: ir::FnType,
    targ: Option<ir::JumpTarget>,
    param_count: u32,
}

impl<'a> XirCallVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        block: &'a mut ir::Block,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            block,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            late_bound_intrinsic: None,
            fnty: ir::FnType::default(),
            targ: None,
            param_count: 0,
        }
    }
}

impl<'a> CallVisitor for XirCallVisitor<'a> {
    fn visit_retplace(&mut self, _: mir::SsaVarId) {}

    fn visit_target(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_fnty(&mut self) -> Option<impl FunctionTyVisitor + '_> {
        Some(Box::new(XirFunctionTyVisitor::new(
            self.defs,
            self.names,
            &mut self.fnty,
            self.properties,
        )))
    }

    fn visit_param(&mut self) -> Option<impl ExprVisitor + '_> {
        self.param_count += 1;
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            &mut self.block.expr,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_next(&mut self) -> Option<impl JumpVisitor + '_> {
        if self.late_bound_intrinsic.is_some() {
            if let ir::Terminator::Empty = self.block.term {
                let param_count = self.param_count;
                self.block.expr.push(ir::Expr::Pivot(1, param_count));
                self.block.expr.push(ir::Expr::Pop(param_count));
                self.block.term = ir::Terminator::Jump(ir::JumpTarget::default());
                match &mut self.block.term {
                    ir::Terminator::Jump(targ) => Some(Box::new(XirJumpVisitor::new(
                        self.defs,
                        self.names,
                        self.properties,
                        self.deftys,
                        self.cur_fnty,
                        &mut self.block.expr,
                        targ,
                        self.locals,
                        self.ssa_tys,
                        self.stack_height,
                        self.var_heights,
                        self.var_stack,
                        self.param_count + 1,
                        false,
                    ))),
                    _ => unreachable!(),
                }
            } else {
                None // Another terminator was set by the intrinsic body, we're not interested in jumping to the next block
            }
        } else {
            self.block.term = ir::Terminator::Call(
                ir::CallFlags::empty(),
                XLangBox::new(core::mem::take(&mut self.fnty)),
                ir::JumpTarget::default(),
            );
            match &mut self.block.term {
                ir::Terminator::Call(_, _, targ) => Some(Box::new(XirJumpVisitor::new(
                    self.defs,
                    self.names,
                    self.properties,
                    self.deftys,
                    self.cur_fnty,
                    &mut self.block.expr,
                    targ,
                    self.locals,
                    self.ssa_tys,
                    self.stack_height,
                    self.var_heights,
                    self.var_stack,
                    self.param_count + 1,
                    false,
                ))),
                _ => unreachable!(),
            }
        }
    }

    fn visit_intrinsic(
        &mut self,
        intrin: IntrinsicDef,
        generics: &generics::GenericArgs,
    ) -> Option<impl BasicBlockVisitor + '_> {
        use IntrinsicDef::*;
        match intrin {
            __builtin_assume
            | __builtin_abort
            | transmute
            | black_box
            | construct_in_place
            | __builtin_likely
            | __builtin_unlikely
            | __atomic_read_in_transaction
            | __atomic_write_in_transaction
            | __atomic_commit_transaction
            | __builtin_va_arg
            | __builtin_va_copy
            | __builtin_va_end
            | __builtin_max
            | __builtin_min => {
                let ty = ir::Type::FnType(XLangBox::new(self.fnty.clone()));
                let path = match intrin {
                    __builtin_assume => {
                        ir::simple_path!(__lccc::xlang::assume)
                    }
                    __builtin_abort => ir::simple_path!(__lccc::xlang::abort),
                    transmute => todo!(),
                    black_box => ir::simple_path!(__lccc::xlang::black_box),
                    construct_in_place => todo!(),
                    __builtin_likely => ir::simple_path!(__lccc::xlang::likely),
                    __builtin_unlikely => ir::simple_path!(__lccc::xlang::unlikely),
                    __atomic_read_in_transaction => todo!(),
                    __atomic_write_in_transaction => todo!(),
                    __atomic_commit_transaction => todo!(),
                    __builtin_va_arg => todo!(),
                    __builtin_va_copy => todo!(),
                    __builtin_va_end => todo!(),
                    __builtin_max => todo!(),
                    __builtin_min => todo!(),
                    _ => todo!("Eager bound intrinsic {intrin}"),
                };

                None
            }

            intrin => {
                self.late_bound_intrinsic = Some(intrin);
                match intrin {
                    __atomic_load => todo!(),
                    __atomic_store => todo!(),
                    __atomic_compare_exchange_strong => todo!(),
                    __atomic_compare_exchange_weak => todo!(),
                    __atomic_swap => todo!(),
                    __atomic_fetch_add => todo!(),
                    __atomic_fetch_sub => todo!(),
                    __builtin_read => todo!(),
                    __builtin_read_freeze => todo!(),
                    __builtin_read_volatile => todo!(),
                    __builtin_write => todo!(),
                    __builtin_write_volatile => todo!(),
                    __builtin_cmp => todo!(),
                    __builtin_fadd_fast => todo!(),
                    __builtin_fsub_fast => todo!(),
                    __builtin_fmul_fast => todo!(),
                    __builtin_fdiv_fast => todo!(),
                    __builtin_frem_fast => todo!(),
                    __builtin_ffma_fast => todo!(),
                    __builtin_fneg_fast => todo!(),

                    _ => Some(Box::new(XirIntrinsicBodyVisitor::new(
                        self.defs,
                        self.names,
                        self.properties,
                        self.deftys,
                        self.cur_fnty,
                        self.block,
                        self.locals,
                        self.ssa_tys,
                        self.stack_height,
                    ))),
                }
            }
        }
    }

    fn visit_tailcall(&mut self) {
        if self.late_bound_intrinsic.is_some() {
            if let ir::Terminator::Empty = self.block.term {
                self.block.term = ir::Terminator::Exit(1)
            }
        } else {
            self.block.term = ir::Terminator::Tailcall(
                ir::CallFlags::empty(),
                XLangBox::new(core::mem::take(&mut self.fnty)),
            );
        }
    }

    fn visit_unwind(&mut self) -> Option<impl JumpVisitor + '_> {
        None::<()>
    }
}

pub struct XirStatementVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
}

impl<'a> XirStatementVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
        }
    }
}

impl<'a> StatementVisitor for XirStatementVisitor<'a> {
    fn visit_let(&mut self) -> Option<impl LetStatementVisitor + '_> {
        Some(Box::new(XirLetStatementVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_store_dead(&mut self, _: mir::SsaVarId) {}

    fn visit_discard(&mut self) -> Option<impl ExprVisitor + '_> {
        None::<()>
    }
}

pub struct XirLetStatementVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    var: SsaVarId,
}

impl<'a> XirLetStatementVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            var: SsaVarId::INVALID,
        }
    }
}

impl<'a> LetStatementVisitor for XirLetStatementVisitor<'a> {
    fn visit_var(&mut self, var: mir::SsaVarId) {
        self.var = var;
        self.var_heights.insert(var, *self.stack_height);
        self.var_stack.push(var);
    }

    fn visit_var_ty(&mut self) -> Option<impl TypeVisitor + '_> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            self.ssa_tys.get_or_insert_mut(self.var, ir::Type::Null),
            self.properties,
        )))
    }

    fn visit_init(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

pub struct XirExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
}

impl<'a> XirExprVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
        }
    }
}

impl<'a> ExprVisitor for XirExprVisitor<'a> {
    fn visit_unreachable(&mut self) {
        todo!()
    }

    fn visit_const_int(&mut self) -> Option<impl ConstIntVisitor + '_> {
        let (intty, val) = match self.exprs.push_mut(ir::Expr::Const(ir::Value::Integer {
            ty: ir::ScalarType::default(),
            val: 0,
        })) {
            ir::Expr::Const(ir::Value::Integer { ty, val }) => (ty, val),
            _ => unsafe { core::hint::unreachable_unchecked() },
        };
        Some(Box::new(XirConstIntVisitor::new(
            self.properties,
            val,
            intty,
        )))
    }

    fn visit_const_char(&mut self) -> Option<impl ConstCharVisitor + '_> {
        let (intty, val) = match self.exprs.push_mut(ir::Expr::Const(ir::Value::Integer {
            ty: ir::ScalarType::default(),
            val: 0,
        })) {
            ir::Expr::Const(ir::Value::Integer { ty, val }) => (ty, val),
            _ => unsafe { core::hint::unreachable_unchecked() },
        };
        Some(Box::new(XirConstIntVisitor::new(
            self.properties,
            val,
            intty,
        )))
    }

    fn visit_const(&mut self, defid: DefId) {
        let name = self.names[&defid];

        let path = ir::Path {
            components: vec![ir::PathComponent::Text((&name).into())],
        };

        let ty = self.deftys[&defid].clone();

        self.exprs
            .push(ir::Expr::Const(ir::Value::GlobalAddress { ty, item: path }));
    }

    fn visit_cast(&mut self) -> Option<impl CastVisitor + '_> {
        Some(Box::new(XirCastVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_const_string(&mut self) -> Option<impl ConstStringVisitor + '_> {
        let (ty, val) = match self.exprs.push_mut(ir::Expr::Const(ir::Value::String {
            encoding: ir::StringEncoding::Utf8,
            ty: ir::Type::default(),
            utf8: XLangString::new(),
        })) {
            ir::Expr::Const(ir::Value::String { ty, utf8, .. }) => (ty, utf8),
            _ => unsafe { core::hint::unreachable_unchecked() },
        };
        Some(Box::new(XirConstStringVisitor::new(
            self.properties,
            val,
            ty,
        )))
    }

    fn visit_var(&mut self, var: mir::SsaVarId) {
        let height = self.var_heights[&var];

        let depth = (*self.stack_height) - (height + 1);

        if depth != 0 {
            self.exprs.push(ir::Expr::Pivot(1, depth));
        }
        self.exprs.push(ir::Expr::Dup(1));
        if depth != 0 {
            self.exprs.push(ir::Expr::Pivot(depth + 1, 1));
        }
    }

    fn visit_tuple(&mut self) -> Option<impl TupleExprVisitor + '_> {
        Some(Box::new(XirTupleVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_ctor(&mut self) -> Option<impl ConstructorVisitor + '_> {
        Some(Box::new(XirConstructorVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_field_subobject(&mut self) -> Option<impl FieldAccessVisitor + '_> {
        Some(Box::new(XirFieldSubobjectVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_field_project(&mut self) -> Option<impl FieldAccessVisitor + '_> {
        None::<()>
    }

    fn visit_binary_expr(&mut self) -> Option<impl BinaryExprVisitor + '_> {
        Some(Box::new(XirBinaryExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_unary_expr(&mut self) -> Option<impl UnaryExprVisitor + '_> {
        Some(Box::new(XirUnaryExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

impl<'a> Drop for XirExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height += 1;
    }
}

pub struct XirFieldSubobjectVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
}

impl<'a> XirFieldSubobjectVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
        }
    }
}

impl<'a> FieldAccessVisitor for XirFieldSubobjectVisitor<'a> {
    fn visit_base(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_field(&mut self, field_name: &ty::FieldName) {
        *self.stack_height -= 1;
        self.exprs.push(ir::Expr::Member(match field_name {
            ty::FieldName::Field(x) => x.to_string().into(),
            x => todo!("{:?}", x),
        }))
    }
}

pub struct XirTupleVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    tys: Vec<ir::Type>,
}

impl<'a> XirTupleVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            tys: vec![],
        }
    }
}

impl<'a> TupleExprVisitor for XirTupleVisitor<'a> {
    fn visit_elem(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

impl<'a> Drop for XirTupleVisitor<'a> {
    fn drop(&mut self) {
        let fields = self
            .tys
            .iter()
            .zip(0..)
            .map(|(_, x)| xlang::abi::format!("{}", x))
            .collect::<Vec<_>>();

        *self.stack_height -= fields.len() as u32;

        self.exprs.push(ir::Expr::Aggregate(ir::AggregateCtor {
            ty: ir::Type::Product(core::mem::take(&mut self.tys)),
            fields,
        }));
    }
}

pub struct XirConstructorVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    ty: Option<ir::Type>,
    fields: Vec<String>,
}

impl<'a> XirConstructorVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            ty: None,
            fields: vec![],
        }
    }
}

impl<'a> ConstructorVisitor for XirConstructorVisitor<'a> {
    fn visit_ctor_def(&mut self, defid: DefId) {
        self.ty = Some(ir::Type::Named(ir::Path {
            components: vec![ir::PathComponent::Text(
                self.names[&defid].to_string().into(),
            )],
        }));
    }

    fn visit_field(&mut self) -> Option<impl FieldInitVisitor + '_> {
        Some(Box::new(XirFieldInitVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
            &mut self.fields,
        )))
    }

    fn visit_init(&mut self) -> Option<impl ExprVisitor + '_> {
        None::<()>
    }
}

impl<'a> Drop for XirConstructorVisitor<'a> {
    fn drop(&mut self) {
        let fields = std::mem::replace(&mut self.fields, vec![]);
        *self.stack_height -= self.fields.len() as u32;

        self.exprs.push(ir::Expr::Aggregate(ir::AggregateCtor {
            ty: self
                .ty
                .take()
                .expect("ConstructorVisitor::visit_ty was never called"),
            fields: fields.into_iter().map(String::into).collect(),
        }));
    }
}

pub struct XirFieldInitVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    fields: &'a mut Vec<String>,
}

impl<'a> XirFieldInitVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
        fields: &'a mut Vec<String>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            fields,
        }
    }
}

impl<'a> FieldInitVisitor for XirFieldInitVisitor<'a> {
    fn visit_field(&mut self, field_name: &ty::FieldName) {
        self.fields.push(match field_name {
            ty::FieldName::Field(x) => x.to_string(),
            x => todo!("{:?}", x),
        });
    }

    fn visit_value(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

pub struct XirBinaryExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    op: Option<ir::Expr>,
}

impl<'a> XirBinaryExprVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            op: None,
        }
    }
}

impl<'a> BinaryExprVisitor for XirBinaryExprVisitor<'a> {
    fn visit_op(&mut self, op: BinaryOp) {
        self.op = Some(match op {
            BinaryOp::Add => ir::Expr::BinaryOp(ir::BinaryOp::Add, ir::OverflowBehaviour::Wrap),
            BinaryOp::Sub => ir::Expr::BinaryOp(ir::BinaryOp::Sub, ir::OverflowBehaviour::Wrap),
            BinaryOp::Mul => ir::Expr::BinaryOp(ir::BinaryOp::Mul, ir::OverflowBehaviour::Wrap),
            BinaryOp::Div => ir::Expr::BinaryOp(ir::BinaryOp::Div, ir::OverflowBehaviour::Wrap),
            BinaryOp::Rem => ir::Expr::BinaryOp(ir::BinaryOp::Mod, ir::OverflowBehaviour::Wrap),
            BinaryOp::Less => ir::Expr::CompareOp(ir::CompareOp::CmpLt, BOOL),
            BinaryOp::Greater => ir::Expr::CompareOp(ir::CompareOp::CmpGt, BOOL),
            BinaryOp::Equal => ir::Expr::CompareOp(ir::CompareOp::CmpEq, BOOL),
            BinaryOp::BitAnd => {
                ir::Expr::BinaryOp(ir::BinaryOp::BitAnd, ir::OverflowBehaviour::Wrap)
            }
            x => todo!("{:?}", x),
        });
    }

    fn visit_lhs(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_rhs(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

impl<'a> Drop for XirBinaryExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= 2;
        self.exprs.push(self.op.take().unwrap());
    }
}

pub struct XirUnaryExprVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    op: Option<ir::UnaryOp>,
}

impl<'a> XirUnaryExprVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            op: None,
        }
    }
}

impl<'a> UnaryExprVisitor for XirUnaryExprVisitor<'a> {
    fn visit_op(&mut self, op: UnaryOp) {
        self.op = Some(match op {
            UnaryOp::Neg => ir::UnaryOp::Minus,
            x => todo!("{:?}", x),
        });
    }

    fn visit_lhs(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }
}

impl<'a> Drop for XirUnaryExprVisitor<'a> {
    fn drop(&mut self) {
        *self.stack_height -= 1;
        self.exprs.push(ir::Expr::UnaryOp(
            self.op
                .expect("UnaryExprVisitor::visit_op was never called"),
            ir::OverflowBehaviour::Wrap,
        ));
    }
}

pub struct XirCastVisitor<'a> {
    defs: &'a Definitions,
    names: &'a NameMap,
    properties: &'a TargetProperties<'a>,
    deftys: &'a HashMap<DefId, ir::Type>,
    cur_fnty: &'a mut ir::FnType,
    exprs: &'a mut Vec<ir::Expr>,
    locals: &'a mut Vec<ir::Type>,
    ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
    stack_height: &'a mut u32,
    var_heights: &'a mut HashMap<SsaVarId, u32>,
    var_stack: &'a mut Vec<SsaVarId>,
    ty: ir::Type,
}

impl<'a> XirCastVisitor<'a> {
    pub fn new(
        defs: &'a Definitions,
        names: &'a NameMap,
        properties: &'a TargetProperties<'a>,
        deftys: &'a HashMap<DefId, ir::Type>,
        cur_fnty: &'a mut ir::FnType,
        exprs: &'a mut Vec<ir::Expr>,
        locals: &'a mut Vec<ir::Type>,
        ssa_tys: &'a mut HashMap<SsaVarId, ir::Type>,
        stack_height: &'a mut u32,
        var_heights: &'a mut HashMap<SsaVarId, u32>,
        var_stack: &'a mut Vec<SsaVarId>,
    ) -> Self {
        Self {
            defs,
            names,
            properties,
            deftys,
            cur_fnty,
            exprs,
            locals,
            ssa_tys,
            stack_height,
            var_heights,
            var_stack,
            ty: ir::Type::Null,
        }
    }
}

impl<'a> CastVisitor for XirCastVisitor<'a> {
    fn visit_inner(&mut self) -> Option<impl ExprVisitor + '_> {
        Some(Box::new(XirExprVisitor::new(
            self.defs,
            self.names,
            self.properties,
            self.deftys,
            self.cur_fnty,
            self.exprs,
            self.locals,
            self.ssa_tys,
            self.stack_height,
            self.var_heights,
            self.var_stack,
        )))
    }

    fn visit_cast_type(&mut self) -> Option<impl TypeVisitor + '_> {
        Some(Box::new(XirTypeVisitor::new(
            self.defs,
            self.names,
            &mut self.ty,
            self.properties,
        )))
    }
}

impl<'a> Drop for XirCastVisitor<'a> {
    fn drop(&mut self) {
        self.exprs.push(ir::Expr::Convert(
            ir::ConversionStrength::Reinterpret,
            core::mem::take(&mut self.ty),
        ));
        *self.stack_height -= 1;
    }
}

pub struct XirConstStringVisitor<'a> {
    properties: &'a TargetProperties<'a>,
    val: &'a mut XLangString,
    ty: &'a mut ir::Type,
}

impl<'a> XirConstStringVisitor<'a> {
    pub fn new(
        properties: &'a TargetProperties<'a>,
        val: &'a mut XLangString,
        ty: &'a mut ir::Type,
    ) -> Self {
        Self {
            properties,
            val,
            ty,
        }
    }
}

impl<'a> ConstStringVisitor for XirConstStringVisitor<'a> {
    fn visit_string_type(&mut self, st: StringType) {
        match st {
            StringType::Default | StringType::Raw(_) => todo!(),
            StringType::Byte | StringType::RawByte(_) => {
                let mut ptr = ir::PointerType::default();
                *ptr.inner = ir::Type::Scalar(ir::ScalarType {
                    header: ir::ScalarTypeHeader {
                        bitsize: 0,
                        vectorsize: XLangNone,
                        validity: ir::ScalarValidity::empty(),
                    },
                    kind: ir::ScalarTypeKind::Integer {
                        signed: false,
                        min: XLangNone,
                        max: XLangNone,
                    },
                });
                *self.ty = ir::Type::Pointer(ptr);
            }
        }
    }

    fn visit_value(&mut self, val: Symbol) {
        *self.val = (&val).into();
    }
}

pub struct XirConstIntVisitor<'a> {
    properties: &'a TargetProperties<'a>,
    val: &'a mut u128,
    intty: &'a mut ir::ScalarType,
}

impl<'a> XirConstIntVisitor<'a> {
    pub fn new(
        properties: &'a TargetProperties<'a>,
        val: &'a mut u128,
        intty: &'a mut ir::ScalarType,
    ) -> Self {
        Self {
            properties,
            val,
            intty,
        }
    }
}

impl<'a> ConstIntVisitor for XirConstIntVisitor<'a> {
    fn visit_intty(&mut self) -> Option<impl IntTyVisitor + '_> {
        Some(Box::new(XirIntTyVisitor::new(self.intty, self.properties)))
    }

    fn visit_value(&mut self, val: u128) {
        *self.val = val;
    }
}

impl<'a> ConstCharVisitor for XirConstIntVisitor<'a> {
    fn visit_charty(&mut self, ty: crate::lex::CharType) {
        match ty {
            CharType::Default => {
                self.intty.kind = ir::ScalarTypeKind::Char {
                    flags: ir::CharFlags::UNICODE,
                };
                self.intty.header.bitsize = 32;
            }
            CharType::Byte => {
                self.intty.kind = ir::ScalarTypeKind::Integer {
                    signed: false,
                    min: XLangNone,
                    max: XLangNone,
                };
                self.intty.header.bitsize = 8;
            }
        }
    }

    fn visit_value(&mut self, val: u32) {
        *self.val = val as u128;
    }
}
