use ty::Spanned;
use xlang::abi::pair::Pair;

use crate::{
    ast::CharType,
    interning::Symbol,
    lex::StringType,
    sema::{
        cx, generics,
        hir::BinaryOp,
        intrin::IntrinsicDef,
        mir::{self, BasicBlockId, SsaVarId, UnaryOp},
        ty, Attr, Constructor, DefId, DefinitionInner, Definitions, FunctionBody, UserTypeKind,
    },
};

macro_rules! visitor_todo{
    ($($tt:tt)*) => {
        {
            #![allow(unreachable_code)]
            todo!($($tt)*);
            None::<()>
        }
    }
}

pub(crate) use visitor_todo;

macro_rules! def_visitors {
    (
        $(
            $vis:vis trait $trait:ident {
                $(
                    fn $visitor_fn:ident(&mut self $(,$($pname:ident : $ty:ty ),* $(,)?)?) $(-> $ret_ty:ty)?;
                )*

            }
        )*
    ) => {
        $(
            impl $trait for (){
                $(
                    #[inline(always)]
                    fn $visitor_fn(&mut self, $($(_: $ty),*)?) $(-> $ret_ty)?{
                        ().into()
                    }
                )*

                fn is_none(&self) -> bool{
                    true
                }
            }
            impl<V: $trait + ?Sized> $trait for &mut V {
                $(
                    #[inline]
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)? {
                        <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                    }
                )*

                fn is_none(&self) -> bool {
                    (**self).is_none()
                }
            }

            impl<V: $trait + ?Sized> $trait for Box<V> {
                $(
                    #[inline]
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)? {
                        <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                    }
                )*

                fn is_none(&self) -> bool {
                    (**self).is_none()
                }
            }

            impl<V: $trait> $trait for Option<V> {
                $(
                    #[inline]
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)? {
                        match self {
                            Some(this) => <V as $trait>::$visitor_fn(this, $($($pname),*)?),
                            None => ::core::default::Default::default(),
                        }
                    }
                )*

                fn is_none(&self) -> bool {
                    match self{
                        Some(x) => x.is_none(),
                        None => true,
                    }
                }
            }

            $vis trait $trait {
                $(
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)?;
                )*

                fn is_none(&self) -> bool {
                    false
                }
            }
        )*
    }
}

pub fn visit_module<V: ModVisitor>(
    md: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
    if visit.is_none() {
        return;
    }
    visit.visit_defid(md);
    let md = defs.as_module(md);

    for Pair(name, def) in &md.types {
        if defs.is_module(*def) {
            let visitor = visit.visit_submodule();
            names.push(*name);
            visit_module(*def, defs, visitor, names);
            names.pop();
        }
    }

    for Pair(name, def) in &md.types {
        if !defs.is_module(*def) {
            let visitor = visit.visit_type();
            names.push(*name);
            visit_type_def(*def, defs, visitor, names);
            names.pop();
        }
    }

    for Pair(name, def) in &md.values {
        if let DefinitionInner::Function(_, Some(FunctionBody::Intrinsic(_))) =
            defs.definition(*def).inner.body
        {
            continue;
        }
        let visitor = visit.visit_value();
        names.push(*name);

        visit_value_def(*def, defs, visitor, names);
        names.pop();
    }
}

#[allow(unused_variables)]
pub fn visit_type_def<V: TypeDefVisitor>(
    def: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
    if visit.is_none() {
        return;
    }
    visit.visit_defid(def);
    visit.visit_name(names);

    let def = defs.definition(def);
    for attr in &def.attrs {
        visit_attr(visit.visit_attr(), &attr.body);
    }
    match &def.inner.body {
        DefinitionInner::UserType(ty) => match ty {
            crate::sema::UserType::Struct(uty, def) => {
                visit.visit_kind(*uty);
                visit_ctor_def(visit.visit_struct(), &def.ctor, defs)
            }
            _ => todo!("user type"),
        },
        DefinitionInner::Trait(_) => {}
        x => panic!("Invalid definition: {:?}", x),
    }
}

pub fn visit_ctor_def<V: ConstructorDefVisitor>(
    mut visit: V,
    ctor: &Constructor,
    defs: &Definitions,
) {
    if visit.is_none() {
        return;
    }

    match ctor {
        Constructor::Unit => {}
        Constructor::Tuple(v) => {
            for (n, field) in v.iter().enumerate() {
                let ty = &field.ty;
                let mut field = visit.visit_field();
                if field.is_none() {
                    break;
                }
                let name = Symbol::intern_by_val(n.to_string());
                let name = ty::FieldName::Field(name);

                field.visit_name(&name);
                visit_type(field.visit_ty(), ty, defs);
            }
        }
        Constructor::Struct(v) => {
            for field in v {
                let ty = &field.ty;
                let name = ty::FieldName::Field(*field.name);
                let mut field = visit.visit_field();
                if field.is_none() {
                    break;
                }

                field.visit_name(&name);
                visit_type(field.visit_ty(), ty, defs);
            }
        }
    }
}

pub fn visit_value_def<V: ValueDefVisitor>(
    def: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
    if visit.is_none() {
        return;
    }

    visit.visit_defid(def);
    let def = defs.definition(def);
    visit.visit_name(names);

    for attr in &def.attrs {
        visit_attr(visit.visit_attr(), &attr.body);
    }
    match &def.inner.body {
        DefinitionInner::Function(fnty, body @ (Some(FunctionBody::MirBody(_)) | None)) => {
            let visitor = visit.visit_function();
            visit_fndef(visitor, fnty, body, defs);
        }
        x => panic!("Invalid definition: {:?}", x),
    }
}

pub fn visit_attr<V: AttrVisitor>(mut visitor: V, attr: &Attr) {
    match attr {
        Attr::NoMangle => visitor.visit_no_mangle(),
        Attr::Repr(_) => {}
        x => todo!("{:?}", x),
    }
}

pub fn visit_fndef<V: FunctionDefVisitor>(
    mut visitor: V,
    fnty: &ty::FnType,
    body: &Option<FunctionBody>,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    let fnty_visit = visitor.visit_fnty();
    visit_fnty(fnty_visit, fnty, defs);
    match body {
        Some(FunctionBody::MirBody(body)) => {
            visit_fnbody(visitor.visit_fnbody(), body, defs);
        }
        None => {}
        _ => unreachable!(),
    }
}

pub fn visit_fnbody<V: FunctionBodyVisitor>(
    mut visitor: V,
    fnbody: &mir::MirFunctionBody,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    for bb in &fnbody.bbs {
        let visitor = visitor.visit_basic_block();
        visit_basic_block(visitor, bb, defs);
    }
}

pub fn visit_basic_block<V: BasicBlockVisitor>(
    mut visitor: V,
    bb: &mir::MirBasicBlock,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_id(bb.id);
    for (var, ty) in &bb.incoming_vars {
        visit_type(visitor.visit_incoming_var(*var), ty, defs);
    }

    for stmt in &bb.stmts {
        visit_statement(visitor.visit_stmt(), stmt, defs);
    }

    visit_terminator(visitor.visit_term(), &bb.term, defs);
}

#[allow(unused_variables, unused_mut)]
pub fn visit_statement<V: StatementVisitor>(
    mut visitor: V,
    stmt: &mir::MirStatement,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    match stmt {
        mir::MirStatement::Declare { var, ty, init } => {
            visit_let(visitor.visit_let(), **var, ty, init, defs)
        }
        mir::MirStatement::Discard(expr) => visit_expr(visitor.visit_discard(), expr, defs),
        mir::MirStatement::StoreDead(var) => visitor.visit_store_dead(*var),
        mir::MirStatement::Write(_, _) => todo!("write"),
        mir::MirStatement::EndRegion(_) => todo!("end region"),
        mir::MirStatement::Dealloca(_) => todo!("dealloca"),
        mir::MirStatement::MarkAll(_, _) => todo!("mark all"),
        mir::MirStatement::MarkDropState(_, _, _) => todo!("mark drop state"),
        mir::MirStatement::CaptureException(_) => todo!("capture exception"),
    }
}

pub fn visit_let<V: LetStatementVisitor>(
    mut visitor: V,
    var: SsaVarId,
    ty: &ty::Type,
    init: &mir::MirExpr,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }

    visitor.visit_var(var);
    visit_type(visitor.visit_var_ty(), ty, defs);
    visit_expr(visitor.visit_init(), init, defs);
}

pub fn visit_terminator<V: TerminatorVisitor>(
    mut visitor: V,
    term: &mir::MirTerminator,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    match term {
        mir::MirTerminator::Branch(info) => visit_branch(visitor.visit_branch(), info, defs),
        mir::MirTerminator::Call(info) => visit_call(visitor.visit_call(), info, defs),
        mir::MirTerminator::Jump(info) => visit_jump(visitor.visit_jump(), info),
        mir::MirTerminator::Tailcall(info) => visit_tailcall(visitor.visit_call(), info, defs),
        mir::MirTerminator::Return(expr) => visit_expr(visitor.visit_return(), expr, defs),
        mir::MirTerminator::Unreachable => visitor.visit_unreachable(),
        x => todo!("{:?}", x),
    }
}

#[allow(unused_variables, unused_mut)]
pub fn visit_call<V: CallVisitor>(mut visitor: V, info: &mir::MirCallInfo, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }

    if let mir::MirExpr::Intrinsic(intrin, generics) = &info.targ.body {
        let body_visitor = visitor.visit_intrinsic(*intrin, generics);

        if !body_visitor.is_none() {
            if let Some(body) = intrin.default_body(
                defs,
                BasicBlockId::UNUSED,
                info.next.targbb,
                info.unwind
                    .as_ref()
                    .map(|jmp| jmp.targbb)
                    .unwrap_or(BasicBlockId::UNUSED),
                generics,
            ) {
                visit_basic_block(body_visitor, &body, defs);
            } else {
                panic!("`visit_intrinsic` expects a default body but {intrin} does not have one");
            }
        }
    } else {
        visit_expr(visitor.visit_target(), &info.targ, defs);
    }

    visitor.visit_retplace(info.retplace.body);
    visit_fnty(visitor.visit_fnty(), &info.fnty, defs);

    for expr in &info.params {
        visit_expr(visitor.visit_param(), expr, defs);
    }

    visit_jump(visitor.visit_next(), &info.next);
    if let Some(uw) = &info.unwind {
        visit_jump(visitor.visit_unwind(), &uw);
    }
}

#[allow(unused_variables, unused_mut)]
pub fn visit_tailcall<V: CallVisitor>(
    mut visitor: V,
    info: &mir::MirTailcallInfo,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visit_fnty(visitor.visit_fnty(), &info.fnty, defs);

    if let mir::MirExpr::Intrinsic(intrin, generics) = &info.targ.body {
        let body_visitor = visitor.visit_intrinsic(*intrin, generics);

        if !body_visitor.is_none() {
            if let Some(body) = intrin.default_body(
                defs,
                BasicBlockId::UNUSED,
                BasicBlockId::UNUSED,
                info.unwind
                    .as_ref()
                    .map(|jmp| jmp.targbb)
                    .unwrap_or(BasicBlockId::UNUSED),
                generics,
            ) {
                visit_basic_block(body_visitor, &body, defs);
            } else {
                panic!("`visit_intrinsic` expects a default body but {intrin} does not have one");
            }
        }
    } else {
        visit_expr(visitor.visit_target(), &info.targ, defs);
    }

    for expr in &info.params {
        visit_expr(visitor.visit_param(), expr, defs);
    }

    visitor.visit_tailcall();
    if let Some(uw) = &info.unwind {
        visit_jump(visitor.visit_unwind(), &uw);
    }
}

pub fn visit_branch<V: BranchVisitor>(
    mut visitor: V,
    info: &mir::MirBranchInfo,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }

    visit_expr(visitor.visit_cond(), &info.cond, defs);
    visit_jump(visitor.visit_if_arm(), &info.if_block);
    visit_jump(visitor.visit_else(), &info.else_block);
}

pub fn visit_jump<V: JumpVisitor>(mut visitor: V, info: &mir::MirJumpInfo) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_target_bb(info.targbb);
    for remap in &info.remaps {
        visitor.visit_remap(remap.0, remap.1);
    }
    if info.fallthrough {
        visitor.visit_fallthrough();
    }
}

pub fn visit_fnty<V: FunctionTyVisitor>(mut visitor: V, fnty: &ty::FnType, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_tag(*fnty.tag);

    visit_type(visitor.visit_return(), &fnty.retty, defs);

    for param in &fnty.paramtys {
        visit_type(visitor.visit_param(), param, defs);
    }

    if fnty.iscvarargs.body {
        visitor.visit_cvarargs();
    }
}

pub fn visit_type<V: TypeVisitor>(mut visitor: V, ty: &ty::Type, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    match ty {
        ty::Type::Array(ty, len) => {
            visit_type_array(visitor.visit_array(), &ty.body, &len.body, defs);
        }
        ty::Type::Int(int_type) => visit_type_int(visitor.visit_int(), int_type),
        ty::Type::Pointer(mutability, ty) => {
            visit_type_pointer(visitor.visit_pointer(), mutability.body, &ty.body, defs);
        }
        ty::Type::Reference(lifetime, mutability, ty) => {
            visit_type_reference(
                visitor.visit_reference(),
                lifetime.as_ref().map(|x| &x.body),
                mutability.body,
                &ty.body,
                defs,
            );
        }
        ty::Type::Tuple(tys) => {
            visit_type_tuple(
                visitor.visit_tuple(),
                &tys.iter().map(|x| x.body.clone()).collect::<Vec<_>>(),
                defs,
            );
        }
        ty::Type::Never => {
            visitor.visit_never();
        }
        ty::Type::UserType(ty, _) => {
            visitor.visit_user_type(*ty); // TODO: Visit Generics
        }
        x => todo!("{}", x),
    }
}

pub fn visit_type_array<V: ArrayTyVisitor>(
    mut visitor: V,
    ty: &ty::Type,
    len: &cx::ConstExpr,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visit_type(visitor.visit_type(), ty, defs);
    visitor.visit_len(len);
}

pub fn visit_type_int<V: IntTyVisitor>(mut visitor: V, int_ty: &ty::IntType) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_type(int_ty);
}

pub fn visit_type_pointer<V: PointerTyVisitor>(
    mut visitor: V,
    mutability: ty::Mutability,
    ty: &ty::Type,
    defs: &Definitions,
) {
    visitor.visit_mutability(mutability);
    visit_type(visitor.visit_type(), ty, defs);
}

pub fn visit_type_reference<V: ReferenceTyVisitor>(
    mut visitor: V,
    lifetime: Option<&ty::SemaLifetime>,
    mutability: ty::Mutability,
    ty: &ty::Type,
    defs: &Definitions,
) {
    if let Some(lifetime) = lifetime {
        visitor.visit_lifetime(lifetime);
    }
    visitor.visit_mutability(mutability);
    visit_type(visitor.visit_type(), ty, defs);
}

pub fn visit_type_tuple<V: TupleTyVisitor>(mut visitor: V, tys: &[ty::Type], defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    for ty in tys {
        visit_type(visitor.visit_type(), ty, defs);
    }
}

pub fn visit_expr<V: ExprVisitor>(mut visitor: V, expr: &mir::MirExpr, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }

    match expr {
        mir::MirExpr::ConstInt(ity, val) => {
            let mut visitor = visitor.visit_const_int();

            if visitor.is_none() {
                return;
            }
            visit_type_int(visitor.visit_intty(), ity);

            visitor.visit_value(*val);
        }
        mir::MirExpr::Unreachable => visitor.visit_unreachable(),
        mir::MirExpr::Const(def, _) => visitor.visit_const(*def),
        mir::MirExpr::Cast(val, asty) => visit_cast(visitor.visit_cast(), val, asty, defs),
        mir::MirExpr::ConstString(sty, val) => {
            visit_const_string(visitor.visit_const_string(), *sty, *val)
        }
        mir::MirExpr::Var(var) => visitor.visit_var(*var),
        mir::MirExpr::Tuple(vals) => visit_tuple_expr(visitor.visit_tuple(), vals, defs),
        mir::MirExpr::Read(_) => todo!(),
        mir::MirExpr::Alloca(_, _, _) => todo!(),
        mir::MirExpr::Retag(_, _, _) => todo!(),
        mir::MirExpr::Intrinsic(_, _) => panic!("Cannot use an intrinsic, except to call it"),
        mir::MirExpr::FieldProject(expr, name) => {
            visit_field_access(visitor.visit_field_project(), expr, name, defs)
        }
        mir::MirExpr::GetSubobject(expr, name) => {
            visit_field_access(visitor.visit_field_subobject(), expr, name, defs)
        }
        mir::MirExpr::Ctor(ctor) => visit_constructor(visitor.visit_ctor(), ctor, defs),
        mir::MirExpr::BinaryExpr(op, lhs, rhs) => {
            visit_binary_expr(visitor.visit_binary_expr(), op, lhs, rhs, defs)
        }
        mir::MirExpr::Uninit(_) => todo!("uninit"),
        mir::MirExpr::AllocaDrop(_, _) => todo!("alloca drop"),
        mir::MirExpr::GetSymbol(_) => todo!("get symbol"),
        mir::MirExpr::UnaryExpr(op, lhs) => {
            visit_unary_expr(visitor.visit_unary_expr(), op, lhs, defs)
        }
        mir::MirExpr::ConstChar(ty, val) => {
            let mut visitor = visitor.visit_const_char();

            if visitor.is_none() {
                return;
            }
            visitor.visit_charty(*ty);

            visitor.visit_value(*val);
        }
        mir::MirExpr::InlineConst(_) => todo!("inline const"),
    }
}

pub fn visit_field_access<V: FieldAccessVisitor>(
    mut visitor: V,
    base: &mir::MirExpr,
    field_name: &ty::FieldName,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_field(field_name);
    visit_expr(visitor.visit_base(), base, defs);
}

pub fn visit_binary_expr<V: BinaryExprVisitor>(
    mut visitor: V,
    op: &mir::BinaryOp,
    lhs: &mir::MirExpr,
    rhs: &mir::MirExpr,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_op(*op);
    visit_expr(visitor.visit_lhs(), lhs, defs);
    visit_expr(visitor.visit_rhs(), rhs, defs);
}

pub fn visit_unary_expr<V: UnaryExprVisitor>(
    mut visitor: V,
    op: &mir::UnaryOp,
    lhs: &mir::MirExpr,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_op(*op);
    visit_expr(visitor.visit_lhs(), lhs, defs);
}

pub fn visit_constructor<V: ConstructorVisitor>(
    mut visitor: V,
    ctor: &mir::MirConstructor,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }

    visitor.visit_ctor_def(ctor.ctor_def);

    for (field, val) in &ctor.fields {
        let mut visitor = visitor.visit_field();
        if visitor.is_none() {
            break;
        }
        visitor.visit_field(field);
        visit_expr(visitor.visit_value(), val, defs);
    }

    if let Some(rest_init) = &ctor.rest_init {
        visit_expr(visitor.visit_init(), rest_init, defs);
    }
}

pub fn visit_cast<V: CastVisitor>(
    mut visitor: V,
    val: &mir::MirExpr,
    asty: &ty::Type,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }

    visit_expr(visitor.visit_inner(), val, defs);
    visit_type(visitor.visit_cast_type(), asty, defs);
}

pub fn visit_const_string<V: ConstStringVisitor>(mut visitor: V, sty: StringType, val: Symbol) {
    if visitor.is_none() {
        return;
    }

    visitor.visit_string_type(sty);
    visitor.visit_value(val);
}

pub fn visit_tuple_expr<V: TupleExprVisitor>(
    mut visitor: V,
    vals: &[Spanned<mir::MirExpr>],
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }

    for val in vals {
        visit_expr(visitor.visit_elem(), val, defs);
    }
}

def_visitors! {
    pub trait ModVisitor {
        fn visit_defid(&mut self, defid: DefId);
        fn visit_submodule(&mut self) -> Option<impl ModVisitor + '_>;
        fn visit_type(&mut self) -> Option<impl TypeDefVisitor + '_>;
        fn visit_value(&mut self) -> Option<impl ValueDefVisitor + '_>;
    }

    pub trait TypeDefVisitor {
        fn visit_defid(&mut self, defid: DefId);
        fn visit_name(&mut self, name: &[Symbol]);
        fn visit_attr(&mut self) -> Option<impl AttrVisitor + '_>;
        fn visit_kind(&mut self, kind: UserTypeKind);
        fn visit_struct(&mut self) -> Option<impl ConstructorDefVisitor + '_>;
    }

    pub trait ConstructorDefVisitor {
        fn visit_field(&mut self) -> Option<impl FieldVisitor + '_>;
    }

    pub trait FieldVisitor {
        fn visit_name(&mut self, name: &ty::FieldName);
        fn visit_ty(&mut self) -> Option<impl TypeVisitor + '_>;
    }

    pub trait AttrVisitor {
        fn visit_no_mangle(&mut self);
    }

    pub trait ValueDefVisitor {
        fn visit_defid(&mut self, defid: DefId);
        fn visit_name(&mut self, name: &[Symbol]);
        fn visit_attr(&mut self) -> Option<impl AttrVisitor + '_>;
        fn visit_function(&mut self) -> Option<impl FunctionDefVisitor + '_>;
    }

    pub trait FunctionDefVisitor {
        fn visit_fnty(&mut self) -> Option<impl FunctionTyVisitor + '_>;
        fn visit_fnbody(&mut self) -> Option<impl FunctionBodyVisitor + '_>;
    }

    pub trait FunctionBodyVisitor {
        fn visit_inner_value(&mut self) -> Option<impl ValueDefVisitor + '_>;
        fn visit_basic_block(&mut self) -> Option<impl BasicBlockVisitor + '_>;
    }

    pub trait BasicBlockVisitor {
        fn visit_id(&mut self, id: mir::BasicBlockId);
        fn visit_incoming_var(&mut self, incomin: SsaVarId) -> Option<impl TypeVisitor + '_>;
        fn visit_stmt(&mut self) -> Option<impl StatementVisitor + '_>;
        fn visit_term(&mut self) -> Option<impl TerminatorVisitor + '_>;
    }

    pub trait ExprVisitor {
        fn visit_unreachable(&mut self);
        fn visit_const_int(&mut self) -> Option<impl ConstIntVisitor + '_>;
        fn visit_const_char(&mut self) -> Option<impl ConstCharVisitor + '_>;
        fn visit_const(&mut self, defid: DefId);
        fn visit_cast(&mut self) -> Option<impl CastVisitor + '_>;
        fn visit_const_string(&mut self) -> Option<impl ConstStringVisitor + '_>;
        fn visit_var(&mut self, var: mir::SsaVarId);
        fn visit_tuple(&mut self) -> Option<impl TupleExprVisitor + '_>;
        fn visit_ctor(&mut self) -> Option<impl ConstructorVisitor + '_>;
        fn visit_field_subobject(&mut self) -> Option<impl FieldAccessVisitor + '_>;
        fn visit_field_project(&mut self) -> Option<impl FieldAccessVisitor + '_>;
        fn visit_binary_expr(&mut self) -> Option<impl BinaryExprVisitor + '_>;
        fn visit_unary_expr(&mut self) -> Option<impl UnaryExprVisitor + '_>;
    }

    pub trait TupleExprVisitor {
        fn visit_elem(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait ConstIntVisitor {
        fn visit_intty(&mut self) -> Option<impl IntTyVisitor + '_>;
        fn visit_value(&mut self, val: u128);
    }

    pub trait ConstCharVisitor {
        fn visit_charty(&mut self, ty: CharType);
        fn visit_value(&mut self, val: u32);
    }

    pub trait CastVisitor {
        fn visit_inner(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_cast_type(&mut self) -> Option<impl TypeVisitor + '_>;
    }

    pub trait ConstStringVisitor {
        fn visit_string_type(&mut self, st: StringType);
        fn visit_value(&mut self, val: Symbol);
    }

    pub trait FieldAccessVisitor {
        fn visit_base(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_field(&mut self, field_name: &ty::FieldName);
    }

    pub trait BinaryExprVisitor {
        fn visit_op(&mut self, op: BinaryOp);
        fn visit_lhs(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_rhs(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait UnaryExprVisitor {
        fn visit_op(&mut self, op: UnaryOp);
        fn visit_lhs(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait ConstructorVisitor {
        fn visit_ctor_def(&mut self, defid: DefId);
        fn visit_field(&mut self) -> Option<impl FieldInitVisitor + '_>;
        fn visit_init(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait FieldInitVisitor {
        fn visit_field(&mut self, field_name: &ty::FieldName);
        fn visit_value(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait StatementVisitor {
        fn visit_let(&mut self) -> Option<impl LetStatementVisitor + '_>;
        fn visit_store_dead(&mut self, var: mir::SsaVarId);
        fn visit_discard(&mut self) -> Option<impl ExprVisitor + '_>;
    }

    pub trait LetStatementVisitor{
        fn visit_var(&mut self, var: mir::SsaVarId);
        fn visit_var_ty(&mut self) -> Option<impl TypeVisitor + '_>;
        fn visit_init(&mut self) -> Option<impl ExprVisitor + '_>;
    }


    pub trait CallVisitor {
        fn visit_retplace(&mut self, retplace: mir::SsaVarId);
        fn visit_target(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_fnty(&mut self) -> Option<impl FunctionTyVisitor + '_>;
        fn visit_param(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_next(&mut self) -> Option<impl JumpVisitor + '_>;
        fn visit_intrinsic(&mut self, intrin: IntrinsicDef, generics: &generics::GenericArgs) -> Option<impl BasicBlockVisitor + '_>;
        fn visit_tailcall(&mut self);
        fn visit_unwind(&mut self) -> Option<impl JumpVisitor + '_>;
    }

    pub trait BranchVisitor {
        fn visit_cond(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_if_arm(&mut self) -> Option<impl JumpVisitor + '_>;
        fn visit_else(&mut self) -> Option<impl JumpVisitor + '_>;
    }

    pub trait JumpVisitor {
        fn visit_target_bb(&mut self, targbb: mir::BasicBlockId);
        fn visit_remap(&mut self, src: mir::SsaVarId, targ: mir::SsaVarId);
        fn visit_fallthrough(&mut self);
    }

    pub trait TerminatorVisitor {
        fn visit_branch(&mut self) -> Option<impl BranchVisitor + '_>;
        fn visit_call(&mut self) -> Option<impl CallVisitor + '_>;
        fn visit_jump(&mut self) -> Option<impl JumpVisitor + '_>;
        fn visit_return(&mut self) -> Option<impl ExprVisitor + '_>;
        fn visit_unreachable(&mut self);
    }

    pub trait FunctionTyVisitor {
        fn visit_tag(&mut self, abi: ty::AbiTag);
        fn visit_return(&mut self) -> Option<impl TypeVisitor + '_>;
        fn visit_param(&mut self) -> Option<impl TypeVisitor + '_>;
        fn visit_cvarargs(&mut self);
    }

    pub trait ArrayTyVisitor {
        fn visit_type(&mut self) -> Option<impl TypeVisitor + '_>;
        fn visit_len(&mut self, expr: &cx::ConstExpr);
    }

    pub trait IntTyVisitor {
        fn visit_type(&mut self, int_type: &ty::IntType);
    }

    pub trait PointerTyVisitor {
        fn visit_mutability(&mut self, mutability: ty::Mutability);
        fn visit_type(&mut self) -> Option<impl TypeVisitor + '_>;
    }

    pub trait ReferenceTyVisitor {
        fn visit_lifetime(&mut self, lifetime: &ty::SemaLifetime);
        fn visit_mutability(&mut self, mutability: ty::Mutability);
        fn visit_type(&mut self) -> Option<impl TypeVisitor + '_>;
    }

    pub trait TupleTyVisitor {
        fn visit_type(&mut self) -> Option<impl TypeVisitor + '_>;
    }

    pub trait TypeVisitor {
        fn visit_array(&mut self) -> Option<impl ArrayTyVisitor + '_>;
        fn visit_int(&mut self) -> Option<impl IntTyVisitor + '_>;
        fn visit_pointer(&mut self) -> Option<impl PointerTyVisitor + '_>;
        fn visit_reference(&mut self) -> Option<impl ReferenceTyVisitor + '_>;
        fn visit_tuple(&mut self) -> Option<impl TupleTyVisitor + '_>;
        fn visit_never(&mut self);
        fn visit_user_type(&mut self, defid: DefId);
    }
}
