use ty::Spanned;
use xlang::abi::pair::Pair;

use crate::{
    interning::Symbol,
    lex::StringType,
    sema::{
        mir::{self, SsaVarId},
        ty, Attr, DefId, DefinitionInner, Definitions, FunctionBody,
    },
};

macro_rules! def_visitors{
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
                            None => core::default::Default::default()
                        }
                    }
                )*

                fn is_none(&self) -> bool {
                    Option::is_none(self)
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
    todo!()
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
    visit.visit_name(names);
    let def = defs.definition(def);
    for attr in &def.attrs {
        visit_attr(visit.visit_attr(), &attr.body);
    }
    match &def.inner.body {
        DefinitionInner::Function(fnty, body @ (Some(FunctionBody::MirBody(_)) | None)) => {
            let visitor = visit.visit_function();
            visit_fndef(visitor, fnty, body, defs);
        }
        DefinitionInner::Function(_, Some(FunctionBody::Intrinsic(_))) => {}
        x => panic!("Invalid definition: {:?}", x),
    }
}

pub fn visit_attr<V: AttrVisitor>(mut visitor: V, attr: &Attr) {
    match attr {
        Attr::NoMangle => visitor.visit_no_mangle(),
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
    visitor.visit_incoming_vars(&bb.incoming_vars);

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
        mir::MirTerminator::Call(info) => visit_call(visitor.visit_call(), info, defs),
        mir::MirTerminator::Jump(info) => visit_jump(visitor.visit_jump(), info),
        mir::MirTerminator::Tailcall(info) => visit_tailcall(visitor.visit_tailcall(), info, defs),
        mir::MirTerminator::Return(expr) => visit_expr(visitor.visit_return(), expr, defs),
        x => todo!("{:?}", x),
    }
}

#[allow(unused_variables, unused_mut)]
pub fn visit_call<V: CallVisitor>(mut visitor: V, info: &mir::MirCallInfo, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_retplace(info.retplace.body);
    visit_fnty(visitor.visit_fnty(), &info.fnty, defs);

    visit_expr(visitor.visit_target(), &info.targ, defs);

    for expr in &info.params {
        visit_expr(visitor.visit_param(), expr, defs);
    }

    visit_jump(visitor.visit_next(), &info.next);
}

#[allow(unused_variables, unused_mut)]
pub fn visit_tailcall<V: TailcallVisitor>(
    mut visitor: V,
    info: &mir::MirTailcallInfo,
    defs: &Definitions,
) {
    if visitor.is_none() {
        return;
    }
    visit_fnty(visitor.visit_fnty(), &info.fnty, defs);

    visit_expr(visitor.visit_target(), &info.targ, defs);

    for expr in &info.params {
        visit_expr(visitor.visit_param(), expr, defs);
    }
}

pub fn visit_jump<V: JumpVisitor>(mut visitor: V, info: &mir::MirJumpInfo) {
    if visitor.is_none() {
        return;
    }
    visitor.visit_target_bb(info.targbb);
    for remap in &info.remaps {
        visitor.visit_remap(remap.0, remap.1);
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
        ty::Type::Int(int_type) => visit_int_type(visitor.visit_int(), int_type), // oh no i broke paradigm oh no
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
        x => todo!("{}", x),
    }
}

pub fn visit_int_type<V: IntTyVisitor>(mut visitor: V, int_ty: &ty::IntType) {
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
            visit_int_type(visitor.visit_intty(), ity);

            visitor.visit_value(*val);
        }
        mir::MirExpr::Unreachable => visitor.visit_unreachable(),
        mir::MirExpr::Const(def) => visitor.visit_const(*def),
        mir::MirExpr::Cast(val, asty) => visit_cast(visitor.visit_cast(), val, asty, defs),
        mir::MirExpr::ConstString(sty, val) => {
            visit_const_string(visitor.visit_const_string(), *sty, *val, defs)
        }
        mir::MirExpr::Var(var) => visitor.visit_var(*var),
        mir::MirExpr::Tuple(vals) => visit_tuple_expr(visitor.visit_tuple(), vals, defs),
        mir::MirExpr::Read(_) => todo!(),
        mir::MirExpr::Alloca(_, _, _) => todo!(),

        mir::MirExpr::Retag(_, _, _) => todo!(),

        mir::MirExpr::Intrinsic(_) => todo!(),
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

pub fn visit_const_string<V: ConstStringVisitor>(
    mut visitor: V,
    sty: StringType,
    val: Symbol,
    defs: &Definitions,
) {
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
        fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>>;
        fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>>;
        fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>>;
    }

    pub trait TypeDefVisitor {
        fn visit_defid(&mut self, defid: DefId);
    }

    pub trait AttrVisitor {
        fn visit_no_mangle(&mut self);
    }

    pub trait ValueDefVisitor {
        fn visit_defid(&mut self, defid: DefId);
        fn visit_name(&mut self, name: &[Symbol]);
        fn visit_attr(&mut self) -> Option<Box<dyn AttrVisitor + '_>>;
        fn visit_function(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>>;
    }

    pub trait FunctionDefVisitor {
        fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>>;
        fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>>;
    }

    pub trait FunctionBodyVisitor {
        fn visit_basic_block(&mut self) -> Option<Box<dyn BasicBlockVisitor + '_>>;
    }

    pub trait BasicBlockVisitor {
        fn visit_id(&mut self, id: mir::BasicBlockId);
        fn visit_incoming_vars(&mut self, incoming: &[SsaVarId]);
        fn visit_stmt(&mut self) -> Option<Box<dyn StatementVisitor + '_>>;
        fn visit_term(&mut self) -> Option<Box<dyn TerminatorVisitor + '_>>;
    }

    pub trait ExprVisitor {
        fn visit_unreachable(&mut self);
        fn visit_const_int(&mut self) -> Option<Box<dyn ConstIntVisitor +'_>>;
        fn visit_const(&mut self, defid: DefId);
        fn visit_cast(&mut self) -> Option<Box<dyn CastVisitor + '_>>;
        fn visit_const_string(&mut self) -> Option<Box<dyn ConstStringVisitor + '_>>;
        fn visit_var(&mut self, var: mir::SsaVarId);
        fn visit_tuple(&mut self) -> Option<Box<dyn TupleExprVisitor + '_>>;
    }

    pub trait TupleExprVisitor{
        fn visit_elem(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
    }

    pub trait ConstIntVisitor{
        fn visit_intty(&mut self) -> Option<Box<dyn IntTyVisitor + '_>>;
        fn visit_value(&mut self, val: u128);
    }

    pub trait CastVisitor{
        fn visit_inner(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_cast_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }

    pub trait ConstStringVisitor {
        fn visit_string_type(&mut self, st: StringType);
        fn visit_value(&mut self, val: Symbol);
    }

    pub trait StatementVisitor {
        fn visit_let(&mut self) -> Option<Box<dyn LetStatementVisitor + '_>>;
        fn visit_store_dead(&mut self, var: mir::SsaVarId);
        fn visit_discard(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
    }

    pub trait LetStatementVisitor{
        fn visit_var(&mut self, var: mir::SsaVarId);
        fn visit_var_ty(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_init(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
    }

    pub trait CallVisitor {
        fn visit_retplace(&mut self, retplace: mir::SsaVarId);
        fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_next(&mut self) -> Option<Box<dyn JumpVisitor + '_>>;
        // TODO: visit unwind
    }

    pub trait TailcallVisitor {
        fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        // TODO: visit unwind
    }

    pub trait JumpVisitor {
        fn visit_target_bb(&mut self, targbb: mir::BasicBlockId);
        fn visit_remap(&mut self, src: mir::SsaVarId, targ: mir::SsaVarId);
    }

    pub trait TerminatorVisitor {
        fn visit_call(&mut self) -> Option<Box<dyn CallVisitor + '_>>;
        fn visit_jump(&mut self) -> Option<Box<dyn JumpVisitor + '_>>;
        fn visit_tailcall(&mut self) -> Option<Box<dyn TailcallVisitor + '_>>;
        fn visit_return(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
    }

    pub trait FunctionTyVisitor {
        fn visit_tag(&mut self, abi: ty::AbiTag);
        fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_cvarargs(&mut self);
    }

    pub trait IntTyVisitor {
        fn visit_type(&mut self, int_type: &ty::IntType);
    }

    pub trait PointerTyVisitor {
        fn visit_mutability(&mut self, mutability: ty::Mutability);
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }

    pub trait ReferenceTyVisitor {
        fn visit_lifetime(&mut self, lifetime: &ty::SemaLifetime);
        fn visit_mutability(&mut self, mutability: ty::Mutability);
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }

    pub trait TupleTyVisitor {
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }

    pub trait TypeVisitor {
        fn visit_int(&mut self) -> Option<Box<dyn IntTyVisitor + '_>>;
        fn visit_pointer(&mut self) -> Option<Box<dyn PointerTyVisitor + '_>>;
        fn visit_reference(&mut self) -> Option<Box<dyn ReferenceTyVisitor + '_>>;
        fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>>;
        fn visit_never(&mut self);
    }
}
