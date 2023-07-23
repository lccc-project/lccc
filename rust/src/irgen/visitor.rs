use xlang::abi::pair::Pair;

use crate::{
    interning::Symbol,
    sema::{mir, ty, DefId, DefinitionInner, Definitions, FunctionBody},
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
    match &defs.definition(def).inner.body {
        DefinitionInner::Function(fnty, body @ (Some(FunctionBody::MirBody(_)) | None)) => {
            let visitor = visit.visit_function();
            visit_fndef(visitor, fnty, body, defs);
        }
        _ => panic!("Invalid definition"),
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
            let body_visit = visitor.visit_fnbody();
            visit_fnbody(body_visit, body, defs);
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

    for stmt in &bb.stmts {
        visit_statement(visitor.visit_stat(), stmt, defs);
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
    todo!()
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
        x => todo!("{:?}", x),
    }
}

#[allow(unused_variables, unused_mut)]
pub fn visit_call<V: CallVisitor>(mut visitor: V, info: &mir::MirCallInfo, defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    todo!()
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
        ty::Type::Tuple(tys) => {
            visit_type_tuple(
                visitor.visit_tuple(),
                &tys.iter().map(|x| x.body.clone()).collect::<Vec<_>>(),
                defs,
            );
        }
        x => todo!("{:?}", x),
    }
}

pub fn visit_type_tuple<V: TupleTyVisitor>(mut visitor: V, tys: &[ty::Type], defs: &Definitions) {
    if visitor.is_none() {
        return;
    }
    for ty in tys {
        visit_type(visitor.visit_type(), ty, defs);
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

    pub trait ValueDefVisitor {
        fn visit_name(&mut self, name: &[Symbol]);
        fn visit_defid(&mut self, defid: DefId);
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
        fn visit_stat(&mut self) -> Option<Box<dyn StatementVisitor + '_>>;
        fn visit_term(&mut self) -> Option<Box<dyn TerminatorVisitor + '_>>;
    }

    pub trait ExprVisitor {}

    pub trait StatementVisitor {}

    pub trait CallVisitor {
        fn visit_retplace(&mut self, retplace: mir::SsaVarId);
        fn visit_target(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn ExprVisitor + '_>>;
        fn visit_next(&mut self) -> Option<Box<dyn JumpVisitor + '_>>;
        // TODO: visit unwind
    }

    pub trait JumpVisitor {
        fn visit_target_bb(&mut self, targbb: mir::BasicBlockId);
        fn visit_remap(&mut self, src: mir::SsaVarId, targ: mir::SsaVarId);
    }

    pub trait TerminatorVisitor {
        fn visit_call(&mut self) -> Option<Box<dyn CallVisitor + '_>>;
        fn visit_jump(&mut self) -> Option<Box<dyn JumpVisitor + '_>>;
    }

    pub trait FunctionTyVisitor {
        fn visit_tag(&mut self, abi: ty::AbiTag);
        fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_cvarargs(&mut self);
    }

    pub trait TypeVisitor {
        fn visit_tuple(&mut self) -> Option<Box<dyn TupleTyVisitor + '_>>;
    }

    pub trait TupleTyVisitor {
        fn visit_type(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    }
}
