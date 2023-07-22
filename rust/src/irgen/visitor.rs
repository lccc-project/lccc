use xlang::abi::pair::Pair;

use crate::{
    interning::Symbol,
    sema::{
        mir,
        ty,
        DefId, DefinitionInner, Definitions, FunctionBody,
    },
};

macro_rules! def_visitors{
    ($($vis:vis trait $trait:path {$(fn $visitor_fn:ident(&mut self $(,$($pname:ident : $ty:ty ),* $(,)?)?) $(-> $ret_ty:ty)?;)*})*) => {
        $(
            impl<V: $trait + ?Sized> $trait for &mut V {
                $(
                    #[inline]
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)? {
                        <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                    }
                )*
            }

            impl<V: $trait + ?Sized> $trait for Box<V> {
                $(
                    #[inline]
                    fn $visitor_fn(&mut self, $($($pname: $ty),*)?) $(-> $ret_ty)? {
                        <V as $trait>::$visitor_fn(self, $($($pname),*)?)
                    }
                )*
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
            }
        )*
    }
}

pub trait ModVisitor {
    fn visit_defid(&mut self, defid: DefId);
    fn visit_submodule(&mut self) -> Option<Box<dyn ModVisitor + '_>>;
    fn visit_type(&mut self) -> Option<Box<dyn TypeDefVisitor + '_>>;
    fn visit_value(&mut self) -> Option<Box<dyn ValueDefVisitor + '_>>;
}

pub fn visit_module<V: ModVisitor>(
    md: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
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

pub trait TypeDefVisitor {
    fn visit_defid(&mut self, defid: DefId);
}

pub fn visit_type_def<V: TypeDefVisitor>(
    def: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
    visit.visit_defid(def);
    todo!()
}

pub trait ValueDefVisitor {
    fn visit_name(&mut self, name: &[Symbol]);
    fn visit_defid(&mut self, defid: DefId);
    fn visit_function(&mut self) -> Option<Box<dyn FunctionDefVisitor + '_>>;
}

pub fn visit_value_def<V: ValueDefVisitor>(
    def: DefId,
    defs: &Definitions,
    mut visit: V,
    names: &mut Vec<Symbol>,
) {
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

pub trait FunctionDefVisitor {
    fn visit_fnty(&mut self) -> Option<Box<dyn FunctionTyVisitor + '_>>;
    fn visit_fnbody(&mut self) -> Option<Box<dyn FunctionBodyVisitor + '_>>;
}

pub fn visit_fndef<V: FunctionDefVisitor>(
    mut visitor: V,
    fnty: &ty::FnType,
    body: &Option<FunctionBody>,
    defs: &Definitions,
) {
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

pub trait FunctionBodyVisitor {
    fn visit_basic_block(&mut self) -> Option<Box<dyn BasicBlockVisitor + '_>>;
}

pub fn visit_fnbody<V: FunctionBodyVisitor>(
    mut visitor: V,
    fnbody: &mir::MirFunctionBody,
    defs: &Definitions,
) {
    for bb in &fnbody.bbs {
        let visitor = visitor.visit_basic_block();
        visit_basic_block(visitor, bb, defs);
    }
}

pub trait BasicBlockVisitor {
    fn visit_id(&mut self, id: mir::BasicBlockId);
    fn visit_stat(&mut self) -> Option<Box<dyn StatementVisitor + '_>>;
    fn visit_term(&mut self) -> Option<Box<dyn TerminatorVisitor + '_>>;
}

pub fn visit_basic_block<V: BasicBlockVisitor>(
    mut visitor: V,
    bb: &mir::MirBasicBlock,
    defs: &Definitions,
) {
    visitor.visit_id(bb.id);

    for stmt in &bb.stmts {
        visit_statement(visitor.visit_stat(), stmt, defs);
    }

    visit_terminator(visitor.visit_term(), &bb.term, defs);
}

pub trait StatementVisitor {}

pub fn visit_statement<V: StatementVisitor>(
    mut visitor: V,
    stmt: &mir::MirStatement,
    defs: &Definitions,
) {
    todo!()
}

pub trait TerminatorVisitor {}

pub fn visit_terminator<V: TerminatorVisitor>(
    mut visitor: V,
    stmt: &mir::MirTerminator,
    defs: &Definitions,
) {
    todo!()
}

pub trait FunctionTyVisitor {
    fn visit_tag(&mut self, abi: ty::AbiTag);
    fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
    fn visit_cvarargs(&mut self);
}

pub fn visit_fnty<V: FunctionTyVisitor>(mut visitor: V, fnty: &ty::FnType, defs: &Definitions) {
    visitor.visit_tag(*fnty.tag);

    visit_type(visitor.visit_return(), &fnty.retty, defs);

    for param in &fnty.paramtys {
        visit_type(visitor.visit_param(), param, defs);
    }

    if fnty.iscvarargs.body {
        visitor.visit_cvarargs();
    }
}

pub trait TypeVisitor {}

pub fn visit_type<V: TypeVisitor>(mut visitor: V, ty: &ty::Type, defs: &Definitions) {
    todo!()
}

#[rustfmt::skip]
def_visitors!{
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
    
    pub trait StatementVisitor {
    
    }
    
    pub trait TerminatorVisitor{
        
    }
    
    pub trait FunctionTyVisitor {
        fn visit_tag(&mut self, abi: ty::AbiTag);
        fn visit_return(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_param(&mut self) -> Option<Box<dyn TypeVisitor + '_>>;
        fn visit_cvarargs(&mut self);
    }
    
    pub trait TypeVisitor {
    }
    
}
