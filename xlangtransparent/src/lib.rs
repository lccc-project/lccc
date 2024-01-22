use xlang::{
    abi::result::Result::Ok,
    ir::{FunctionDeclaration, Path, PointerType, Scope, Type, Value},
    plugin::XLangPlugin,
    prelude::v1::*,
    targets::properties::TargetProperties,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum OptValue {
    Opaque(Type),
    Const(Value),
    LValue(Type, LValue),
    Pointer(PointerType, LValue),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LValue {
    OpaquePointer,
    GlobalAddress(Path),
}

pub struct TransparentPlugin {}

impl TransparentPlugin {
    fn accept_scope(&mut self, s: &mut Scope) {
        for Pair(name, member) in &mut s.members {
            println!("Accepting member {:?}", name);
            match &mut member.member_decl {
                xlang::ir::MemberDeclaration::Scope(s) => self.accept_scope(s),
                xlang::ir::MemberDeclaration::Function(f) => self.accept_function(f),
                _ => {}
            }
        }
    }

    fn accept_function(&mut self, _: &mut FunctionDeclaration) {}
}

impl XLangPlugin for TransparentPlugin {
    fn accept_ir(
        &mut self,
        ir: &mut xlang::ir::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        self.accept_scope(&mut ir.root);
        Ok(())
    }

    fn set_target(&mut self, _: &'static TargetProperties<'static>) {}
}

xlang::host::rustcall! {
    #[no_mangle]
    pub extern "rustcall" fn xlang_plugin_main() -> DynBox<dyn XLangPlugin>{
        DynBox::unsize_box(Box::new(TransparentPlugin{}))
    }
}
