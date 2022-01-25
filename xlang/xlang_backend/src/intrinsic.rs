use crate::expr::{Trap, VStackValue};

macro_rules! define_xlang_intrinsics{
    {
        $(($($path:ident)::* ($codegen:pat, $ty:pat => $expr:expr))),*
    } => {
        ///
        /// Calls an intrinsic function defined
        #[allow(clippy::redundant_closure_call)] // needed for hygine
        pub fn call_intrinsic<F: $crate::FunctionRawCodegen>(path: &::xlang::ir::Path, codegen: &mut $crate::FunctionCodegen<F>, fnty: &::xlang::ir::FnType){
            match &*path.components{
                [::xlang::ir::PathComponent::Root,rest @ ..]
                    | [rest @ ..] => {
                        $(match &[$(::core::stringify!($path)),*]{
                            [idents @ ..] if rest.iter().map(|comp: &::xlang::ir::PathComponent| {match comp{
                                ::xlang::ir::PathComponent::Text(__name) => ::std::option::Option::Some(&**__name),
                                _ => ::std::option::Option::None
                            }}).eq(idents.iter().map(|n|::std::option::Option::Some(*n)))=> return (|$codegen: &mut $crate::FunctionCodegen<F>,$ty: &::xlang::ir::FnType| $expr)(codegen,fnty),
                            _ => {}
                        })*
                        panic!("Unknown intrinsic: {:?}", path)
                    }
            }
        }
    }
}

define_xlang_intrinsics! {
    (__lccc::intrinsics::C::__builtin_trap(codegen,_ => {
        codegen.raw_inner().write_trap(Trap::Abort);
        codegen.push_value(VStackValue::Trapped);
    }))

}
