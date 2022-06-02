use xlang::{
    abi::pair::Pair,
    ir::{FnType, PathComponent},
    targets::properties::TargetProperties,
};

use crate::{
    expr::{Trap, VStackValue},
    FunctionCodegen, FunctionRawCodegen,
};

macro_rules! define_xlang_intrinsics{
    {
        $(($($path:ident)::* ($codegen:pat, $ty:pat => $expr:expr))),*
    } => {
        ///
        /// Calls an intrinsic function defined
        #[allow(clippy::redundant_closure_call)] // needed for hygine
        pub fn call_intrinsic<F: $crate::FunctionRawCodegen>(path: &::xlang::ir::Path, codegen: &mut $crate::FunctionCodegen<F>, fnty: &::xlang::ir::FnType,properties: &::xlang::targets::properties::TargetProperties){
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
                        call_target_intrinsic(rest,codegen,fnty,properties)
                    }
            }
        }
    }
}

fn call_target_intrinsic<F: FunctionRawCodegen>(
    path: &[PathComponent],
    code: &mut FunctionCodegen<F>,
    fnty: &FnType,
    properties: &TargetProperties,
) {
    match &path {
        [PathComponent::Text(lccc), PathComponent::Text(intrinsics), PathComponent::Text(aname), PathComponent::Text(iname)]
            if lccc.strip_suffix("__").unwrap_or(lccc) == "lccc"
                && intrinsics
                    .strip_prefix("__")
                    .map_or(&**intrinsics, |s| s.strip_suffix("__").unwrap_or(s))
                    == "intrinsics" =>
        {
            for name in properties.arch.arch_names {
                if name == &**aname {
                    for Pair(name, _) in properties.arch.builtins {
                        if name == &**iname {
                            let params = code.pop_values(fnty.params.len()).unwrap();
                            let val = code.raw_inner().write_intrinsic(*name, params);
                            code.push_value(val);
                        }
                    }
                }
            }
        }
        [path @ ..] => panic!("Unknown intrinsics call {:?}", path),
    }
}

define_xlang_intrinsics! {
    (__lccc::intrinsics::C::__builtin_trap(codegen,_ => {
        codegen.raw_inner().write_trap(Trap::Abort);
        codegen.push_value(VStackValue::Trapped);
    }))

}
