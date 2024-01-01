use xlang::{
    abi::option::Some as XLangSome,
    abi::pair::Pair,
    abi::vec::Vec,
    ir::{FnType, PathComponent},
    targets::properties::TargetProperties,
};

use crate::{
    expr::{Trap, VStackValue},
    FunctionCodegen, FunctionRawCodegen,
};

///
/// Calls an intrinsic function defined
///
pub fn call_intrinsic<F: crate::FunctionRawCodegen>(
    path: &::xlang::ir::Path,
    codegen: &mut crate::FunctionCodegen<F>,
    fnty: &::xlang::ir::FnType,
    properties: &::xlang::targets::properties::TargetProperties,
    params: &mut Vec<VStackValue<F::Loc>>,
) -> bool {
    if call_generic_intrinsic(path, codegen, fnty, properties, params) {
        true
    } else if call_nongeneric_intrinsic(path, codegen, fnty, properties, params) {
        true
    } else if call_target_intrinsic(path, codegen, fnty, properties, params) {
        true
    } else {
        false
    }
}

macro_rules! define_generic_xlang_intrinsics{
    {$($($path:ident)::* :: <($($generics:pat),*)>  | $codegen:pat, $properties:pat, $ty:pat, $params:pat | => $expr:expr),* $(,)?} => {
        fn call_generic_intrinsic<F: $crate::FunctionRawCodegen>(path: &::xlang::ir::Path, codegen: &mut $crate::FunctionCodegen<F>, fnty: &::xlang::ir::FnType,properties: &::xlang::targets::properties::TargetProperties, params: &mut Vec<VStackValue<F::Loc>>) -> bool{
            match &*path.components{
                [::xlang::ir::PathComponent::Root,rest @ ..]
                    | [rest @ ..] => {
                        match rest{
                            [rest @ .., ::xlang::ir::PathComponent::Generics(generics)] => {
                                $(match &[$(::core::stringify!($path)),*]{
                                    [idents @ ..] if rest.iter().map(|comp: &::xlang::ir::PathComponent| {match comp{
                                        ::xlang::ir::PathComponent::Text(__name) => ::std::option::Option::Some(&**__name),
                                        _ => ::std::option::Option::None
                                    }}).eq(idents.iter().map(|n|::std::option::Option::Some(*n)))=> return {(|$codegen: &mut $crate::FunctionCodegen<F>, $properties: &::xlang::targets::properties::TargetProperties,$ty: &::xlang::ir::FnType, generics: &[::xlang::ir::GenericParameter], $params: Vec<VStackValue<F::Loc>>| match generics{
                                        [$($generics),*] => $expr,
                                        _ => panic!("invalid signature")
                                })(codegen, properties,fnty,generics, core::mem::take(params)); true},
                                    _ => {}
                                })*
                            }
                            _ => {}
                        }
                    }

            }
            return false
        }
    }
}

macro_rules! define_xlang_intrinsics{
    {
        $($($path:ident)::* | $codegen:pat, $properties:pat, $ty:pat, $params:pat | => $expr:expr),* $(,)?
    } => {

        #[allow(clippy::redundant_closure_call)] // needed for hygine
        fn call_nongeneric_intrinsic<F: $crate::FunctionRawCodegen>(path: &::xlang::ir::Path, codegen: &mut $crate::FunctionCodegen<F>, fnty: &::xlang::ir::FnType,properties: &::xlang::targets::properties::TargetProperties, params: &mut Vec<VStackValue<F::Loc>>) -> bool{
            match &*path.components{
                [::xlang::ir::PathComponent::Root,rest @ ..]
                    | [rest @ ..] => {
                        $(match &[$(::core::stringify!($path)),*]{
                            [idents @ ..] if rest.iter().map(|comp: &::xlang::ir::PathComponent| {match comp{
                                ::xlang::ir::PathComponent::Text(__name) => ::std::option::Option::Some(&**__name),
                                _ => ::std::option::Option::None
                            }}).eq(idents.iter().map(|n|::std::option::Option::Some(*n)))=> return {(|$codegen: &mut $crate::FunctionCodegen<F>, $properties: &::xlang::targets::properties::TargetProperties,$ty: &::xlang::ir::FnType, $params: Vec<VStackValue<F::Loc>> | $expr)(codegen,properties,fnty, core::mem::take(params)); true},
                            _ => {}
                        })*
                    }
            }
            return false
        }
    }
}

fn call_target_intrinsic<F: FunctionRawCodegen>(
    path: &xlang::ir::Path,
    code: &mut FunctionCodegen<F>,
    _fnty: &FnType,
    properties: &TargetProperties,
    params: &mut Vec<VStackValue<F::Loc>>,
) -> bool {
    match &*path.components {
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
                            let params = core::mem::take(params);
                            let val = code.raw_inner().write_intrinsic(*name, params);
                            code.push_value(val);
                            return true;
                        }
                    }
                }
            }

            return false;
        }
        [..] => false,
    }
}

define_xlang_intrinsics! {
    __lccc::intrinsics::C::__builtin_trap | codegen, _, _, _ | => {
        codegen.raw_inner().write_trap(Trap::Abort);
        codegen.push_value(VStackValue::Trapped);
    },
    __lccc::xlang::deoptimize |codegen, _, fnty,params| => {
        let value = params.into_iter().next().unwrap();
        let ret = &fnty.ret;



        let loc = if let XLangSome(loc) = value.opaque_location(){
            loc.clone()
        }else{
            let loc = codegen.raw_inner().allocate(ret,false);
            codegen.move_val(value,loc.clone());
            loc
        };

        let loc = codegen.raw_inner().write_deoptimize(loc);

        let ret = codegen.opaque_value(ret, loc);
        codegen.push_value(ret);
    }
}

define_generic_xlang_intrinsics! {
    __lccc::xlang::__atomic_is_always_lockfree::<(xlang::ir::GenericParameter::Type(ty))> | codegen, _, fnty, _ | => {
        if fnty.params.len()!=0{panic!("bad signature for intrinsic __lccc::xlang::__atomic_is_always_lockfree")}
        match &fnty.ret{
            xlang::ir::Type::Scalar(retty @ xlang::ir::ScalarType{kind: xlang::ir::ScalarTypeKind::Integer{..},..}) => {

                let val = codegen.get_type_information().atomic_is_lock_free(ty).expect("__lccc::xlang::__atomic_is_always_lockfree requires a complete value type") as u128;

                let val = VStackValue::Constant(xlang::ir::Value::Integer{ty: *retty, val});
                codegen.push_value(val);
            }
            _ => panic!("bad signature for intrinsic __lccc::xlang::__atomic_is_always_lockfree")
        }
    },
    __lccc::xlang::__atomic_required_alignment::<(xlang::ir::GenericParameter::Type(ty))> | codegen, _, fnty, _ | => {
        if fnty.params.len()!=0{panic!("bad signature for intrinsic __lccc::xlang::__atomic_required_alignment")}
        match &fnty.ret{
            xlang::ir::Type::Scalar(retty @ xlang::ir::ScalarType{kind: xlang::ir::ScalarTypeKind::Integer{..},..}) => {

                let val = codegen.get_type_information().atomic_required_alignment(ty).expect("__lccc::xlang::__atomic_required_alignment requires a complete value type") as u128;

                let val = VStackValue::Constant(xlang::ir::Value::Integer{ty: *retty, val});
                codegen.push_value(val);
            }
            _ => panic!("bad signature for intrinsic __lccc::xlang::__atomic_required_alignment")
        }
    }
}
