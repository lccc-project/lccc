use crate::ast::{Mutability, Safety};

use super::ty::{self, AbiTag, Type};

use super::{DefId, DefinitionInner, Definitions};

use crate::span::{Span, Spanned};

macro_rules! spanned {
    (box $expr:expr) => {
        Box::new(Spanned {
            body: $expr,
            span: Span::synthetic(),
        })
    };
    ($expr:expr) => {
        Spanned {
            body: $expr,
            span: Span::synthetic(),
        }
    };
}

macro_rules! parse_safety {
    () => {
        Safety::Safe
    };
    (unsafe) => {
        Safety::Unsafe
    };
}

macro_rules! parse_tag {
    () => {
        AbiTag::Rust
    };
    (extern "Rust") => {
        AbiTag::Rust
    };
    (extern "rust-call") => {
        AbiTag::RustCall
    };
    (extern "rust-intrinsic") => {
        AbiTag::RustIntrinsic
    };
    (extern "lcrust") => {
        AbiTag::LCRust(None)
    };
    (extern "lcrust-v0") => {
        AbiTag::LCRust(Some(0))
    };
    (extern "C") => {
        AbiTag::C { unwind: false }
    };
    (extern "C-unwind") => {
        AbiTag::C { unwind: true }
    };
    (extern "system") => {
        AbiTag::System { unwind: false }
    };
    (extern "system-unwind") => {
        AbiTag::System { unwind: true }
    };
    (extern $lit:literal) => {{
        compile_error!(concat!("unrecognized abi tag `", stringify!($lit), "`"));
        loop {}
    }};
}

macro_rules! parse_cvarargs {
    () => {
        false
    };
    (...) => {
        true
    };
}

macro_rules! parse_type_inner{
    (($inner:ty)) => {
        parse_type!($inner)
    };
    ($ident:ident) => {
        super::ty::convert_builtin_type(::core::stringify!($ident)).unwrap() // TODO: also support lang item types and generics
    };
    (&$($inner:ty)*) => {
        {Type::Reference(None, spanned!(Mutability::Const), spanned!(box parse_type!($($inner)*)))}
    };
    (&mut $($inner:ty)*) => {
        {Type::Reference(None, spanned!(Mutability::Mut), spanned!(box parse_type!($($inner)*)))}
    };
    (*const $($inner:ty)*) => {
        {Type::Pointer(spanned!(Mutability::Const), spanned!(box parse_type!($($inner)*)))}
    };
    (*mut $($inner:ty)*) => {
        {Type::Pointer(spanned!(Mutability::Mut), spanned!(box parse_type!($($inner)*)))}
    };
    (($($inner:ty),* $(,)?)) => {
        {{Type::Tuple(vec![$(spanned!(parse_type!($inner))),*])}}
    };
    (!) => {
        Type::Never
    };
    ($(unsafe $(@$_vol:tt)?)? $(extern $lit:literal)? fn($($param:ty),* $(, $(... $(@$_vol2:tt)?)?)?) -> $ret:ty) => {
        Type::FnType{
            safety: spanned!(parse_safety!($(unsafe $($_vol)?)?)),
            constness: spanned!(Mutability::Const),
            asyncness: spanned!(ty::AsyncType::Normal),
            tag: spanned!(parse_tag!($(extern $lit)?)),
            retty: spanned!(box parse_type!($ret)),
            paramtys: vec![$(spanned!($param)),*],
            iscvarargs: spanned!(parse_cvarargs!($($(... $($_vol2)?)?)?))
        }
    };
}

macro_rules! parse_type{
    ($inner:ty) => {
        {::defile::defile!({parse_type_inner!(@$inner)})}
    };
}

macro_rules! parse_intrinsic_signature{
    ($(unsafe $(@$_vol:tt)?)? fn($($param:ty),* $(,)?) -> $retty:ty) => {
        ty::FnType{
            safety: spanned!(parse_safety!($(unsafe $($_vol)?)?)),
            constness: spanned!(Mutability::Const),
            asyncness: spanned!(ty::AsyncType::Normal),
            tag: spanned!(AbiTag::RustIntrinsic),
            retty: spanned!(box parse_type!($retty)),
            paramtys: vec![$(spanned!(parse_type!($param))),*],
            iscvarargs: spanned!(false),
        }
    };
}

macro_rules! def_intrinsics{
    {
        $($(unsafe $(@$_vol:tt)?)? intrin $name:ident($($param:ty),* $(,)?) -> $retty:ty;)*
    } => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum IntrinsicDef{
            $($name),*
        }

        impl IntrinsicDef{
            pub fn from_name(s: &str) -> Option<IntrinsicDef>{
                match s{
                    $(::core::stringify!($name) => Some(Self::$name),)*
                    _ => None
                }
            }

            pub fn name(&self) -> &'static str{
                match self{
                    $(Self::$name => ::core::stringify!($name)),*
                }
            }

            pub fn signature(&self) -> ty::FnType{
                match self{
                    $(Self::$name => parse_intrinsic_signature!($(unsafe $($_vol)?)? fn($($param),*) -> $retty)),*
                }
            }

            pub fn register_intrinsics(defs: &mut Definitions){
                $({
                    let defid = defs.allocate_defid();
                    let intrin = Self::$name;
                    let sig = intrin.signature();

                    defs.definition_mut(defid).inner = spanned!(DefinitionInner::Function(sig,Some(super::FunctionBody::Intrinsic(intrin))));
                    defs.intrinsics.insert(intrin,defid);
                })*
            }
        }
    }
}

def_intrinsics! {
    unsafe intrin __builtin_unreachable() -> !;
    intrin __builtin_abort() -> !;
    intrin impl_id() -> &str;
    unsafe intrin __builtin_allocate(usize, usize) -> *mut u8;
    unsafe intrin __builtin_deallocate(usize, usize, *mut u8) -> ();
}
