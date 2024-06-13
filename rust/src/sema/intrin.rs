use crate::ast::{Mutability, Safety};

use super::ty::{self, AbiTag, Type};

use super::{DefId, DefinitionInner, Definitions};

use crate::span::{Span, Spanned};

use crate::lang::LangItem;

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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum IntrinsicGenericParam {
    Type,
    Const,
}

macro_rules! parse_safety {
    () => {
        Safety::Safe
    };
    (unsafe) => {
        Safety::Unsafe
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! parse_tag {
    () => {
        $crate::sema::ty::AbiTag::Rust
    };
    (extern "Rust") => {
        $crate::sema::ty::AbiTag::Rust
    };
    (extern "rust-call") => {
        $crate::sema::ty::AbiTag::RustCall
    };
    (extern "rust-intrinsic") => {
        $crate::sema::ty::AbiTag::RustIntrinsic
    };
    (extern "lcrust") => {
        $crate::sema::ty::AbiTag::LCRust(None)
    };
    (extern "lcrust-v0") => {
        $crate::sema::ty::AbiTag::LCRust(Some(0))
    };
    (extern "C") => {
        $crate::sema::ty::AbiTag::C { unwind: false }
    };
    (extern "C-unwind") => {
        $crate::sema::ty::AbiTag::C { unwind: true }
    };
    (extern "system") => {
        $crate::sema::ty::AbiTag::System { unwind: false }
    };
    (extern "system-unwind") => {
        $crate::sema::ty::AbiTag::System { unwind: true }
    };
    (extern $lit:literal) => {{
        compile_error!(concat!("unrecognized abi tag `", stringify!($lit), "`"));
        loop {}
    }};
}
#[doc(hidden)]
pub use parse_tag as __parse_tag;

macro_rules! parse_cvarargs {
    () => {
        false
    };
    (...) => {
        true
    };
}

macro_rules! parse_type_inner {
    (($inner:ty)) => {
        parse_type!($inner)
    };
    (Var$(::)?<$id:literal>) => {
        (super::ty::Type::Param($id))
    };
    ($ident:ident) => {
        super::ty::convert_builtin_type(::core::stringify!($ident)).unwrap() // TODO: also support lang item types and generics
    };
    (&$inner:ty) => {
        {Type::Reference(None, spanned!(Mutability::Const), spanned!(box parse_type!($inner)))}
    };
    (&mut $inner:ty) => {
        {Type::Reference(None, spanned!(Mutability::Mut), spanned!(box parse_type!($inner)))}
    };
    (*const $inner:ty) => {
        {Type::Pointer(spanned!(Mutability::Const), spanned!(box parse_type!($inner)))}
    };
    (*mut $inner:ty) => {
        {Type::Pointer(spanned!(Mutability::Mut), spanned!(box parse_type!($inner)))}
    };
    (($($inner:ty),* $(,)?)) => {
        {{Type::Tuple(vec![$(spanned!(parse_type!($inner))),*])}}
    };
    (!) => {
        Type::Never
    };
    ($(unsafe $(@$_vol:tt)?)? $(extern $lit:literal)? fn($($param:ty),* $(, $(... $(@$_vol2:tt)?)?)?) -> $ret:ty) => {
        Type::FnType (FnType{
            safety: spanned!(parse_safety!($(unsafe $($_vol)?)?)),
            constness: spanned!(Mutability::Const),
            asyncness: spanned!(ty::AsyncType::Normal),
            tag: spanned!(parse_tag!($(extern $lit)?)),
            retty: spanned!(box parse_type!($ret)),
            paramtys: vec![$(spanned!($param)),*],
            iscvarargs: spanned!(parse_cvarargs!($($(... $($_vol2)?)?)?))
        })
    };
}

macro_rules! parse_type {
    ($inner:ty) => {
        {::defile::defile!({parse_type_inner!(@$inner)})}
    };
}

macro_rules! parse_intrinsic_signature {
    ($(unsafe $(@$_vol:tt)?)? fn($($param:ty),* $(,)?) -> $retty:ty) => {
        ty::FnType {
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

macro_rules! parse_intrinsic_generic {
    (type) => {
        IntrinsicGenericParam::Type
    };
    (const) => {
        IntrinsicGenericParam::Const
    };
}

macro_rules! parse_intrinsic_generics {
    ($($param:ident),* $(,)?) => {
        &[$(parse_intrinsic_generic!($param)),*]
    }
}

macro_rules! parse_default_body {
    (;) => {
        None
    };
    ({$($tt:tt)*}) => {
        Some($crate::sema::mir::mir_basic_block!{$($tt)*})
    };
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum IntrinsicAttribute {
    /// Set for intrinsics that do not explicitly diverge and do not produce any AM Results other than UB and non-determinisic operations
    /// This allows lifting to [`InvokeIntrinsic`][crate::sema::mir::MirExpr::InvokeIntrinsic].
    ///
    /// Note that the intrinsic may perform (non-volatile) writes but cannot, for example, abort or panic.
    Unobservable,
    /// Set for intrinsics that can be inlined as an expression
    InlineAsExpr,
}

macro_rules! parse_attribute {
    (unobservable) => {
        $crate::sema::intrin::IntrinsicAttribute::Unobservable
    };
    (inline_as_expr) => {
        $crate::sema::intrin::IntrinsicAttribute::InlineAsExpr
    };
}

macro_rules! def_intrinsics {
    {
        default |$this_block:ident, $next_block:ident, $unwind_block:ident|;
        $($(#[$meta:ident])* $(unsafe $(@$_vol:tt)?)? intrin $name:ident $(<$($gen_param:ident),* $(,)?>)?($($param:ty),* $(,)?) -> $retty:ty $($([$($lang_name:ident = $lang_item:expr),*])? {$($inner_tt:tt)*})? $(; $(@$_vol2:tt)?)?)*
    } => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum IntrinsicDef {
            $($name),*
        }

        impl IntrinsicDef {
            pub fn from_name(s: &str) -> Option<IntrinsicDef> {
                match s{
                    $(::core::stringify!($name) => Some(Self::$name),)*
                    _ => None
                }
            }

            pub fn name(&self) -> &'static str {
                match self{
                    $(Self::$name => ::core::stringify!($name)),*
                }
            }

            pub fn signature(&self) -> ty::FnType {
                match self{
                    $(Self::$name => parse_intrinsic_signature!($(unsafe $($_vol)?)? fn($($param),*) -> $retty)),*
                }
            }

            #[allow(dead_code)]
            pub fn generic_params(&self) -> &'static [IntrinsicGenericParam] {
                match self{
                    $(Self::$name => parse_intrinsic_generics!($($($gen_param),*)?)),*
                }
            }

            #[allow(dead_code, unused_variables)]
            pub fn default_body(&self, defs: &$crate::sema::Definitions, $this_block: $crate::sema::mir::BasicBlockId, $next_block: $crate::sema::mir::BasicBlockId, $unwind_block: $crate::sema::mir::BasicBlockId) -> Option<$crate::sema::mir::MirBasicBlock>{
                match self{
                    $(Self::$name => {
                        $($($(let $lang_name = defs.get_lang_item($lang_item)?;)*)?)?
                        parse_default_body!($({$($inner_tt)*})? $(; $(@$_vol2)?)?)
                    }),*
                }
            }

            #[allow(dead_code)]
            pub fn attributes(&self) -> &'static [$crate::sema::intrin::IntrinsicAttribute]{
                match self{
                    $(Self::$name => &[$(parse_attribute!($meta)),*]),*
                }
            }
        }
    }
}

def_intrinsics! {
    default |this,next,unwind|;
    #[unobservable]
    unsafe intrin __builtin_unreachable() -> !{
        @0: { []
            unreachable
        }
    }
    #[unobservable]
    unsafe intrin __builtin_assume(bool) -> ();
    intrin __builtin_abort() -> !;
    #[unobservable]
    intrin impl_id() -> &str;
    unsafe intrin __builtin_allocate<type>(Var<0>) -> *mut u8;
    unsafe intrin __builtin_deallocate<type>(Var<0>, *mut u8) -> ();
    #[unobservable]
    intrin type_id<type>() -> (*const u8, usize);
    #[unobservable]
    intrin type_name<type>() -> &str;
    intrin destroy_at<type>(*mut Var<0>) -> ();
    #[unobservable]
    intrin discriminant<type, type>(&Var<0>) -> Var<1>;
    #[unobservable]
    unsafe intrin transmute<type, type>(Var<0>) -> Var<1>;
    intrin black_box<type>(Var<0>) -> Var<1>;

    unsafe intrin construct_in_place<type, type, type>(*mut Var<0>, Var<1>, Var<2>) -> ();

    #[unobservable]
    unsafe intrin __builtin_read<type>(*const Var<0>) -> Var<0>{
        @<this>: { [_0: *const %0]
            return read(*_0)
        }
    }
    #[unobservable]
    unsafe intrin __builtin_read_freeze<type>(*const Var<0>) -> Var<0>;
    unsafe intrin __builtin_read_volatile<type>(*const Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __builtin_write<type>(*mut Var<0>,Var<0>) -> ();
    unsafe intrin __builtin_write_volatile<type>(*mut Var<0>, Var<0>) -> ();

    #[unobservable]
    intrin __builtin_size_of<type>() -> usize;
    #[unobservable]
    intrin __builtin_align_of<type>() -> usize;

    #[unobservable]
    intrin __builtin_size_of_val<type>(*const Var<0>) -> usize;
    #[unobservable]
    intrin __builtin_align_of_val<type>(*const Var<0>) -> usize;

    #[unobservable]
    intrin __builtin_likely(bool) -> bool;
    #[unobservable]
    intrin __builtin_unlikely(bool) -> bool;

    #[unobservable]
    intrin __builtin_cmp<type, type>(Var<0>, Var<0>) -> Var<1>;
    #[unobservable]
    intrin __builtin_max<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_min<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_clamp<type>(Var<0>, Var<0>, Var<0>) -> Var<0>;

    #[unobservable]
    intrin __builtin_fadd_fast<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fsub_fast<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fmul_fast<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fdiv_fast<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_frem_fast<type>(Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_ffma_fast<type>(Var<0>, Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fneg_fast<type>(Var<0>) -> Var<0>;

    #[unobservable]
    unsafe intrin __atomic_load<type, const>(*mut Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_store<type, const>(*mut Var<0>, Var<0>) -> ();
    #[unobservable]
    unsafe intrin __atomic_compare_exchange_strong<type, const, const>(*mut Var<0>, *mut Var<0>, Var<0>) -> bool;
    #[unobservable]
    unsafe intrin __atomic_compare_exchange_weak<type, const, const>(*mut Var<0>, *mut Var<0>, Var<0>) -> bool;
    #[unobservable]
    unsafe intrin __atomic_swap<type, const>(*mut Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_fetch_add<type, const>(*mut Var<0>, Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_fetch_sub<type, const>(*mut Var<0>, Var<0>) -> Var<0>;

    #[unobservable]
    unsafe intrin __atomic_begin_transaction<type>(*mut Var<0>) -> bool;

    #[unobservable]
    unsafe intrin __atomic_read_in_transaction<type, const>(*mut Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_write_in_transaction<type, const>(*mut Var<0>, Var<0>) -> ();
    #[unobservable]
    unsafe intrin __atomic_commit_transaction<type>(*mut Var<0>) -> i32;
}
