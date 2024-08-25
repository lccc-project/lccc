use crate::ast::{Mutability, Safety, StringType};
use crate::interning::Symbol;

use super::ty::{self, AbiTag, Type};

use super::{generics::GenericArg, mir, DefId, DefinitionInner, Definitions};

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
    (|$defs:ident| ($inner:ty)) => {
        parse_type!($inner)
    };
    (|$defs:ident| Var$(::)?<$id:literal>) => {
        (super::ty::Type::Param(super::generics::ParamId::__new_unchecked($id)))
    };
    (|$defs:ident| Lang$(::)?<$id:ident>) => {
        (super::ty::Type::UnresolvedLangItem($crate::lang::LangItem::$id, super::generics::GenericArgs::default()))
    };
    (|$defs:ident| $ident:ident) => {
        super::ty::convert_builtin_type(::core::stringify!($ident)).unwrap() // TODO: also support lang item types and generics
    };
    (|$defs:ident| &$inner:ty) => {
        {Type::Reference(None, spanned!(Mutability::Const), spanned!(box parse_type!(|$defs| $inner)))}
    };
    (|$defs:ident| &mut $inner:ty) => {
        {Type::Reference(None, spanned!(Mutability::Mut), spanned!(box parse_type!(|$defs| $inner)))}
    };
    (|$defs:ident| *const $inner:ty) => {
        {Type::Pointer(spanned!(Mutability::Const), spanned!(box parse_type!(|$defs| $inner)))}
    };
    (|$defs:ident| *mut $inner:ty) => {
        {Type::Pointer(spanned!(Mutability::Mut), spanned!(box parse_type!(|$defs| $inner)))}
    };
    (|$defs:ident| ($($inner:ty),* $(,)?)) => {
        {{Type::Tuple(vec![$(spanned!(parse_type!(|$defs| $inner))),*])}}
    };
    (|$_defs:ident| !) => {
        Type::Never
    };
    (|$defs:ident| $(unsafe $(@$_vol:tt)?)? $(extern $lit:literal)? fn($($param:ty),* $(, $(... $(@$_vol2:tt)?)?)?) -> $ret:ty) => {
        Type::FnType (FnType{
            safety: spanned!(parse_safety!($(unsafe $($_vol)?)?)),
            constness: spanned!(Mutability::Const),
            asyncness: spanned!(ty::AsyncType::Normal),
            tag: spanned!(parse_tag!($(extern $lit)?)),
            retty: spanned!(box parse_type!(|$defs| $ret)),
            paramtys: vec![$(spanned!(parse_type!(|$defs| $param))),*],
            iscvarargs: spanned!(parse_cvarargs!($($(... $($_vol2)?)?)?))
        })
    };
}

macro_rules! parse_type {
    (|$defs:ident| $inner:ty) => {
        {::defile::defile!({parse_type_inner!(|$defs| @$inner)})}
    };
}

macro_rules! parse_intrinsic_signature {
    (|$defs:ident| $(unsafe $(@$_vol:tt)?)? fn($($param:ty),* $(,)?) -> $retty:ty) => {
        ty::FnType {
            safety: spanned!(parse_safety!($(unsafe $($_vol)?)?)),
            constness: spanned!(Mutability::Const),
            asyncness: spanned!(ty::AsyncType::Normal),
            tag: spanned!(AbiTag::RustIntrinsic),
            retty: spanned!(box parse_type!(|$defs| $retty)),
            paramtys: vec![$(spanned!(parse_type!(|$defs| $param))),*],
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
        default $defs:ident |$this_block:ident, $next_block:ident, $unwind_block:ident| <$generics:ident>;
        $($(#[$meta:ident])* $(unsafe $(@$_vol:tt)?)? intrin $name:ident $(<$($gen_param:ident),* $(,)?>)?($($param_name:ident: $param:ty),* $(,)?) -> $retty:ty $($([$($lang_name:ident = $lang_item:expr),*])? {$($inner_tt:tt)*})? $(; $(@$_vol2:tt)?)?)*
    } => {
        #[derive(Copy, Clone, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum IntrinsicDef {
            $($name),*
        }

        impl IntrinsicDef {

            // pub const fn param_count(&self) -> u32{
            //     match self{
            //         $(Self:: $name => {
            //             const __ARR: &[&str] = &[$(::core::stringify!($param_name)),*];

            //             __ARR.len() as u32
            //         })*
            //     }
            // }

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
                    $(Self::$name => parse_intrinsic_signature!(|defs| $(unsafe $($_vol)?)? fn($($param),*) -> $retty)),*
                }
            }

            #[allow(dead_code)]
            pub fn generic_params(&self) -> &'static [IntrinsicGenericParam] {
                match self{
                    $(Self::$name => parse_intrinsic_generics!($($($gen_param),*)?)),*
                }
            }

            #[allow(dead_code, unused_variables)]
            pub fn default_body(&self, $defs: &$crate::sema::Definitions, $this_block: $crate::sema::mir::BasicBlockId, $next_block: $crate::sema::mir::BasicBlockId, $unwind_block: $crate::sema::mir::BasicBlockId, $generics: &$crate::sema::generics::GenericArgs) -> Option<$crate::sema::mir::MirBasicBlock>{
                match self{
                    $(Self::$name => {
                        $($($(let $lang_name = $defs.get_lang_item($lang_item)?;)*)?)?
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
    default defs |this,next,unwind|<generics>;
    #[unobservable]
    unsafe intrin __builtin_unreachable() -> !{
        @0: { []
            unreachable
        }
    }
    #[unobservable]
    unsafe intrin __builtin_assume(val: bool) -> ();
    intrin __builtin_abort() -> !;
    #[unobservable]
    intrin impl_id() -> &str{
        @<this>: { []
            return <{
                const IMPL_ID: &str = core::concat!("lccc v", core::env!("CARGO_PKG_VERSION"));

                mir::MirExpr::ConstString(StringType::Default, Symbol::intern(IMPL_ID))
            }>
        }
    }
    unsafe intrin __builtin_allocate<type>(layout: Var<0>) -> *mut u8 [alloc_sym = LangItem::AllocSym] {
        @<this>: {[_0: %0]
            tailcall get_symbol(#<alloc_sym>): fn(%0)->*mut u8 (_0) unwind @<unwind> []
        }
    }
    unsafe intrin __builtin_deallocate<type>(layout: Var<0>, ptr: *mut u8) -> ();
    #[unobservable]
    intrin type_id<type>() -> (*const u8, usize);
    #[unobservable]
    intrin type_name<type>() -> &str{
        @<this>: { []
            return <{
                let param_ty = match generics.params.get(0){
                    Some(GenericArg::Type(ty)) => ty,
                    _ => panic!("Expected a generic type for argument 0 of `type_name`")
                };
                let name = defs.type_name(param_ty);

                mir::MirExpr::ConstString(StringType::Default, name)
            }>
        }
    }
    intrin destroy_at<type>(ptr: *mut Var<0>) -> (){
        @<this>: { [_0: *const %0]
            drop _0 next @<next> [] unwind @<unwind> []
        }
    }
    #[unobservable]
    intrin discriminant<type, type>(val: *const Var<0>) -> Var<1>;
    #[unobservable]
    unsafe intrin transmute<type, type>(val: Var<0>) -> Var<1>;
    intrin black_box<type>(val: Var<0>) -> Var<1>;

    unsafe intrin construct_in_place<type, type, type>(dest: *mut Var<0>, f: Var<1>, params: Var<2>) -> ();

    #[unobservable]
    unsafe intrin __builtin_read<type>(ptr: *const Var<0>) -> Var<0>{
        @<this>: { [_0: *const %0]
            return read(*_0)
        }
    }
    #[unobservable]
    unsafe intrin __builtin_read_freeze<type>(ptr: *const Var<0>) -> Var<0>;
    unsafe intrin __builtin_read_volatile<type>(ptr: *const Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __builtin_write<type>(ptr: *mut Var<0>,val: Var<0>) -> ();
    unsafe intrin __builtin_write_volatile<type>(ptr: *mut Var<0>, val: Var<0>) -> ();

    #[unobservable]
    intrin __builtin_size_of<type>() -> usize{
        @<this>: { []
            return <{
                let param_ty = match generics.params.get(0){
                    Some(GenericArg::Type(ty)) => ty,
                    _ => panic!("Expected a generic type for argument 0 of `__builtin_size_of`")
                };

                let size = defs.size_of(param_ty).expect("Expected a `Sized` type for type argument `0` of `__builtin_size_of`");

                mir::MirExpr::ConstInt(ty::IntType::usize, size as u128)
            }>
        }
    }
    #[unobservable]
    intrin __builtin_align_of<type>() -> usize{
        @<this>: { []
            return <{
                let param_ty = match generics.params.get(0){
                    Some(GenericArg::Type(ty)) => ty,
                    _ => panic!("Expected a generic type for argument 0 of `__builtin_size_of`")
                };

                let align = defs.align_of(param_ty).expect("Expected a `Sized` type for type argument `0` of `__builtin_size_of`");

                mir::MirExpr::ConstInt(ty::IntType::usize, align as u128)
            }>
        }
    }

    #[unobservable]
    intrin __builtin_size_of_val<type>(val: *const Var<0>) -> usize{
        @<this>: { [_0: *const %0]
            return <{
                match &generics.params[0]{
                    GenericArg::Type(ty) => {
                        let layout = defs.layout_of(ty, DefId::ROOT, DefId::ROOT);
                        if let Some(size) = layout.size{
                            mir::MirExpr::ConstInt(ty::IntType::usize, size as u128)
                        }else{
                            todo!("Unsized types")
                        }
                    }
                    _ => panic!("Expected a generic type for argument 0 of `__builtin_size_of_val`")
                }
            }>
        }
    }
    #[unobservable]
    intrin __builtin_align_of_val<type>(val: *const Var<0>) -> usize{
        @<this>: { [_0: *const %0]
            return <{
                match &generics.params[0]{
                    GenericArg::Type(ty) => {
                        let layout = defs.layout_of(ty, DefId::ROOT, DefId::ROOT);
                        if let Some(align) = layout.align{
                            mir::MirExpr::ConstInt(ty::IntType::usize, align as u128)
                        }else{
                            todo!("Trait objects")
                        }
                    }
                    _ => panic!("Expected a generic type for argument 0 of `__builtin_size_of_val`")
                }
            }>
        }
    }

    #[unobservable]
    intrin __builtin_likely(val: bool) -> bool;
    #[unobservable]
    intrin __builtin_unlikely(val: bool) -> bool;

    #[unobservable]
    intrin __builtin_cmp<type, type>(a: Var<0>, b: Var<0>) -> Var<1>;
    #[unobservable]
    intrin __builtin_max<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_min<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_clamp<type>(val: Var<0>, min: Var<0>, max: Var<0>) -> Var<0>;

    #[unobservable]
    intrin __builtin_fadd_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fsub_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fmul_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fdiv_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_frem_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_ffma_fast<type>(a: Var<0>, b: Var<0>, c: Var<0>) -> Var<0>;
    #[unobservable]
    intrin __builtin_fneg_fast<type>(a: Var<0>) -> Var<0>;

    #[unobservable]
    unsafe intrin __atomic_load<type, const>(ptr: *mut Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_store<type, const>(dest: *mut Var<0>, val: Var<0>) -> ();
    #[unobservable]
    unsafe intrin __atomic_compare_exchange_strong<type, const, const>(dest: *mut Var<0>, expected: *mut Var<0>, new: Var<0>) -> bool;
    #[unobservable]
    unsafe intrin __atomic_compare_exchange_weak<type, const, const>(dest: *mut Var<0>, expected: *mut Var<0>, new: Var<0>) -> bool;
    #[unobservable]
    unsafe intrin __atomic_swap<type, const>(dest: *mut Var<0>, new: Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_fetch_add<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_fetch_sub<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;


    #[unobservable]
    unsafe intrin __atomic_read_in_transaction<type, const>(ptr: *mut Var<0>) -> Var<0>;
    #[unobservable]
    unsafe intrin __atomic_write_in_transaction<type, const>(ptr: *mut Var<0>, val: Var<0>) -> ();
    #[unobservable]
    unsafe intrin __atomic_commit_transaction<type>(ptr: *mut Var<0>) -> i32;


    unsafe intrin __builtin_va_arg<type>(va_list: *mut Lang<VaList>) -> Var<0>;
    unsafe intrin __builtin_va_copy(va_list: *mut Lang<VaList>, out:*mut Lang<VaList>)->();
    unsafe intrin __builtin_va_end(va_list: *mut Lang<VaList>) -> ();
}

impl core::fmt::Display for IntrinsicDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl core::fmt::Debug for IntrinsicDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}
