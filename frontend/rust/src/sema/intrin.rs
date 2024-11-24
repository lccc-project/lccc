use crate::ast::{Mutability, Safety, StringType};
use crate::interning::Symbol;
use crate::mir_expr;

use super::ty::{self, AbiTag, Type};

use super::{attr::Attr, generics::GenericArg, mir, DefId, DefinitionInner, Definitions};

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
            params: vec![$(spanned!($crate::sema::ty::FnParam{ty: spanned!(parse_type!(|$defs| $param)),attrs: ::std::vec![]})),*],
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
            params: vec![$(spanned!($crate::sema::ty::FnParam{ty: spanned!(parse_type!(|$defs| $param)),attrs: ::std::vec![]})),*],
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
    ///
    /// ## Notes
    /// In general, the intrinsic must have a default body which satisfies the following conditions:
    /// * It does not contain any statements other than `let` statements or `write` statements
    /// * The terminator is one of the following:
    ///     * `unreachable`
    ///     * `return`
    ///     * `tailcall`, with no unwind block and the target must be another intrinsic annotated with `#[unobservable]`
    ///
    /// If the intrinsic does not have a default body, the intrinsic requires special xlang support to lower. Do not apply this intrinsic this without approval from #lcrust-irgen channel.
    Unobservable,
    /// Set for intrinsics that can be inlined as an expression, This is a strict subset of the rules for `#[unobservable]`.
    /// The intrinsic must have a default body which only contains one of the allowed terminators
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
        default $defs:ident |$this_block:ident, $next_block:ident, $unwind_block:ident, $caller:ident| <$generics:ident>;

        $(#[doc = $($doc_tt:tt)*] $(#[$meta:ident])* $(unsafe $(@$_vol:tt)?)? intrin $name:ident $(<$($gen_param:ident),* $(,)?>)?($($param_name:ident: $param:ty),* $(,)?) -> $retty:ty $($([$($lang_name:ident = $lang_item:expr),*])? {$($inner_tt:tt)*} $(const |$interp_block:ident| $cx_block:block)?)?  $(; $(@$_vol2:tt)? $(const |$interp_semi:ident| $cx_semi:block)?)?)*
    } => {
        #[derive(Copy, Clone, Hash, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        pub enum IntrinsicDef {
            $(#[doc = $($doc_tt)*] $name),*
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
            pub fn default_body(&self, $defs: &$crate::sema::Definitions, $this_block: $crate::sema::mir::BasicBlockId, $next_block: $crate::sema::mir::BasicBlockId, $unwind_block: $crate::sema::mir::BasicBlockId, $generics: &$crate::sema::generics::GenericArgs, $caller: $crate::sema::DefId) -> Option<$crate::sema::mir::MirBasicBlock>{
                match self{
                    $(Self::$name => {
                        $($($(let $lang_name = $defs.get_lang_item($lang_item)?;)*)?)?
                        parse_default_body!($({$($inner_tt)*})? $(; $(@$_vol2)?)?)
                    }),*
                }
            }

            #[allow(dead_code, unused_variables)]
            pub fn eval_const(&self, $defs: &$crate::sema::Definitions, $generics: &$crate::sema::generics::GenericArgs, interp: &mut $crate::sema::cx::eval::MirEvaluator, params: Vec<$crate::sema::cx::eval::CxEvalValue>, $caller: $crate::sema::DefId) -> $crate::sema::cx::Result<$crate::sema::cx::eval::CxEvalValue>{
                match self{
                    $(Self::$name => {
                        $($({
                            let $interp_block = &mut *interp;
                            return core::result::Result::Ok($cx_block)
                        })?)?
                        $($({
                            let $interp_semi = &mut *interp;
                            return core::result::Result::Ok($cx_semi)
                        })?)?
                        core::result::Result::Err($crate::sema::cx::ConstEvalError::UnsupportedIntrinsic(Self::$name))
                    })*
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
    default defs |this,next,unwind, caller|<generics>;
    ///
    #[unobservable]
    unsafe intrin __builtin_unreachable() -> !{
        @0: { []
            unreachable
        }
    }
    ///
    #[unobservable] // compiled as no-op in expression position
    unsafe intrin __builtin_assume(val: bool) -> ();
    ///
    intrin __builtin_abort() -> !;
    ///
    #[unobservable]
    intrin impl_id() -> &str{
        @<this>: { []
            return <{
                const IMPL_ID: &str = core::concat!("lccc v", core::env!("CARGO_PKG_VERSION"));

                mir::MirExpr::ConstString(StringType::Default, Symbol::intern(IMPL_ID))
            }>
        }
    }
    ///
    unsafe intrin __builtin_allocate<type>(layout: Var<0>) -> *mut u8 [alloc_sym = LangItem::AllocSym] {
        @<this>: {[_0: %0]
            tailcall get_symbol(#<alloc_sym>): fn(%0)->*mut u8 (_0) unwind @<unwind> []
        }
    }
    ///
    unsafe intrin __builtin_deallocate<type>(layout: Var<0>, ptr: *mut u8) -> () [dealloc_sym = LangItem::DeallocSym] {
        @<this>: { [_0: %0, _1: *mut u8]
            tailcall get_symbol(#<dealloc_sym>): fn(%0, *mut u8)->() (_0, _1) unwind @<unwind> []
        }
    }
    ///
    #[unobservable] // FIXME: Add body
    intrin type_id<type>() -> (*const u8, usize);
    ///
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
    ///
    intrin destroy_at<type>(ptr: *mut Var<0>) -> (){
        @<this>: { [_0: *const %0]
            drop _0 next @<next> [] unwind @<unwind> []
        }
    }
    ///
    #[unobservable] // FIXME: Add body
    intrin discriminant<type, type>(val: *const Var<0>) -> Var<1>;
    ///
    intrin black_box<type>(val: Var<0>) -> Var<1>;

    ///
    unsafe intrin construct_in_place<type, type, type>(dest: *mut Var<0>, f: Var<1>, params: Var<2>) -> ();

    ///
    #[unobservable]
    unsafe intrin __builtin_read<type>(ptr: *const Var<0>) -> Var<0>{
        @<this>: { [_0: *const %0]
            return read(*_0)
        }
    }

    ///
    #[unobservable]
    unsafe intrin __builtin_read_freeze<type>(ptr: *const Var<0>) -> Var<0>{
        @<this>: { [_0: *const %0]
            return read freeze(*_0)
        }
    }

    ///
    unsafe intrin __builtin_read_volatile<type>(ptr: *const Var<0>) -> Var<0>;

    ///
    #[unobservable] // FIXME: add body
    unsafe intrin __builtin_write<type>(ptr: *mut Var<0>,val: Var<0>) -> ();

    ///
    unsafe intrin __builtin_write_volatile<type>(ptr: *mut Var<0>, val: Var<0>) -> ();

    ///
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

    ///
    #[unobservable]
    intrin __builtin_align_of<type>() -> usize{
        @<this>: { []
            return <{
                let param_ty = match generics.params.get(0){
                    Some(GenericArg::Type(ty)) => ty,
                    _ => panic!("Expected a generic type for argument 0 of `__builtin_align_of`")
                };

                let align = defs.align_of(param_ty).expect("Expected a `Sized` type for type argument `0` of `__builtin_align_of`");

                mir::MirExpr::ConstInt(ty::IntType::usize, align as u128)
            }>
        }
    }

     ///
     #[unobservable]
     intrin __builtin_known_align_of<type>() -> usize{
         @<this>: { []
             return <{
                 let param_ty = match generics.params.get(0){
                     Some(GenericArg::Type(ty)) => ty,
                     _ => panic!("Expected a generic type for argument 0 of `__builtin_known_align_of`")
                 };

                 let align = defs.align_of(param_ty).unwrap_or(1);

                 mir::MirExpr::ConstInt(ty::IntType::usize, align as u128)
             }>
         }
     }

    ///
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

    ///
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

    ///
    #[unobservable]
    intrin __builtin_likely(val: bool) -> bool{
        @<this>: { [_0: bool]
            return _0
        }
    }

    ///
    #[unobservable]
    intrin __builtin_unlikely(val: bool) -> bool{
        @<this>: { [_0: bool]
            return _0
        }
    }

    ///
    #[unobservable] // unobservable because lowers to xlang cmp instruction
    intrin __builtin_cmp<type, type>(a: Var<0>, b: Var<0>) -> Var<1>;

    ///
    #[unobservable] // unobservable because lowers to xlang cmpgt + select
    intrin __builtin_max<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to xlang cmplt + select
    intrin __builtin_min<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to xlang select
    intrin __builtin_clamp<type>(val: Var<0>, min: Var<0>, max: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang add
    intrin __builtin_fadd_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang sub
    intrin __builtin_fsub_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang mul
    intrin __builtin_fmul_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang div
    intrin __builtin_fdiv_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang mod
    intrin __builtin_frem_fast<type>(a: Var<0>, b: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang ffma
    intrin __builtin_ffma_fast<type>(a: Var<0>, b: Var<0>, c: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because can be lowered to xlang neg
    intrin __builtin_fneg_fast<type>(a: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to load
    unsafe intrin __atomic_load<type, const>(ptr: *mut Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to assign
    unsafe intrin __atomic_store<type, const>(dest: *mut Var<0>, val: Var<0>) -> ();

    ///
    #[unobservable] // unobservable because lowers to cmpxchg
    unsafe intrin __atomic_compare_exchange_strong<type, const, const>(dest: *mut Var<0>, expected: *mut Var<0>, new: Var<0>) -> bool;

    ///
    #[unobservable] // unobservable because lowers to wcmpxchg
    unsafe intrin __atomic_compare_exchange_weak<type, const, const>(dest: *mut Var<0>, expected: *mut Var<0>, new: Var<0>) -> bool;

    ///
    #[unobservable] // unobservable because lowers to xchg
    unsafe intrin __atomic_swap<type, const>(dest: *mut Var<0>, new: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to fetch_assign add
    unsafe intrin __atomic_fetch_add<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to fetch_assign sub
    unsafe intrin __atomic_fetch_sub<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to fetch_assign and
    unsafe intrin __atomic_fetch_and<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to fetch_assign or
    unsafe intrin __atomic_fetch_or<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;

    ///
    #[unobservable] // unobservable because lowers to fetch_assign xor
    unsafe intrin __atomic_fetch_xor<type, const>(dest: *mut Var<0>, val: Var<0>) -> Var<0>;

    ///
    unsafe intrin __atomic_read_in_transaction<type, const>(ptr: *mut Var<0>) -> Var<0>;

    ///
    unsafe intrin __atomic_write_in_transaction<type, const>(ptr: *mut Var<0>, val: Var<0>) -> ();

    ///
    unsafe intrin __atomic_commit_transaction<type>(ptr: *mut Var<0>) -> i32;

    ///
    unsafe intrin __builtin_va_arg<type>(va_list: *mut Lang<VaList>) -> Var<0>;

    ///
    unsafe intrin __builtin_va_copy(va_list: *mut Lang<VaList>, out:*mut Lang<VaList>)->();

    ///
    unsafe intrin __builtin_va_end(va_list: *mut Lang<VaList>) -> ();

    ///
    #[unobservable]
    intrin __builtin_is_target_feature_enabled<const>() -> bool {
        @<this>: { []
            return <{
                let feature = match &generics.params[0] {
                    GenericArg::Const(const_expr) => defs.evaluate_as_string(Spanned{body: const_expr, span: Span::synthetic()}, caller, caller).expect("Expected parameter 0 to be of type &str"),
                    _ => panic!("Expected parameter 0 to be a constant")
                };

                let val = if defs.global_features().contains(&*feature){
                    true
                }else{
                    let caller = defs.definition(caller);
                    caller.attrs.iter().filter_map(|attr| match &**attr{
                        Attr::TargetFeature(features) => Some(features),
                        _ => None
                    }).flatten().any(|enabled| feature == enabled.body)
                };
                mir::MirExpr::ConstBool(val)
            }>
        }
    }
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
