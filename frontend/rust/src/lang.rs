#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LangItemTarget {
    Type,
    Variant,
    Function,
    #[allow(dead_code)]
    AssociatedType,
    AssociatedFunction,
    Trait,
    ImplBlock,
}

macro_rules! define_lang_items {
    {
        $($enum:ident: $name:ident @ $target:ident),* $(,)?
    } => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[repr(u32)]
        pub enum LangItem {
            $(
                $enum
            ),*
        }

        impl LangItem {
            pub fn from_name(x: &str) -> Option<Self> {
                match x {
                    $(::core::stringify!($name) => Some(Self::$enum),)*
                    _ => None
                }
            }

            pub const fn name(&self) -> &'static str {
                match self {
                    $(Self::$enum => ::core::stringify!($name)),*
                }
            }

            pub const fn target(&self) -> LangItemTarget {
                match self {
                    $(Self::$enum => LangItemTarget::$target),*
                }
            }
        }
    }
}

define_lang_items! {
    Copy: copy @ Trait,
    Clone: clone @ Trait,
    IntoIterator: into_iterator @ Trait,
    IteratorNext: iterator_next @ AssociatedFunction,
    Some: some @ Variant,
    None: none @ Variant,
    FnOnce: fn_once @ Trait,
    FnMut: fn_mut @ Trait,
    Fn: fn_trait @ Trait,
    Main: main @ Function,
    Bool: bool @ ImplBlock,
    Unit: unit @ ImplBlock,
    Tuple: tuple @ ImplBlock,
    Slice: slice @ ImplBlock,
    Array: array @ ImplBlock,
    I8:    i8 @ ImplBlock,
    I16:   i16 @ ImplBlock,
    I32:   i32 @ ImplBlock,
    I64:   i64 @ ImplBlock,
    I128:  i128 @ ImplBlock,
    ISize: isize @ ImplBlock,
    U8:    u8 @ ImplBlock,
    U16:   u16 @ ImplBlock,
    U32:   u32 @ ImplBlock,
    U64:   u64 @ ImplBlock,
    U128:  u128 @ ImplBlock,
    USize: usize @ ImplBlock,
    F32: f32 @ ImplBlock,
    F64: f64 @ ImplBlock,
    F32Rt: f32_rt @ ImplBlock,
    F64Rt: f64_rt @ ImplBlock,
    F16: f16 @ ImplBlock,
    F128: f128 @ ImplBlock,
    F16Rt: f16_rt @ ImplBlock,
    F128Rt: f128_rt @ ImplBlock,
    Bf16: bf16 @ ImplBlock,
    Bf16Rt: bf16_rt @ ImplBlock,
    Fx80: fx80 @ ImplBlock,
    Fx80Rt: fx80_rt @ ImplBlock,
    Fd128: fd128 @ ImplBlock,
    Fd128Rt: fd128_rt @ ImplBlock,
    ConstPtr: const_ptr @ ImplBlock,
    MutPtr: mut_ptr @ ImplBlock,
    Char: char @ ImplBlock,
    Str: str @ ImplBlock,
    Never: never @ ImplBlock,
    LayoutTy: layout @ Type,
    AllocSym: alloc_sym @ Function,
    DeallocSym: dealloc_sym @ Function,
    VaList : va_list @ Type,
}
