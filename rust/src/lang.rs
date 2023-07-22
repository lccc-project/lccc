#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LangItemTarget {
    Type,
    Variant,
    Function,
    AssociatedType,
}

macro_rules! define_lang_items{
    {
        $($enum:ident: $name:ident),* $(,)?
    } => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        #[repr(u32)]
        pub enum LangItem{
            $(
                $enum
            ),*
        }

        impl LangItem{
            pub fn from_name(x: &str) -> Option<LangItem>{
                match x{
                    $(::core::stringify!($name) => Some(Self:: $enum),)*
                    _ => None
                }
            }

            #[allow(dead_code)]
            pub const fn name(&self) -> &'static str{
                match self{
                    $(Self:: $enum => ::core::stringify!($name)),*
                }
            }
        }
    }
}

define_lang_items! {
    Copy: copy,
    Clone: clone,
    IntoIterator: into_iterator,
    IteratorNext: iterator_next,
    Some: some,
    None: none,
    FnOnce: fn_once,
    FnMut: fn_mut,
    Fn: fn_trait,
    Main: main,
    Bool: bool,
    Unit: unit,
    Tuple: tuple,
    Slice: slice,
    Array: array,
    I8: i8,
    I16: i16,
    I32: i32,
    I64: i64,
    I128: i128,
    ISize: isize,
    U8:    u8,
    U16:   u16,
    U32:   u32,
    U64:   u64,
    U128:  u128,
    USize: usize,
    F32: f32,
    F64: f64,
    F32Rt: f32_rt,
    F64Rt: f64_rt,
    ConstPtr: const_ptr,
    MutPtr: mut_ptr,
    Char: char,
    Str: str,
    Never: never,
}
