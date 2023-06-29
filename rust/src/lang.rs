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
            pub const fn from_name(x: &str) -> Option<LangItem>{
                match x{
                    $(::core::stringify!($name) => Some(Self:: $enum),)*
                    _ => None
                }
            }

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

}
