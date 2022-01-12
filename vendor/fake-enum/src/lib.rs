#![cfg_attr(not(test), no_std)]

//!
//! Support for fake enum types, that act like rust enums, but all values of the underlying type
//!  are accepted as values.
//! See the macro [`fake_enum`] for details.

///
/// Constructs a "Fake Enum", that acts like a rust enum with unit variants,
///  but can accept invalid (undefined) variants without undefined behaviour.
/// The enum derives Copy, Clone, Eq, and PartialEq. Additionally, it implements Debug, where all valid variants are printed as defined,
///  and invalid variants are formatted as name(value).
/// Any other derives can be added following the repr.
/// Two forms of this macro is provided. `enum name` declares an enum named "name". All the variants are declared with the same visibility as the type in the enclosing module.
/// `enum struct name` declares an scoped enum named "name". The variants are declared `pub` within "name".
///
/// In Both cases, it is valid to transmute the declared type to and from the repr type (note that no from implementation is provided)
///
/// ```rust
/// use fake_enum::fake_enum;
/// fake_enum!{
///    #[repr(u8)] pub enum Foo{
///        Bar = 0,
///        Baz = 1,
///    }  
/// };
/// let x = Bar;
/// assert_eq!(format!("{:?}",x),"Bar");
/// assert_eq!(unsafe{std::mem::transmute::<_,Foo>(1u8)},Baz)
/// ```
///
/// The underlying type may be given

#[macro_export]
macro_rules! fake_enum{
    {#[repr($tvis:vis $t:ty)] $(#[$meta:meta])* $vis:vis enum $name:ident {
        $(#![$meta1:meta])*
        $($(#[$r:meta])* $item:ident = $expr:literal),*$(,)?
    }} => {

        #[derive(Copy,Clone,Eq,PartialEq)]
        #[repr(transparent)]
        $(#[$meta])*
        $(#[$meta1])*
        $vis struct $name($tvis $t);

        $(#[allow(non_upper_case_globals)] #[allow(dead_code)] $(#[$r])* $vis const $item: $name = $name($expr as $t);)*

        impl ::core::fmt::Debug for $name{
            #[allow(unreachable_patterns)]
            fn fmt(&self,f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result{
                match self{
                    $(Self($expr) => f.write_str(::core::stringify!($item)),)*
                    e => f.write_fmt(::core::format_args!("{}({})",::core::stringify!($name),e.0))
                }
            }
        }
    };
    {#[repr($tvis:vis $t:ty)] $(#[$meta:meta])* $vis:vis enum struct $name:ident {
        $(#![$meta1:meta])*
        $($(#[$r:meta])* $item:ident = $expr:literal),*$(,)?
    }} => {
        #[derive(Copy,Clone,Eq,PartialEq)]
        #[repr(transparent)]
        $(#[$meta])*
        $(#[$meta1])*
        $vis struct $name($tvis $t);
        impl $name{
            $(#[allow(non_upper_case_globals)] #[allow(dead_code)] $(#[$r])* pub const $item: $name = $name($expr as $t);)*
        }
        impl ::core::fmt::Debug for $name{
            #[allow(unreachable_patterns)]
            fn fmt(&self,f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result{
                match self{
                    $(Self($expr) => f.write_str(::core::stringify!($item)),)*
                    e => f.write_fmt(::core::format_args!("{}({})",::core::stringify!($name),e.0))
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    fake_enum! {
        #[repr(u16)] pub enum ElfType{
            //! The type of an elf file

            /// No Elf Type/Invalid Elf File
            ET_NONE = 0,
            /// Relocatable file
            ET_REL = 1,
            /// Executable file
            ET_EXEC = 2,
            /// Dynamic Library/Shared Object
            ET_DYN = 3,
            /// Core Dump
            ET_CORE = 4
        }
    }

    #[test]
    pub fn fake_enum_elf_type_name() {
        assert_eq!(format!("{:?}", ET_NONE), "ET_NONE");
        assert_eq!(format!("{:?}", ET_REL), "ET_REL");
        assert_eq!(format!("{:?}", ET_EXEC), "ET_EXEC");
        assert_eq!(format!("{:?}", ET_DYN), "ET_DYN");
        assert_eq!(format!("{:?}", ET_CORE), "ET_CORE");
    }

    #[test]
    pub fn fake_enum_partial_eq_impl() {
        assert_eq!(ET_NONE, ET_NONE);
        assert_ne!(ET_NONE, ET_REL);
        assert_ne!(ET_NONE, ET_EXEC);
        assert_ne!(ET_NONE, ET_DYN);
        assert_ne!(ET_NONE, ET_CORE);
        assert_eq!(ET_REL, ET_REL);
        assert_ne!(ET_REL, ET_EXEC);
        assert_ne!(ET_REL, ET_DYN);
        assert_ne!(ET_REL, ET_CORE);
        assert_eq!(ET_EXEC, ET_EXEC);
        assert_ne!(ET_EXEC, ET_DYN);
        assert_ne!(ET_EXEC, ET_CORE);
        assert_eq!(ET_DYN, ET_DYN);
        assert_ne!(ET_DYN, ET_CORE);
        assert_eq!(ET_CORE, ET_CORE);
    }

    #[test]
    pub fn fake_enum_transmute_test() {
        assert_eq!(unsafe { std::mem::transmute::<u16, ElfType>(0) }, ET_NONE);
        assert_eq!(unsafe { std::mem::transmute::<u16, ElfType>(1) }, ET_REL);
        assert_eq!(unsafe { std::mem::transmute::<u16, ElfType>(2) }, ET_EXEC);
        assert_eq!(unsafe { std::mem::transmute::<u16, ElfType>(3) }, ET_DYN);
        assert_eq!(unsafe { std::mem::transmute::<u16, ElfType>(4) }, ET_CORE);
    }

    fake_enum! {
        #[repr(u8)]
        #[derive(Hash,Default)]
        pub enum struct NbtTagType{
            //! The type of an Nbt Tag

            /// An End Tag
            End = 0,
            /// A byte
            Byte = 1,
            /// A Short
            Short = 2,
            /// An Int
            Int = 3,
            /// A Long
            Long = 4,
            /// A Float
            Float = 5,
            Double = 6,
            ByteArray = 7,
            String = 8,
            List = 9,
            Compound = 10,
            IntArray = 11,
            LongArray = 12,
            FloatArray = 13,
            DoubleArray = 14,
            Uuid = 15
        }
    }

    fake_enum! {
        #[repr(pub u8)]
        pub enum struct Test{
            Foo = 0
        }
    }

    #[test]
    fn pub_repr_test() {
        let foo = Test(0);
        assert_eq!(foo, Test::Foo);
    }
}
