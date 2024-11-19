//! Helper traits and types for [`TargetProperties::extended_property`][crate::properties::TargetProperties::extended_property]

use std::{convert::Infallible, str::ParseBoolError};

use xlang_abi::{
    alloc::Allocator,
    string::{CowStr, StringView},
};

/// Trait for types that can be obtained by parsing a property string.
/// This is similar to the [`FromStr`][core::str::FromStr] trait, but takes a lifetime parameter, so that the result can borrow from the input (this allows parsing into a `&str` or a `StringView`)
pub trait FromPropertyString<'a>: Sized {
    /// The error returned from parsing the string
    type Err: core::fmt::Debug;

    /// Parse a [`StringView`] into the type, returning an error if the parse cannot be completed.
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err>;
}

impl<'a, 'b> FromPropertyString<'a> for &'b str
where
    'a: 'b,
{
    type Err = Infallible;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        Ok(st.into_str())
    }
}

impl<'a, 'b> FromPropertyString<'a> for StringView<'b>
where
    'a: 'b,
{
    type Err = Infallible;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        Ok(st)
    }
}

impl<'a, 'b, A: Allocator> FromPropertyString<'a> for CowStr<'b, A>
where
    'a: 'b,
{
    type Err = Infallible;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        Ok(CowStr::borrowed(st))
    }
}

impl<'a> FromPropertyString<'a> for std::string::String {
    type Err = Infallible;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        Ok(Self::from(&*st))
    }
}

impl<'a> FromPropertyString<'a> for xlang_abi::string::String {
    type Err = Infallible;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        Ok(Self::from(st))
    }
}

impl<'a> FromPropertyString<'a> for bool {
    type Err = ParseBoolError;
    fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
        st.parse()
    }
}

macro_rules! impl_for_int_ty {
    ($($int_ty:ty),* $(,)?) => {
        $(
            impl<'a> FromPropertyString<'a> for $int_ty{
                type Err = ::core::num::ParseIntError;
                fn from_property(st: StringView<'a>) -> Result<Self, Self::Err> {
                    st.parse()
                }
            }
        )*
    };
}

impl_for_int_ty!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128);
