use core::{
    ops::{Deref, DerefMut},
    str::Utf8Error,
};
use std::{
    fmt::{Formatter, Write},
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use crate::alloc::{Allocator, XLangAlloc};

pub struct FromUtf8Error<A: Allocator> {
    err: Utf8Error,
    bytes: crate::vec::Vec<u8, A>,
}

impl<A: Allocator> FromUtf8Error<A> {
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn into_bytes(self) -> crate::vec::Vec<u8, A> {
        self.bytes
    }
}

impl<A: Allocator> Deref for FromUtf8Error<A> {
    type Target = Utf8Error;
    fn deref(&self) -> &Utf8Error {
        &self.err
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct String<A: Allocator = XLangAlloc>(crate::vec::Vec<u8, A>);

impl<A: Allocator> Deref for String<A> {
    type Target = str;
    fn deref(&self) -> &str {
        unsafe { core::str::from_utf8_unchecked(&self.0) }
    }
}

impl<A: Allocator> DerefMut for String<A> {
    fn deref_mut(&mut self) -> &mut str {
        unsafe { core::str::from_utf8_unchecked_mut(&mut self.0) }
    }
}

impl String<XLangAlloc> {
    pub fn new() -> Self {
        Self(crate::vec::Vec::new())
    }
}

impl<A: Allocator> String<A> {
    pub fn new_in(alloc: A) -> Self {
        Self(crate::vec::Vec::new_in(alloc))
    }
}

impl<A: Allocator> Hash for String<A> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher)
    }
}

impl<A: Allocator> core::fmt::Debug for String<A> {
    fn fmt(&self, fmt: &mut Formatter) -> core::fmt::Result {
        <str as core::fmt::Debug>::fmt(self, fmt)
    }
}

impl<A: Allocator> core::fmt::Display for String<A> {
    fn fmt(&self, fmt: &mut Formatter) -> core::fmt::Result {
        <str as core::fmt::Display>::fmt(self, fmt)
    }
}

impl<A: Allocator, A1: Allocator> PartialEq<String<A1>> for String<A> {
    fn eq(&self, other: &String<A1>) -> bool {
        self.0 == other.0
    }
}

impl<A: Allocator> Eq for String<A> {}

impl From<&str> for String {
    fn from(s: &str) -> Self {
        if s.len() == 0 {
            Self::new()
        } else {
            let mut bytes = crate::vec::Vec::with_capacity(s.len());
            bytes.extend_from_slice(s.as_bytes());
            Self(bytes)
        }
    }
}

impl<A: Allocator> Write for String<A> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.extend_from_slice(s.as_bytes());
        Ok(())
    }
}

///
/// An abi safe &str
#[repr(C)]
pub struct StringView<'a> {
    begin: *const u8,
    end: *const u8,
    inner: PhantomData<&'a str>,
}

unsafe impl<'a> Send for StringView<'a> {}
unsafe impl<'a> Sync for StringView<'a> {}

impl<'a> From<&'a str> for StringView<'a> {
    fn from(v: &'a str) -> Self {
        let begin = v.as_ptr();
        let end = unsafe { begin.add(v.len()) };
        Self {
            begin,
            end,
            inner: PhantomData,
        }
    }
}

impl Deref for StringView<'_> {
    type Target = str;

    fn deref(&self) -> &str {
        unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(
                self.begin,
                (self.end as usize) - (self.begin as usize), // This is really annoying t have to do
            ))
        }
    }
}

impl core::fmt::Debug for StringView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        <str as core::fmt::Debug>::fmt(self, f)
    }
}

impl core::fmt::Display for StringView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        <str as core::fmt::Display>::fmt(self, f)
    }
}

impl core::hash::Hash for StringView<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <str as core::hash::Hash>::hash(self, state)
    }
}

impl PartialEq for StringView<'_> {
    fn eq(&self, other: &Self) -> bool {
        <str as PartialEq>::eq(self, other)
    }
}

impl Eq for StringView<'_> {}
