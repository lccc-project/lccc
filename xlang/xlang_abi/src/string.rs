use core::{
    ops::{Deref, DerefMut},
    str::Utf8Error,
};
use std::{
    fmt::Formatter,
    hash::{Hash, Hasher},
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
