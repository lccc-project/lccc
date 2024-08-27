use core::{
    ops::{Deref, DerefMut},
    str::Utf8Error,
};
use std::{
    borrow::{Borrow, BorrowMut},
    ffi::OsStr,
    fmt::{Formatter, Write},
    hash::{Hash, Hasher},
    iter::FromIterator,
    marker::PhantomData,
    ops::{Add, AddAssign},
    path::Path,
    ptr::NonNull,
};

use crate::{
    alloc::{Allocator, XLangAlloc},
    span::Span,
};

/// Error returned when converting a [`crate::vec::Vec`] into [`String`] if that vector does not contain valid Utf8
pub struct FromUtf8Error<A: Allocator> {
    err: Utf8Error,
    bytes: crate::vec::Vec<u8, A>,
}

impl<A: Allocator> FromUtf8Error<A> {
    /// Returns a reference to the bytes that were used in the conversion
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Returns the bytes that were used in the conversion
    pub fn into_bytes(self) -> crate::vec::Vec<u8, A> {
        self.bytes
    }

    /// Returns the Utf8 Error that occured when validating the value
    pub fn utf8_error(&self) -> &Utf8Error {
        &self.err
    }
}

impl<A: Allocator> Deref for FromUtf8Error<A> {
    type Target = Utf8Error;
    fn deref(&self) -> &Utf8Error {
        &self.err
    }
}

/// An ABI Safe [`std::string::String`]
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

impl<A: Allocator> AsRef<str> for String<A> {
    fn as_ref(&self) -> &str {
        self
    }
}

impl<A: Allocator> AsMut<str> for String<A> {
    fn as_mut(&mut self) -> &mut str {
        self
    }
}

impl<A: Allocator> Borrow<str> for String<A> {
    fn borrow(&self) -> &str {
        self
    }
}

impl<A: Allocator> BorrowMut<str> for String<A> {
    fn borrow_mut(&mut self) -> &mut str {
        self
    }
}

impl<A: Allocator> AsRef<[u8]> for String<A> {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<A: Allocator> AsRef<OsStr> for String<A> {
    fn as_ref(&self) -> &OsStr {
        <str as AsRef<OsStr>>::as_ref(self)
    }
}

impl<A: Allocator> AsRef<Path> for String<A> {
    fn as_ref(&self) -> &Path {
        <str as AsRef<Path>>::as_ref(self)
    }
}

impl String<XLangAlloc> {
    /// Returns a new (empty) string
    #[must_use]
    pub fn new() -> Self {
        Self(crate::vec::Vec::new())
    }
}

impl Default for String<XLangAlloc> {
    fn default() -> Self {
        Self::new()
    }
}

impl<A: Allocator> String<A> {
    /// Returns a new (empty) string using `alloc`
    pub fn new_in(alloc: A) -> Self {
        Self(crate::vec::Vec::new_in(alloc))
    }

    /// Appends the contents of `st` to self.
    pub fn push_str(&mut self, st: &str) {
        self.0.extend_from_slice(st.as_bytes());
    }

    /// Pushes a character
    pub fn push(&mut self, c: char) {
        self.push_str(c.encode_utf8(&mut [0u8; 4]));
    }

    /// Pops the last character from the string
    pub fn pop(&mut self) -> Option<char> {
        let mut b = self.0.pop()?;
        let mut val = 0;
        while b & 0xC0 == 0x80 {
            val |= (b & 0x3F) as u32;
            val <<= 6;

            b = self.0.pop().unwrap();
        }
        let bits = b.leading_ones();
        val |= (b & (!0 >> bits)) as u32;

        // SAFETY: the `String` is valid UTF-8 so we just decoded a valid `char`
        Some(unsafe { char::from_u32_unchecked(val) })
    }

    /// Converts a [`String`] into a [`crate::vec::Vec`] of UTF-8 bytes
    pub fn into_bytes(self) -> crate::vec::Vec<u8, A> {
        self.0
    }

    ///
    /// Converts a [`Vec`] of `u8` into a [`String`], validating that it contains Utf8
    ///
    /// # Errors
    /// Returns [`FromUtf8Error`] if `bytes` does not contain valid UTF-8
    pub fn from_utf8(bytes: crate::vec::Vec<u8, A>) -> Result<Self, FromUtf8Error<A>> {
        match core::str::from_utf8(&bytes).map(drop) {
            Ok(()) => Ok(Self(bytes)),
            Err(err) => Err(FromUtf8Error { err, bytes }),
        }
    }

    /// Converts a [`Vec`] of `u8` into a [`String`], without validating the contents of the vec
    ///
    /// # Safety
    /// Shall not be called with a [`Vec`] that contains invalid UTF-8 sequences
    pub unsafe fn from_utf8_unchecked(bytes: crate::vec::Vec<u8, A>) -> Self {
        Self(bytes)
    }

    /// Leaks the string and returns a mutable `str` slice with an unbound lifetime.
    ///
    /// This permanently leaks the allocation unless the backing memory of the allocator itself is reclaimed.
    pub fn leak<'a>(self) -> &'a mut str
    where
        A: 'a,
    {
        // SAFETY:
        // We are valid UTF-8 because of the safety-invariant of `String`
        unsafe { core::str::from_utf8_unchecked_mut(self.0.leak()) }
    }
}

impl<A: Allocator, S: AsRef<str>> Add<S> for String<A> {
    type Output = Self;

    fn add(mut self, rhs: S) -> Self::Output {
        self += rhs;
        self
    }
}

impl<A: Allocator, S: AsRef<str>> AddAssign<S> for String<A> {
    fn add_assign(&mut self, rhs: S) {
        self.push_str(rhs.as_ref());
    }
}

impl<A: Allocator> Hash for String<A> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher);
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

impl<A: Allocator, S: AsRef<str> + ?Sized> PartialEq<S> for String<A> {
    fn eq(&self, other: &S) -> bool {
        &**self == other.as_ref()
    }
}

impl<A: Allocator> Eq for String<A> {}

impl From<char> for String {
    fn from(value: char) -> Self {
        let mut utf8 = [0u8; 4];

        let st = value.encode_utf8(&mut utf8);

        let mut bytes = crate::vec::Vec::with_capacity(st.len());
        bytes.extend_from_slice(st.as_bytes());
        Self(bytes)
    }
}

impl<S: AsRef<str> + ?Sized> From<&S> for String {
    fn from(s: &S) -> Self {
        let s = s.as_ref();
        if s.is_empty() {
            Self::new()
        } else {
            let mut bytes = crate::vec::Vec::with_capacity(s.len());
            bytes.extend_from_slice(s.as_bytes());
            Self(bytes)
        }
    }
}

impl<'a> From<StringView<'a>> for String {
    fn from(s: StringView<'a>) -> Self {
        let s = &*s;
        if s.is_empty() {
            Self::new()
        } else {
            let mut bytes = crate::vec::Vec::with_capacity(s.len());
            bytes.extend_from_slice(s.as_bytes());
            Self(bytes)
        }
    }
}

impl From<std::string::String> for String {
    fn from(s: std::string::String) -> Self {
        let s = &*s;
        if s.is_empty() {
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

impl<A: Allocator> Extend<char> for String<A> {
    fn extend<T: IntoIterator<Item = char>>(&mut self, iter: T) {
        let iter = iter.into_iter();
        let (c_count, _) = iter.size_hint();
        self.0.reserve(c_count);

        for c in iter {
            self.push(c)
        }
    }
}

impl<'a, A: Allocator> Extend<&'a str> for String<A> {
    fn extend<T: IntoIterator<Item = &'a str>>(&mut self, iter: T) {
        for s in iter {
            self.push_str(s)
        }
    }
}

impl<I> FromIterator<I> for String
where
    String: Extend<I>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut st = String::new();
        st.extend(iter);
        st
    }
}

/// Version of [`std::format`] that returns a [`String`]
#[macro_export]
macro_rules! format{
    ($($tt:tt)*) => {
        {
            let mut val = $crate::string::String::new();
            ::core::fmt::write(&mut val, ::core::format_args!($($tt)*)).unwrap();
            val
        }
    }
}

///
/// An abi safe &str
#[repr(C)]
#[allow(clippy::module_name_repetitions)] // TODO: Determine if this should or should not be changed
#[derive(Clone, Copy)]
pub struct StringView<'a> {
    begin: NonNull<u8>,
    end: NonNull<u8>,
    phantom: PhantomData<&'a str>,
}

unsafe impl<'a> Send for StringView<'a> {}
unsafe impl<'a> Sync for StringView<'a> {}

impl<'a> From<&'a str> for StringView<'a> {
    fn from(v: &'a str) -> Self {
        Self::new(v)
    }
}

impl Deref for StringView<'_> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> AsRef<str> for StringView<'a> {
    fn as_ref(&self) -> &str {
        self
    }
}

impl<'a> AsRef<[u8]> for StringView<'a> {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<'a> AsRef<OsStr> for StringView<'a> {
    fn as_ref(&self) -> &OsStr {
        <str as AsRef<OsStr>>::as_ref(self)
    }
}

impl AsRef<Path> for StringView<'_> {
    fn as_ref(&self) -> &Path {
        <str as AsRef<Path>>::as_ref(self)
    }
}

impl Borrow<str> for StringView<'_> {
    fn borrow(&self) -> &str {
        self
    }
}

impl core::fmt::Debug for StringView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        <str as core::fmt::Debug>::fmt(self, f)
    }
}

impl core::fmt::Display for StringView<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self)
    }
}

impl core::hash::Hash for StringView<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        <str as core::hash::Hash>::hash(self, state);
    }
}

impl<S: AsRef<str> + ?Sized> PartialEq<S> for StringView<'_> {
    fn eq(&self, rhs: &S) -> bool {
        &**self == rhs.as_ref()
    }
}

impl<'a> PartialEq<StringView<'a>> for str {
    fn eq(&self, other: &StringView<'a>) -> bool {
        self == other.as_str()
    }
}

impl Eq for StringView<'_> {}

impl<'a> StringView<'a> {
    /// Returns an empty [`StringView`]
    #[must_use]
    pub const fn empty() -> Self {
        StringView {
            begin: NonNull::dangling(),
            end: NonNull::dangling(),
            phantom: PhantomData,
        }
    }

    /// Returns a view over the string referred to by `v`
    #[must_use]
    pub const fn new(v: &'a str) -> Self {
        let bytes = v.as_bytes();

        let begin = bytes.as_ptr();
        let end = unsafe { begin.add(bytes.len()) };

        unsafe { Self::from_raw_parts(begin, end) }
    }

    /// Obtains a [`StringView`] over the contiguous range `[begin,end)`
    ///
    /// ## Safety
    /// The behaviour is undefined if any of the following constraints are violated:
    /// * Neither begin nor end may be null pointers or deallocated pointers
    /// * For 'a, the range [begin,end) must be a range that is valid for reads
    /// * For 'a, the range [begin,end) must not be modified from any other
    /// * The text in the range [begin,end) must be valid UTF-8
    #[must_use]
    pub const unsafe fn from_raw_parts(begin: *const u8, end: *const u8) -> Self {
        Self {
            begin: NonNull::new_unchecked(begin as *mut u8),
            end: NonNull::new_unchecked(end as *mut u8),
            phantom: PhantomData,
        }
    }

    /// Obtains a raw pointer to the beginning of the string.
    ///
    /// ## Safety
    ///
    /// The returned `*const u8` is valid for the length of the string view up to the lifetime `'a`.
    /// When using the pointer you must not write through it
    #[must_use]
    pub const fn as_ptr(&self) -> *const u8 {
        self.begin.as_ptr()
    }

    /// Determines the length of the string view
    #[must_use]
    #[allow(clippy::cast_sign_loss)] // offset_from can never be negative
    pub const fn len(&self) -> usize {
        unsafe { self.end.as_ptr().offset_from(self.begin.as_ptr()) as usize }
    }

    /// Checks if this string view is empty
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    ///
    /// Converts an owned [`StringView`] into  &[`str`] with the same lifetime
    #[must_use]
    pub const fn into_str(self) -> &'a str {
        unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(
                self.as_ptr(),
                self.len(), // This is really annoying that have to do this
            ))
        }
    }

    ///
    /// Obtains a [`Span`] over the UTF-8 bytes of this [`StringView`] for the same lifetime
    #[must_use]
    #[allow(clippy::cast_sign_loss)] // allocation size is `[0,isize::MAX)` so this can never possibly overflow
    pub const fn into_byte_span(self) -> Span<'a, u8> {
        unsafe { Span::from_raw_parts(self.as_ptr(), self.len()) }
    }

    /// Borrows the string view as a [`Span`] over the UTF-8 Bytes of the [`StringView`].
    #[must_use]
    pub const fn as_byte_span(&self) -> Span<u8> {
        self.into_byte_span()
    }

    /// Obtains a reference to the string slice contained
    pub const fn as_str(&self) -> &str {
        self.into_str()
    }
}

#[doc(hidden)]
pub use str as __rust_str;

#[doc(hidden)]
pub use core::ptr::addr_of as __addr_of;

///
/// Constructs a [`StringView`] in a constant context.
/// This is equivalent to `StringView::new(str)`, except it is valid in a const initializer
///
/// must be called with a string literal or a constant expression of type `&'static str`
/// ## Examples
/// ```
///# use xlang_abi::string::StringView;
///# use xlang_abi::const_sv;
/// const HELLO_WORLD: StringView = const_sv!("Hello World");
/// assert_eq!(HELLO_WORLD,StringView::new("Hello World"));
/// ```
///
#[macro_export]
macro_rules! const_sv {
    ($str:expr) => {{
        $crate::string::StringView::new($str)
    }};
}

#[cfg(test)]
mod test {
    use crate::string::StringView;

    #[test]
    pub fn test_new() {
        let x = StringView::new("Foo");
        assert_eq!(&*x, "Foo");
    }

    #[test]
    pub fn test_new_empty() {
        let y = StringView::new("");
        assert_eq!(&*y, "");
    }

    #[test]

    pub fn test_empty() {
        let y = StringView::empty();
        assert_eq!(&*y, "");
    }

    #[test]
    pub fn test_const_sv() {
        const FOO: StringView = crate::const_sv!("Foo");
        assert_eq!(FOO.len(), 3);
        assert_eq!(&*FOO, "Foo");
    }

    #[test]
    pub fn test_const_sv_empty() {
        const FOO: StringView = crate::const_sv!("");
        assert_eq!(&*FOO, "");
    }

    #[test]
    pub fn test_eq() {
        let y = StringView::new("");
        assert_eq!(y, y);
    }

    #[test]
    pub fn test_display() {
        let x = StringView::new("Hello World");
        assert_eq!(*x, *std::format!("{}", x))
    }

    #[test]
    fn test_format() {
        assert_eq!(*"5", *format!("{}", 5))
    }
}
