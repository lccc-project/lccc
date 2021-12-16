use core::{
    ops::{Deref, DerefMut},
    str::Utf8Error,
};
use std::{
    fmt::{Formatter, Write},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ptr::NonNull,
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
    pub fn new_in(alloc: A) -> Self {
        Self(crate::vec::Vec::new_in(alloc))
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

impl<A: Allocator, A1: Allocator> PartialEq<String<A1>> for String<A> {
    fn eq(&self, other: &String<A1>) -> bool {
        self.0 == other.0
    }
}

impl<A: Allocator> Eq for String<A> {}

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

impl<A: Allocator> Write for String<A> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.extend_from_slice(s.as_bytes());
        Ok(())
    }
}

///
/// An abi safe &str
#[repr(C)]
#[allow(clippy::module_name_repetitions)] // TODO: Determine if this should or should not be changed
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
        unsafe {
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(
                self.begin.as_ptr(),
                (self.end.as_ptr() as usize) - (self.begin.as_ptr() as usize), // This is really annoying that have to do this
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
        <str as core::hash::Hash>::hash(self, state);
    }
}

impl PartialEq for StringView<'_> {
    fn eq(&self, other: &Self) -> bool {
        <str as PartialEq>::eq(self, other)
    }
}

impl Eq for StringView<'_> {}

impl<'a> StringView<'a> {
    #[must_use]
    pub const fn empty() -> Self {
        StringView {
            begin: NonNull::dangling(),
            end: NonNull::dangling(),
            phantom: PhantomData,
        }
    }

    #[must_use]
    pub fn new(v: &'a str) -> Self {
        let ptr = v.as_bytes().as_ptr();
        Self {
            begin: unsafe { NonNull::new_unchecked(ptr as *mut u8) },
            end: unsafe { NonNull::new_unchecked(ptr.add(v.len()) as *mut u8) },
            phantom: PhantomData,
        }
    }

    ///
    /// ## SAFETY
    /// The behaviour is undefined if any of the following constraints are violated:
    /// * Neither begin nor end may be null pointers or deallocated pointers
    /// * For 'a, the range [begin,end) must be a valid range
    /// * The text in the range [begin,end) must be valid UTF-8
    #[must_use]
    pub const unsafe fn from_raw_parts(begin: *const u8, end: *const u8) -> Self {
        Self {
            begin: NonNull::new_unchecked(begin as *mut u8),
            end: NonNull::new_unchecked(end as *mut u8),
            phantom: PhantomData,
        }
    }
}

#[doc(hidden)]
pub use str as __rust_str;

///
/// Constructs a `StringView` in a constant context.
/// This is equivalent to `StringView::new(str)`, except it is valid in a const initializer
/// ## Examples
/// ```
///# use xlang_abi::string::StringView;
///# use xlang_abi::const_sv;
/// const HELLO_WORLD: StringView = const_sv!("Hello World");
/// assert_eq!(HELLO_WORLD,StringView::new("Hello World"));
/// ```
///
#[macro_export]
macro_rules! const_sv{
    ($str:expr) => {
        {
            const __RET: $crate::string::StringView = {
                // Thanks to matt1992 on the Rust Community Discord Server for this Rust 1.39 Friendly Hack
                // Why is
                const __STR: &$crate::string::__rust_str = $str;
                /// # Safety
                ///
                /// `Self::Arr`'s type must be `[Self::Elem; Self::SLICE.len()]`
                ///
                unsafe trait GetSlice {
                    type Elem: 'static;
                    type Arr;
                    const SLICE: &'static [Self::Elem];
                }

                #[repr(C)]
                struct ArrayAndEmpty<Arr> {
                    array: Arr,
                    empty: [u8; 0],
                }

                #[repr(C)]
                union AsPacked<'a, Arr, T> {
                    start: *const T,
                    packed: &'a ArrayAndEmpty<Arr>,
                }

                struct GetEnd<T>(::core::marker::PhantomData<T>);

                // Non-Empty slice case
                impl<T: GetSlice> GetEnd<T> {
                    pub const AS_END: *const T::Elem = unsafe {
                        use ::std::mem::size_of;
                        let same_size = size_of::<ArrayAndEmpty<T::Arr>>() != size_of::<T::Arr>();
                        [(/* no padding allowed */)][same_size as usize];
                        [(/* slice must be non-empty */)][T::SLICE.is_empty() as usize];

                        AsPacked::<T::Arr, T::Elem> {
                            start: T::SLICE.as_ptr(),
                        }
                        .packed
                        .empty
                        .as_ptr() as *const T::Elem
                    };
                }

                const __SLICE: &[u8] = (__STR).as_bytes();


                struct __Dummy;

                unsafe impl GetSlice for __Dummy {
                    type Elem = u8;
                    type Arr = [u8; __SLICE.len()];
                    const SLICE: &'static [u8] = __SLICE;
                }

                let (begin, end) = (__SLICE.as_ptr(), GetEnd::<__Dummy>::AS_END);
                // SAFETY:
                //
                unsafe{$crate::string::StringView::from_raw_parts(begin,end)}
            };
            __RET
        }
    }
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
    pub fn test_eq() {
        let y = StringView::new("");
        assert_eq!(y, y);
    }

    #[test]
    pub fn test_display() {
        let x = StringView::new("Hello World");
        assert_eq!(&*x, &*format!("{}", x))
    }
}
