use std::{
    borrow::{Borrow, BorrowMut},
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Deref, DerefMut, RangeBounds},
    ptr::NonNull,
};

#[repr(C)]
pub struct Span<'a, T> {
    begin: NonNull<T>,
    len: usize,
    phantom: PhantomData<&'a [T]>,
}

impl<T> Clone for Span<'_, T> {
    fn clone(&self) -> Self {
        Self { ..*self }
    }
}

impl<T> Copy for Span<'_, T> {}

impl<'a, T> Span<'a, T> {
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            begin: NonNull::dangling(),
            len: 0,
            phantom: PhantomData,
        }
    }

    #[must_use]
    pub const fn new(x: &'a [T]) -> Self {
        Self {
            begin: unsafe { NonNull::new_unchecked(x.as_ptr() as *mut T) },
            len: x.len(),
            phantom: PhantomData,
        }
    }

    ///
    /// Reborrows the span immutably.
    /// This is the same as copying the Span and shrinking the lifetime.
    #[must_use]
    pub const fn reborrow(&self) -> Span<'_, T> {
        *self
    }

    ///
    /// Returns a subspan of the span, using a range as the index.
    /// Note: This function only accepts ranges. To slice a particular element, use [`slice::get`] or [`std::ops::Index`]
    pub fn subspan<I: RangeBounds<usize>>(&self, idx: I) -> Option<Span<T>> {
        let begin = match idx.start_bound() {
            std::ops::Bound::Included(x) => *x,
            std::ops::Bound::Excluded(x) => x.saturating_add(1),
            std::ops::Bound::Unbounded => 0,
        };
        let end = match idx.end_bound() {
            std::ops::Bound::Included(x) => x.saturating_add(1),
            std::ops::Bound::Excluded(x) => *x,
            std::ops::Bound::Unbounded => self.len,
        };
        let len = if end < begin {
            0
        } else {
            end.saturating_sub(begin)
        };
        if (end.saturating_sub(1)) > self.len {
            None
        } else {
            Some(Span {
                begin: unsafe { NonNull::new_unchecked(self.begin.as_ptr().add(begin)) },
                len,
                phantom: PhantomData,
            })
        }
    }

    pub fn into_slice(self) -> &'a [T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }
}

impl<'a, T> IntoIterator for Span<'a, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        <&'a [T]>::into_iter(self.into_slice())
    }
}

unsafe impl<T: Sync> Send for Span<'_, T> {}
unsafe impl<T: Sync> Sync for Span<'_, T> {}

impl<T> Deref for Span<'_, T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }
}

impl<T> AsRef<[T]> for Span<'_, T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T> Borrow<[T]> for Span<'_, T> {
    fn borrow(&self) -> &[T] {
        self
    }
}

impl<'a, T> From<&'a [T]> for Span<'a, T> {
    fn from(x: &'a [T]) -> Span<'a, T> {
        Self {
            begin: unsafe { NonNull::new_unchecked(x.as_ptr() as *mut T) },
            len: x.len(),
            phantom: PhantomData,
        }
    }
}

impl<'a, T> Debug for Span<'a, T>
where
    [T]: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[T] as Debug>::fmt(self, f)
    }
}

impl<T> Hash for Span<'_, T>
where
    [T]: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        <[T] as Hash>::hash(self, state)
    }
}

impl<T, U> PartialEq<Span<'_, U>> for Span<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &Span<U>) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<SpanMut<'_, U>> for Span<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &SpanMut<U>) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<[U]> for Span<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &[U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<&[U]> for Span<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &&[U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<&mut [U]> for Span<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &&mut [U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T> Eq for Span<'_, T> where [T]: Eq {}

#[allow(clippy::module_name_repetitions)]
#[repr(C)]
pub struct SpanMut<'a, T> {
    begin: NonNull<T>,
    len: usize,
    phantom: PhantomData<&'a mut [T]>,
}

unsafe impl<T: Send> Send for SpanMut<'_, T> {}
unsafe impl<T: Sync> Sync for SpanMut<'_, T> {}

impl<'a, T> From<&'a mut [T]> for SpanMut<'a, T> {
    fn from(x: &'a mut [T]) -> Self {
        Self {
            len: x.len(),
            begin: unsafe { NonNull::new_unchecked(x.as_mut_ptr()) },
            phantom: PhantomData,
        }
    }
}

impl<'a, T> From<SpanMut<'a, T>> for Span<'a, T> {
    fn from(x: SpanMut<'a, T>) -> Self {
        Self {
            begin: x.begin,
            len: x.len,
            phantom: PhantomData,
        }
    }
}

impl<T> Deref for SpanMut<'_, T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }
}

impl<T> DerefMut for SpanMut<'_, T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.begin.as_ptr(), self.len) }
    }
}

impl<T> AsRef<[T]> for SpanMut<'_, T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T> AsMut<[T]> for SpanMut<'_, T> {
    fn as_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T> Borrow<[T]> for SpanMut<'_, T> {
    fn borrow(&self) -> &[T] {
        self
    }
}

impl<T> BorrowMut<[T]> for SpanMut<'_, T> {
    fn borrow_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T> Hash for SpanMut<'_, T>
where
    [T]: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        <[T] as Hash>::hash(self, state)
    }
}

impl<T, U> PartialEq<Span<'_, U>> for SpanMut<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &Span<U>) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<SpanMut<'_, U>> for SpanMut<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &SpanMut<U>) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<[U]> for SpanMut<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &[U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<&[U]> for SpanMut<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &&[U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T, U> PartialEq<&mut [U]> for SpanMut<'_, T>
where
    [T]: PartialEq<[U]>,
{
    fn eq(&self, other: &&mut [U]) -> bool {
        <[T] as PartialEq<[U]>>::eq(self, other)
    }
}

impl<T> Eq for SpanMut<'_, T> where [T]: Eq {}

impl<'a, T> IntoIterator for SpanMut<'a, T> {
    type Item = &'a mut T;
    type IntoIter = core::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_slice_mut().into_iter()
    }
}

trait Hack {
    const PHANTOM: Self;
}

impl<T: ?Sized> Hack for PhantomData<T> {
    const PHANTOM: Self = PhantomData;
}

impl<'a, T> SpanMut<'a, T> {
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            begin: NonNull::dangling(),
            len: 0,
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }

    #[must_use]
    pub fn new(x: &'a mut [T]) -> Self {
        Self {
            len: x.len(),
            begin: unsafe { NonNull::new_unchecked(x.as_mut_ptr()) },
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }

    #[must_use]
    pub const fn reborrow(&self) -> Span<'_, T> {
        Span {
            begin: self.begin,
            len: self.len,
            phantom: PhantomData,
        }
    }

    pub fn reborrow_mut(&mut self) -> SpanMut<'_, T> {
        SpanMut {
            begin: self.begin,
            len: self.len,
            phantom: PhantomData,
        }
    }

    ///
    /// Returns a subspan of the span, using a range as the index.
    /// Note: This function only accepts ranges. To slice a particular element, use [`slice::get`] or [`std::ops::Index`]
    pub fn subspan<I: RangeBounds<usize>>(&self, idx: I) -> Option<Span<T>> {
        let begin = match idx.start_bound() {
            std::ops::Bound::Included(x) => *x,
            std::ops::Bound::Excluded(x) => x.saturating_add(1),
            std::ops::Bound::Unbounded => 0,
        };
        let end = match idx.end_bound() {
            std::ops::Bound::Included(x) => x.saturating_add(1),
            std::ops::Bound::Excluded(x) => *x,
            std::ops::Bound::Unbounded => self.len,
        };
        let len = if end < begin {
            0
        } else {
            end.saturating_sub(begin)
        };
        if len > self.len || begin > self.len {
            None
        } else {
            Some(Span {
                begin: unsafe { NonNull::new_unchecked(self.begin.as_ptr().add(begin)) },
                len,
                phantom: PhantomData,
            })
        }
    }

    ///
    /// Returns a subspan of the span, using a range as the index.
    /// Note: This function only accepts ranges. To slice a particular element, use [`slice::get`] or [`std::ops::Index`]
    pub fn subspan_mut<I: RangeBounds<usize>>(&mut self, idx: I) -> Option<SpanMut<T>> {
        let begin = match idx.start_bound() {
            std::ops::Bound::Included(x) => *x,
            std::ops::Bound::Excluded(x) => x.saturating_add(1),
            std::ops::Bound::Unbounded => 0,
        };
        let end = match idx.end_bound() {
            std::ops::Bound::Included(x) => x.saturating_add(1),
            std::ops::Bound::Excluded(x) => *x,
            std::ops::Bound::Unbounded => self.len,
        };
        let len = if end < begin {
            0
        } else {
            end.saturating_sub(begin)
        };
        if (end.saturating_sub(1)) > self.len {
            None
        } else {
            Some(SpanMut {
                begin: unsafe { NonNull::new_unchecked(self.begin.as_ptr().add(begin)) },
                len,
                phantom: PhantomData,
            })
        }
    }

    pub fn into_slice(self) -> &'a [T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }

    pub fn into_slice_mut(self) -> &'a mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.begin.as_ptr(), self.len) }
    }
}

impl<'a, T> Debug for SpanMut<'a, T>
where
    [T]: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[T] as Debug>::fmt(self, f)
    }
}

#[macro_export]
macro_rules! span{
    [$($expr:expr),* $(,)?] => {
        $crate::span::Span::new(&[$($expr),*])
    };
    [$expr:expr ; $repeat:expr] => {
        $crate::span::Span::new(&[$expr;$repeat])
    }
}
