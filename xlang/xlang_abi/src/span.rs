use std::{
    borrow::{Borrow, BorrowMut},
    cell::UnsafeCell,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut, RangeBounds},
    ptr::NonNull,
};

/// An ABI Safe reference to a dynamically sized contiguous region of values
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
    /// Returns a Span with length of `0`
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            begin: NonNull::dangling(),
            len: 0,
            phantom: PhantomData,
        }
    }

    /// Returns a Span over the elements of `x`
    #[must_use]
    pub const fn new(x: &'a [T]) -> Self {
        Self {
            begin: unsafe { NonNull::new_unchecked(x.as_ptr() as *mut T) },
            len: x.len(),
            phantom: PhantomData,
        }
    }

    /// Returns a span over the elements between `[begin,begin+len)`
    ///
    /// # Safety
    /// * The range `[begin,begin+len)` must be valid and dereferenceable for `'a`.
    /// * `begin` must not be null
    /// * The range `[begin,begin+len)` must not be written to for the duration of 'a, except inside an `UnsafeCell`
    #[must_use]
    #[deny(unsafe_op_in_unsafe_fn)]
    pub const unsafe fn from_raw_parts(begin: *const T, len: usize) -> Self {
        Self {
            begin: unsafe { NonNull::new_unchecked(begin as *mut T) },
            len,
            phantom: PhantomData,
        }
    }

    /// Returns a span over a single element, `x`
    #[must_use]
    pub const fn from_ref(x: &'a T) -> Self {
        Self {
            begin: unsafe { NonNull::new_unchecked(x as *const T as *mut T) },
            len: 1,
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

    /// Converts self into a slice of its elements with the same lifetime
    #[must_use]
    pub fn into_slice(self) -> &'a [T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }
}

impl<'a, T> Span<'a, MaybeUninit<T>> {
    ///
    /// Converts from a [`Span<MaybeUninit<T>>`] to a [`Span<T>`], assuming each element is initialized
    ///
    /// ## Safety
    /// Each element of the span must be valid for `T`, and satisfy any additional invariants `T` imposes.
    #[must_use]
    pub const unsafe fn assume_init_span(self) -> Span<'a, T> {
        Span {
            begin: self.begin.cast(),
            len: self.len,
            phantom: PhantomData,
        }
    }
}

impl<'a, T> IntoIterator for Span<'a, T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        <&'a [T]>::into_iter(self.into_slice())
    }
}

impl<'b, 'a, T> IntoIterator for &'b Span<'a, T> {
    type Item = &'b T;
    type IntoIter = core::slice::Iter<'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
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
        <[T] as Hash>::hash(self, state);
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

/// An abi safe mutable span to a contiguous, dynamically sized, region of values
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
        <[T] as Hash>::hash(self, state);
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
        self.into_slice_mut().iter_mut()
    }
}

impl<'b, 'a, T> IntoIterator for &'b mut SpanMut<'a, T> {
    type Item = &'b mut T;
    type IntoIter = core::slice::IterMut<'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'b, 'a, T> IntoIterator for &'b SpanMut<'a, T> {
    type Item = &'b T;
    type IntoIter = core::slice::Iter<'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

trait Hack {
    const PHANTOM: Self;
}

impl<T: ?Sized> Hack for PhantomData<T> {
    const PHANTOM: Self = PhantomData;
}

impl<'a, T> SpanMut<'a, T> {
    /// Returns a mutable span over an empty range
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            begin: NonNull::dangling(),
            len: 0,
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }

    /// Returns a span over the elements of `x`
    #[must_use]
    pub fn new(x: &'a mut [T]) -> Self {
        Self {
            len: x.len(),
            begin: unsafe { NonNull::new_unchecked(x.as_mut_ptr()) },
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }

    /// Reborws self into an immutable span for a shorter lifetime
    #[must_use]
    pub const fn reborrow(&self) -> Span<'_, T> {
        Span {
            begin: self.begin,
            len: self.len,
            phantom: PhantomData,
        }
    }

    /// Returns a span over a single element, `x`
    #[must_use]
    pub fn from_mut(x: &'a mut T) -> Self {
        Self {
            begin: NonNull::from(x),
            len: 1,
            phantom: PhantomData,
        }
    }

    /// Returns a span over the elements between `[begin,begin+len)`
    ///
    /// # Safety
    /// * The range `[begin,begin+len)` must be valid and dereferenceable for `'a`.
    /// * `begin` must not be null
    /// * The range `[begin,begin+len)` must not be aliased by any other pointer to for the duration of 'a, except inside an `UnsafeCell`
    #[must_use]
    #[deny(unsafe_op_in_unsafe_fn)]
    pub const unsafe fn from_raw_parts(begin: *mut T, len: usize) -> Self {
        Self {
            begin: unsafe { NonNull::new_unchecked(begin) },
            len,
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }

    /// Reborrows self into a mutable span for a shorter lifetime
    #[must_use]
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

    ///
    /// Converts self into an (immutable) slice over its region
    #[must_use]
    pub fn into_slice(self) -> &'a [T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len) }
    }

    /// Converts self into a (mutable) slice over its region
    #[must_use]
    pub fn into_slice_mut(self) -> &'a mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.begin.as_ptr(), self.len) }
    }

    /// Downgrades `self` into an immutable span over the same region. Note that this consumes `self`.
    /// If an immutable region is needed but self should not be consumed, use [`SpanMut::reborrow`] instead.
    #[must_use]
    pub const fn into_span(self) -> Span<'a, T> {
        Span {
            begin: self.begin,
            len: self.len,
            phantom: PhantomData,
        }
    }
}

impl<'a, T> SpanMut<'a, MaybeUninit<T>> {
    ///
    /// Converts from a [`SpanMut<MaybeUninit<T>>`] to a [`SpanMut<T>`], assuming each element is initialized.
    /// This consumes the span. If this is undesirable, use [`SpanMut::reborrow_mut`] first
    ///
    /// ## Safety
    /// Each element of the span must be valid for `T`, and satisfy any additional invariants `T` imposes.
    #[must_use]
    pub const unsafe fn assume_init_span_mut(self) -> SpanMut<'a, T> {
        SpanMut {
            begin: self.begin.cast(),
            len: self.len,
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
    }
}

impl<'a, T> SpanMut<'a, UnsafeCell<T>> {
    /// Converts a [`SpanMut`] of [`UnsafeCell`] into a [`SpanMut`] of `T`.
    ///
    /// This is safe because holding a mutable span to a range precludes all other access.
    #[must_use]
    pub const fn span_mut(self) -> SpanMut<'a, T> {
        SpanMut {
            begin: self.begin.cast(),
            len: self.len,
            phantom: <PhantomData<&'a mut [T]> as Hack>::PHANTOM,
        }
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

/// Produces an immutable span over a static (promoted) array of constant exressions
#[macro_export]
macro_rules! span{
    [$($expr:expr),* $(,)?] => {
        $crate::span::Span::<'static,_>::new(&[$($expr),*])
    };
    [$expr:expr ; $repeat:expr] => {
        $crate::span::Span::<'static,_>::new(&[$expr;$repeat])
    }
}

#[cfg(test)]
mod test {
    use super::{Span, SpanMut};
    use crate::span;
    #[test]
    fn test_span_empty() {
        let s = Span::<u8>::empty();
        assert_eq!(s.len(), 0);
    }

    #[test]
    fn test_span_mut_empty() {
        let s = SpanMut::<u8>::empty();
        assert_eq!(s.len(), 0);
    }

    #[test]
    fn test_span_macro_empty() {
        let s: Span<u8> = span![];
        assert_eq!(s.len(), 0);
    }
}
