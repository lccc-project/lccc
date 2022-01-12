use std::{
    any::{Any, TypeId},
    borrow::{Borrow, BorrowMut},
    future::Future,
    hash::{Hash, Hasher},
    mem::{ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    pin::Pin,
};

use crate::{
    alloc::{Allocator, Layout, XLangAlloc},
    ptr::Unique,
};

///
/// ABI Safe implementation of [`std::boxed::Box`] that uses the given Allocator to allocate and deallocate memory
#[repr(C)]
pub struct Box<T: ?Sized, A: Allocator = XLangAlloc> {
    ptr: Unique<T>,
    alloc: A,
}

impl<T: ?Sized, A: Allocator> Drop for Box<T, A> {
    fn drop(&mut self) {
        let layout = Layout::from_val(&**self);
        let ptr = self.ptr.as_nonnull();
        unsafe {
            core::ptr::drop_in_place(ptr.as_ptr());
        }
        unsafe { self.alloc.deallocate(ptr.cast::<u8>(), layout) }
    }
}

impl<T> Box<T, XLangAlloc> {
    /// Creates a new box, containing `x`
    pub fn new(x: T) -> Self {
        Self::new_in(x, XLangAlloc::new())
    }

    /// Creates a new, pinned, Box, containing `x`
    pub fn pin(x: T) -> Pin<Self> {
        Self::pin_in(x, XLangAlloc::new())
    }
}

impl<T: ?Sized> Box<T, XLangAlloc> {
    /// Converts a raw pointer to `T` into a `Box`.
    ///
    /// # Safety
    /// The following constraints must hold:
    /// * x must have been allocated from [`XLangAlloc`], using the layout of `x`, or returned from [`Box::into_inner`] from a `T` with the correct layout using an allocator compatible with [`XLangAlloc`]
    /// * x must not be accessed after this call and must not alias any references
    pub unsafe fn from_raw(x: *mut T) -> Self {
        Self::from_raw_in(x, XLangAlloc::new())
    }
}

impl<T> Box<MaybeUninit<T>, XLangAlloc> {
    /// Allocates a new box containing uninitialized Memory
    #[must_use]
    pub fn new_uninit() -> Self {
        Self::new_uninit_in(XLangAlloc::new())
    }

    /// Allocates a new box containing zeroed memory
    #[must_use]
    pub fn new_zeroed() -> Self {
        Self::new_zeroed_in(XLangAlloc::new())
    }
}

impl<T, A: Allocator> Box<MaybeUninit<T>, A> {
    /// Alllocates a new box using the given allocator, containing uninitialized memory
    pub fn new_uninit_in(alloc: A) -> Self {
        let ptr = alloc
            .allocate(Layout::new::<T>())
            .unwrap_or_else(|| crate::alloc::handle_alloc_error(Layout::new::<T>()))
            .cast::<MaybeUninit<T>>();
        Self {
            ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
            alloc,
        }
    }

    /// Alllocates a new box using the given allocator, containing zereod memory
    pub fn new_zeroed_in(alloc: A) -> Self {
        let ptr = alloc
            .allocate_zeroed(Layout::new::<T>())
            .unwrap_or_else(|| crate::alloc::handle_alloc_error(Layout::new::<T>()))
            .cast::<MaybeUninit<T>>();
        Self {
            ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
            alloc,
        }
    }

    /// Assumes that `this` contains an initialized `T`, and converts it into a [`Box<T,A>`].
    /// The conversion is performed in-place, no values are moved from or to the stack or any other memory (thus this is safe to call on a pinned Box)
    ///
    /// # Safety
    /// `this` must contain a valid value of type `T`
    pub unsafe fn assume_init(this: Self) -> Box<T, A> {
        let (ptr, alloc) = Self::into_raw_with_alloc(this);

        Box::from_raw_in(ptr.cast(), alloc)
    }
}

impl<T, A: Allocator> Box<T, A> {
    /// Allocates a new box using `alloc` that contains `x`
    pub fn new_in(x: T, alloc: A) -> Self {
        let ptr = alloc
            .allocate(Layout::new::<T>())
            .unwrap_or_else(|| crate::alloc::handle_alloc_error(Layout::new::<T>()))
            .cast::<T>();
        unsafe { core::ptr::write(ptr.as_ptr(), x) }
        Self {
            ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
            alloc,
        }
    }

    /// Allocates a new box using `alloc` that contains `x`, and pins it in place.
    pub fn pin_in(x: T, alloc: A) -> Pin<Self>
    where
        A: 'static,
    {
        unsafe { Pin::new_unchecked(Self::new_in(x, alloc)) }
    }

    /// Moves the contained value out of `this` and returns it. The memory containing the T is deallocated using the allocator
    pub fn into_inner(this: Self) -> T {
        Self::into_inner_with_alloc(this).0
    }
    /// Moves the contained value out of `this` and returns it and the allocator used by `this`. The memory containing the T is deallocated using the allocator
    pub fn into_inner_with_alloc(this: Self) -> (T, A) {
        let this = ManuallyDrop::new(this);
        let ptr = this.ptr.as_nonnull();
        let alloc = unsafe { core::ptr::read(&this.alloc) };
        let val = unsafe { core::ptr::read(ptr.as_ptr()) };
        let layout = Layout::new::<T>();
        unsafe { alloc.deallocate(ptr.cast(), layout) };
        (val, alloc)
    }
}

impl<T: ?Sized, A: Allocator> Box<T, A> {
    /// Returns the pointer to the interior of `this` and the allocator it was obtained from. The inner value is not dropped or deallocated.
    pub fn into_raw_with_alloc(this: Self) -> (*mut T, A) {
        let this = ManuallyDrop::new(this);
        let ptr = this.ptr.as_nonnull();
        let alloc = unsafe { core::ptr::read(&this.alloc) };
        (ptr.as_ptr(), alloc)
    }

    /// Returns the pointer to the interior of `this`. The inner value is not dropped or deallocated.
    pub fn into_raw(this: Self) -> *mut T {
        Self::into_raw_with_alloc(this).0
    }

    /// Converts a raw pointer to `T` into a `Box`.
    ///
    /// # Safety
    /// * x must have been allocated from `alloc` or a compatible allocator, using the layout of `x`, or returned from [`Box::into_inner`] from a `T` with the correct layout using an allocator compatible with [`XLangAlloc`]
    /// * x must not be accessed after this call and must not alias any references
    pub unsafe fn from_raw_in(x: *mut T, alloc: A) -> Self {
        Self {
            ptr: Unique::new_unchecked(x),
            alloc,
        }
    }

    /// Leaks `this` into a mutable reference with an arbitrary lifetime.
    ///
    /// Note: the returned reference is not the same as a pointer obtained from [`Box::into_raw`]. The resulting reference cannot be safely passed into [`Box::from_raw`] or [`Box::from_raw_in`].
    pub fn leak<'a>(this: Self) -> &'a mut T
    where
        A: 'a,
    {
        let ptr = Self::into_raw(this);

        unsafe { &mut *ptr }
    }

    /// [`core::pin::Pin`]s this in place safely.
    pub fn into_pin(this: Self) -> Pin<Self>
    where
        A: 'static,
    {
        unsafe { Pin::new_unchecked(this) }
    }

    /// Returns the Allocator used by `this`
    pub fn allocator(this: &Self) -> &A {
        &this.alloc
    }
}

#[allow(clippy::missing_errors_doc)]
impl<A: Allocator> Box<dyn Any, A> {
    /// Downcasts `this` into `T`
    /// Returns `this` if the downcast fails
    pub fn downcast<T: Any>(this: Self) -> Result<Box<T, A>, Self> {
        if (&*this).type_id() == TypeId::of::<T>() {
            let (ptr, alloc) = Self::into_raw_with_alloc(this);

            Ok(unsafe { Box::from_raw_in(ptr.cast(), alloc) })
        } else {
            Err(this)
        }
    }
}

#[allow(clippy::missing_errors_doc)]
impl<A: Allocator> Box<dyn Any + Send, A> {
    /// Downcasts `this` into `T`
    /// Returns `this` if the downcast fails
    pub fn downcast<T: Any>(this: Self) -> Result<Box<T, A>, Self> {
        if (&*this).type_id() == TypeId::of::<T>() {
            let (ptr, alloc) = Self::into_raw_with_alloc(this);

            Ok(unsafe { Box::from_raw_in(ptr.cast(), alloc) })
        } else {
            Err(this)
        }
    }
}

#[allow(clippy::missing_errors_doc)]
impl<A: Allocator> Box<dyn Any + Send + Sync, A> {
    /// Downcasts `this` into `T`
    /// Returns `this` if the downcast fails
    pub fn downcast<T: Any>(this: Self) -> Result<Box<T, A>, Self> {
        if (&*this).type_id() == TypeId::of::<T>() {
            let (ptr, alloc) = Self::into_raw_with_alloc(this);

            Ok(unsafe { Box::from_raw_in(ptr.cast(), alloc) })
        } else {
            Err(this)
        }
    }
}

impl<T: ?Sized, A: Allocator> Deref for Box<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized, A: Allocator> DerefMut for Box<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: ?Sized, A: Allocator> AsRef<T> for Box<T, A> {
    fn as_ref(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized, A: Allocator> AsMut<T> for Box<T, A> {
    fn as_mut(&mut self) -> &mut T {
        &mut **self
    }
}

impl<T: ?Sized, A: Allocator> Borrow<T> for Box<T, A> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<T: ?Sized, A: Allocator> BorrowMut<T> for Box<T, A> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut **self
    }
}

impl<T: Default, A: Allocator + Default> Default for Box<T, A> {
    fn default() -> Self {
        Self::new_in(Default::default(), Default::default())
    }
}

impl<T> Default for Box<[T], XLangAlloc> {
    fn default() -> Self {
        let (ptr, alloc) = Box::into_raw_with_alloc(Box::<[T; 0]>::new([]));
        unsafe { Self::from_raw_in(ptr as *mut [T], alloc) }
    }
}

impl Default for Box<str, XLangAlloc> {
    fn default() -> Self {
        let (ptr, alloc) = Box::into_raw_with_alloc(Box::<[u8; 0]>::new([]));
        unsafe { Self::from_raw_in(ptr as *mut [u8] as *mut str, alloc) }
    }
}

impl<T: Clone, A: Allocator + Clone> Clone for Box<T, A> {
    fn clone(&self) -> Self {
        let alloc = Self::allocator(self).clone();
        let val = T::clone(self);
        Self::new_in(val, alloc)
    }
}

unsafe impl<T: Allocator + ?Sized, A: Allocator> Allocator for Box<T, A> {
    fn allocate_zeroed(&self, layout: Layout) -> Option<std::ptr::NonNull<u8>> {
        T::allocate_zeroed(self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: std::ptr::NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<std::ptr::NonNull<u8>> {
        T::grow(self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: std::ptr::NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<std::ptr::NonNull<u8>> {
        T::grow_zeroed(self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: std::ptr::NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Option<std::ptr::NonNull<u8>> {
        T::shrink(self, ptr, old_layout, new_layout)
    }

    fn allocate(&self, layout: Layout) -> Option<std::ptr::NonNull<u8>> {
        T::allocate(self, layout)
    }

    unsafe fn deallocate(&self, ptr: std::ptr::NonNull<u8>, layout: Layout) {
        T::deallocate(self, ptr, layout);
    }
}

impl<T: ?Sized, A: Allocator + 'static> Unpin for Box<T, A> {}

impl<F: ?Sized + Future + Unpin, A: Allocator + 'static> Future for Box<F, A> {
    type Output = F::Output;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        F::poll(Pin::new(self.get_mut()), cx)
    }
}

impl<T: ?Sized + Hash, A: Allocator> Hash for Box<T, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        T::hash(self, state);
    }
}

impl<I: ?Sized + Iterator, A: Allocator> Iterator for Box<I, A> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        I::next(self)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        I::size_hint(self)
    }
}

impl<I: ?Sized + ExactSizeIterator, A: Allocator> ExactSizeIterator for Box<I, A> {}

impl<I: ?Sized + DoubleEndedIterator, A: Allocator> DoubleEndedIterator for Box<I, A> {
    fn next_back(&mut self) -> Option<Self::Item> {
        I::next_back(self)
    }
}

impl<T: core::fmt::Debug + ?Sized, A: Allocator> core::fmt::Debug for Box<T, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        T::fmt(self, f)
    }
}

impl<T: PartialEq<U> + ?Sized, U: ?Sized, A: Allocator, A1: Allocator> PartialEq<Box<U, A1>>
    for Box<T, A>
{
    fn eq(&self, other: &Box<U, A1>) -> bool {
        T::eq(self, other)
    }
}

impl<T: Eq, A: Allocator> Eq for Box<T, A> {}

impl<T: PartialOrd<U> + ?Sized, U: ?Sized, A: Allocator, A1: Allocator> PartialOrd<Box<U, A1>>
    for Box<T, A>
{
    fn partial_cmp(&self, other: &Box<U, A1>) -> Option<std::cmp::Ordering> {
        T::partial_cmp(self, other)
    }
}

impl<T: Ord, A: Allocator> Ord for Box<T, A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        T::cmp(self, other)
    }
}

#[cfg(test)]
mod test {
    use super::Box;

    #[test]
    fn test_box() {
        let b = Box::new(5i32);
        assert_eq!(Box::into_inner(b), 5i32);
    }

    #[test]
    fn test_deref() {
        let b = Box::new(5i32);

        assert_eq!(&*b, &5i32);
    }
}
