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
    pub fn new(x: T) -> Self {
        Self::new_in(x, XLangAlloc::new())
    }

    pub fn pin(x: T) -> Pin<Self> {
        Self::pin_in(x, XLangAlloc::new())
    }
}

impl<T: ?Sized> Box<T, XLangAlloc> {
    pub unsafe fn from_raw(x: *mut T) -> Self {
        Self::from_raw_in(x, XLangAlloc::new())
    }
}

impl<T> Box<MaybeUninit<T>, XLangAlloc> {
    pub fn new_uninit() -> Self {
        Self::new_uninit_in(XLangAlloc::new())
    }

    pub fn new_zeroed() -> Self {
        Self::new_zeroed_in(XLangAlloc::new())
    }
}

impl<T, A: Allocator> Box<MaybeUninit<T>, A> {
    pub fn new_uninit_in(alloc: A) -> Self {
        let ptr = alloc
            .allocate(Layout::new::<T>())
            .unwrap_or_else(|| crate::alloc::handle_alloc_error(Layout::new::<T>()))
            .cast::<MaybeUninit<T>>();
        Box {
            ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
            alloc,
        }
    }

    pub fn new_zeroed_in(alloc: A) -> Self {
        let ptr = alloc
            .allocate_zeroed(Layout::new::<T>())
            .unwrap_or_else(|| crate::alloc::handle_alloc_error(Layout::new::<T>()))
            .cast::<MaybeUninit<T>>();
        Box {
            ptr: unsafe { Unique::new_nonnull_unchecked(ptr) },
            alloc,
        }
    }

    pub unsafe fn assume_init(this: Self) -> Box<T, A> {
        let (ptr, alloc) = Self::into_raw_with_alloc(this);

        Box::from_raw_in(ptr.cast(), alloc)
    }
}

impl<T, A: Allocator> Box<T, A> {
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

    pub fn pin_in(x: T, alloc: A) -> Pin<Box<T, A>> {
        unsafe { Pin::new_unchecked(Self::new_in(x, alloc)) }
    }

    pub fn into_inner(this: Self) -> T {
        Self::into_inner_with_alloc(this).0
    }
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
    pub fn into_raw_with_alloc(this: Self) -> (*mut T, A) {
        let this = ManuallyDrop::new(this);
        let ptr = this.ptr.as_nonnull();
        let alloc = unsafe { core::ptr::read(&this.alloc) };
        (ptr.as_ptr(), alloc)
    }

    pub fn into_raw(this: Self) -> *mut T {
        Self::into_raw_with_alloc(this).0
    }

    pub unsafe fn from_raw_in(x: *mut T, alloc: A) -> Self {
        Self {
            ptr: Unique::new_unchecked(x),
            alloc,
        }
    }

    pub fn leak<'a>(this: Self) -> &'a mut T
    where
        A: 'a,
    {
        let ptr = Self::into_raw(this);

        unsafe { &mut *ptr }
    }

    pub fn into_pin(this: Self) -> Pin<Self> {
        unsafe { Pin::new_unchecked(this) }
    }

    pub fn allocator(this: &Self) -> &A {
        &this.alloc
    }
}

impl<A: Allocator> Box<dyn Any, A> {
    pub fn downcast<T: Any>(this: Self) -> Result<Box<T, A>, Self> {
        if (&*this).type_id() == TypeId::of::<T>() {
            let (ptr, alloc) = Self::into_raw_with_alloc(this);

            Ok(unsafe { Box::from_raw_in(ptr.cast(), alloc) })
        } else {
            Err(this)
        }
    }
}

impl<A: Allocator> Box<dyn Any + Send, A> {
    pub fn downcast<T: Any>(this: Self) -> Result<Box<T, A>, Self> {
        if (&*this).type_id() == TypeId::of::<T>() {
            let (ptr, alloc) = Self::into_raw_with_alloc(this);

            Ok(unsafe { Box::from_raw_in(ptr.cast(), alloc) })
        } else {
            Err(this)
        }
    }
}

impl<A: Allocator> Box<dyn Any + Send + Sync, A> {
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
        Box::new_in(Default::default(), Default::default())
    }
}

impl<T> Default for Box<[T], XLangAlloc> {
    fn default() -> Self {
        let (ptr, alloc) = Box::into_raw_with_alloc(Box::<[T; 0]>::new([]));
        unsafe { Box::from_raw_in(ptr as *mut [T], alloc) }
    }
}

impl Default for Box<str, XLangAlloc> {
    fn default() -> Self {
        let (ptr, alloc) = Box::into_raw_with_alloc(Box::<[u8; 0]>::new([]));
        unsafe { Box::from_raw_in(ptr as *mut [u8] as *mut str, alloc) }
    }
}

impl<T: Clone, A: Allocator + Clone> Clone for Box<T, A> {
    fn clone(&self) -> Self {
        let alloc = Self::allocator(self).clone();
        let val = T::clone(self);
        Box::new_in(val, alloc)
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
        T::deallocate(self, ptr, layout)
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
        T::hash(self, state)
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