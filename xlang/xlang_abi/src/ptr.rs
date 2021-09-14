use std::{marker::PhantomData, ptr::NonNull};

#[repr(transparent)]
pub(crate) struct Unique<T: ?Sized> {
    ptr: NonNull<T>,
    phantom: PhantomData<T>,
}

unsafe impl<T: Send> Send for Unique<T> {}
unsafe impl<T: Sync> Sync for Unique<T> {}
impl<T> Unpin for Unique<T> {}

impl<T> Unique<T> {
    pub fn dangling() -> Self {
        Self {
            ptr: NonNull::dangling(),
            phantom: PhantomData,
        }
    }
}

impl<T: ?Sized> Unique<T> {
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Self {
        Self {
            ptr: NonNull::new_unchecked(ptr),
            phantom: PhantomData,
        }
    }

    pub const unsafe fn new_nonnull_unchecked(ptr: NonNull<T>) -> Self {
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    pub unsafe fn as_ref(&self) -> &T {
        self.ptr.as_ref()
    }

    pub unsafe fn as_mut(&mut self) -> &mut T {
        self.ptr.as_mut()
    }

    pub const fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }

    pub const fn as_nonnull(&self) -> NonNull<T> {
        self.ptr
    }
}
