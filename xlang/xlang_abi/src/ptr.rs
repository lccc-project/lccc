use std::{marker::PhantomData, ptr::NonNull};

#[repr(transparent)]
pub(crate) struct Unique<T: ?Sized> {
    ptr: NonNull<T>,
    phantom: PhantomData<T>,
}

impl<T: ?Sized> core::fmt::Debug for Unique<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Unique").field("ptr", &self.ptr).finish()
    }
}

impl<T: ?Sized> core::hash::Hash for Unique<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T: ?Sized> core::cmp::PartialEq for Unique<T> {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self.as_ptr(), other.as_ptr())
    }
}

impl<T: ?Sized> core::cmp::Eq for Unique<T> {}

unsafe impl<T: Send> Send for Unique<T> {}
unsafe impl<T: Sync> Sync for Unique<T> {}
impl<T> Unpin for Unique<T> {}

impl<T> Unique<T> {
    #[must_use]
    pub const fn dangling() -> Self {
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
