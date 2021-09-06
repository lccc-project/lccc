pub use std::alloc::{GlobalAlloc, Layout};
use std::{
    hash::{Hash, Hasher},
    mem::MaybeUninit,
    ptr::NonNull,
};

extern "C" {
    pub fn xlang_allocate(size: usize) -> *mut core::ffi::c_void;
    pub fn xlang_allocate_aligned(size: usize, align: usize) -> *mut core::ffi::c_void;
    pub fn xlang_deallocate(ptr: *mut core::ffi::c_void, size: usize);
    pub fn xlang_deallocate_aligned(ptr: *mut core::ffi::c_void, size: usize, align: usize);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AllocError;

impl ::std::fmt::Display for AllocError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Allocation Failure")
    }
}

pub unsafe trait Allocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<u8>, AllocError>;
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout);

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        let ptr = self.allocate(layout)?;
        unsafe {
            core::ptr::write_bytes(ptr.as_ptr().cast::<u8>(), 0, layout.size());
        }
        Ok(ptr)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let nptr = self.allocate(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                old_layout.size(),
            )
        }
        self.deallocate(ptr, old_layout);
        Ok(nptr)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let nptr = self.allocate_zeroed(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                old_layout.size(),
            )
        }
        self.deallocate(ptr, old_layout);
        Ok(nptr)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        let nptr = self.allocate(new_layout)?;
        #[allow(unused_unsafe)]
        unsafe {
            core::ptr::copy_nonoverlapping(
                ptr.as_ptr(),
                nptr.as_ptr().cast::<u8>(),
                new_layout.size(),
            )
        }
        self.deallocate(ptr, old_layout);
        Ok(nptr)
    }
}

unsafe impl<A: ?Sized + Allocator> Allocator for &A {
    fn allocate(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        A::allocate(self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        A::deallocate(self, ptr, layout)
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        A::allocate_zeroed(self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::grow(self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::grow_zeroed(self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::shrink(self, ptr, old_layout, new_layout)
    }
}

unsafe impl<A: ?Sized + Allocator> Allocator for &mut A {
    fn allocate(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        A::allocate(self, layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        A::deallocate(self, ptr, layout)
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        A::allocate_zeroed(self, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::grow(self, ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::grow_zeroed(self, ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<u8>, AllocError> {
        A::shrink(self, ptr, old_layout, new_layout)
    }
}

#[derive(Copy, Clone)]
pub struct XLangAlloc(MaybeUninit<u8>);

impl XLangAlloc {
    pub const fn new() -> Self {
        Self(MaybeUninit::uninit())
    }
}

impl core::fmt::Debug for XLangAlloc {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        fmt.write_str("XLangAlloc")
    }
}

impl Hash for XLangAlloc {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl PartialEq for XLangAlloc {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for XLangAlloc {}

impl Default for XLangAlloc {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Allocator for XLangAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<u8>, AllocError> {
        NonNull::new(unsafe { xlang_allocate_aligned(layout.size(), layout.align()).cast::<u8>() })
            .ok_or(AllocError)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        xlang_deallocate_aligned(ptr.as_ptr().cast(), layout.size(), layout.align())
    }
}
