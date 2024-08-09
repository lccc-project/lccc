/**
 * rust/liballoc/alloc.rs
 * This file is part of lcrust standard libraries, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * Like all libraries as part of the lccc project,
 *  the lcrust standard libraries are additionally dual licensed under the terms of the MIT and Apache v2 license.
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms.
 */
pub use core::alloc::*;
use core::ptr::NonNull;

#[unstable(feature = "allocator_api", issue = "32838")]
pub struct AllocError;

#[unstable(feature = "allocator_api", issue = "32838")]
pub unsafe trait Allocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError>;
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout);

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let block = self.allocate(layout)?;
        ::__lccc::builtins::C::__builtin_memset(bytes.as_ptr() as *mut u8, 0, layout.size());
        Ok(block)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let block = self.allocate(new_layout)?;
        core::ptr::copy_nonoverlapping(ptr.as_ptr(), block.as_ptr() as *mut u8, old_layout.size());
        self.deallocate(ptr, old_layout);
        Ok(block)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let block = self.allocate_zeroed(new_layout)?;
        core::ptr::copy_nonoverlapping(ptr.as_ptr(), block.as_ptr() as *mut u8, old_layout.size());
        self.deallocate(ptr, old_layout);
        Ok(block)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        let block = self.allocate(new_layout)?;
        core::ptr::copy_nonoverlapping(ptr.as_ptr(), block.as_ptr() as *mut u8, new_layout.size());
        self.deallocate(ptr, old_layout);
        Ok(block)
    }

    fn by_ref(&self) -> &Self {
        self
    }
}

#[unstable(feature = "lccc_allocator_api_align_safety")]
pub unsafe trait AlignSafeAllocator: Allocator {
    #[unstable(feature = "lccc_allocator_api_align_safety")]
    fn alloc_align_compatible(&self, old_align: usize, new_align: usize) -> bool;
}

unsafe impl<A: Allocator> Allocator for &'_ A {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        (*self).allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (*self).deallocate(ptr, layout)
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        (*self).allocate_zeroed(layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).grow(ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).grow_zeroed(ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).shrink(ptr, old_layout, new_layout)
    }
}

unsafe impl<A: Allocator + AlignSafeAllocator> AlignSafeAllocator for &'_ A {
    fn alloc_align_compatible(&self, old_align: usize, new_align: usize) -> bool {
        <A as AlignSafeAllocator>::alloc_align_compatible(self, old_align, new_align)
    }
}

unsafe impl<A: Allocator> Allocator for &'_ mut A {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        (*self).allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (*self).deallocate(ptr, layout)
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        (*self).allocate_zeroed(layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).grow(ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).grow_zeroed(ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (*self).shrink(ptr, old_layout, new_layout)
    }
}

unsafe impl<A: Allocator + AlignSafeAllocator> AlignSafeAllocator for &'_ mut A {
    fn alloc_align_compatible(&self, old_align: usize, new_align: usize) -> bool {
        <A as AlignSafeAllocator>::alloc_align_compatible(self, old_align, new_align)
    }
}

extern "Rust" {
    #[no_mangle]
    static _ZNSt5alloc18__global_allocatorE: &dyn GlobalAlloc;
}

#[lcrust::allocator_function(layout = "layout", style = "malloc")]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub unsafe fn alloc(layout: Layout) -> *mut u8 {
    if core::ptr::raw_const!(_ZNSt5alloc18__global_allocatorE).is_null() {
        handle_alloc_error(layout)
    } else {
        _ZNSt5alloc18__global_allocatorE.alloc(layout)
    }
}

#[lcrust::allocator_function(layout = "layout", style = "calloc")]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub unsafe fn alloc_zeroed(layout: Layout) -> *mut u8 {
    if core::ptr::raw_const!(_ZNSt5alloc18__global_allocatorE).is_null() {
        handle_alloc_error(layout)
    } else {
        _ZNSt5alloc18__global_allocatorE.alloc_zeroed(layout)
    }
}

#[lcrust::allocator_function(layout = "layout", ptr = "ptr", style = "free")]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub unsafe fn dealloc(ptr: *mut u8, layout: Layout) {
    if core::ptr::raw_const!(_ZNSt5alloc18__global_allocatorE).is_null() {
    } else {
        _ZNSt5alloc18__global_allocatorE.dealloc(ptr, layout)
    }
}

#[lcrust::allocator_function(
    layout = "old_layout",
    ptr = "ptr",
    new_layout = "{new_size,old_layout.align}",
    style = "realloc"
)]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub unsafe fn realloc(ptr: *mut u8, old_layout: Layout, new_size: usize) -> *mut u8 {
    if core::ptr::raw_const!(_ZNSt5alloc18__global_allocatorE).is_null() {
        handle_alloc_error(layout)
    } else {
        _ZNSt5alloc18__global_allocatorE.realloc(ptr, old_layout, new_size)
    }
}

#[unstable(feature = "lccc_alloc_align_safety")]
#[lcrust::assume_no_unwind]
#[lcrust::pure]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub fn alloc_align_compatible(old_align: usize, new_align: usize) -> bool {
    if core::ptr::raw_const!(_ZNSt5alloc18__global_allocatorE).is_null() {
        true // trivially -
    } else {
        _ZNSt5alloc18__global_allocatorE.alloc_align_compatible(old_align, new_align)
    }
}

#[lcrust::assume_no_unwind]
#[cold]
#[inline]
#[lcrust::force_export] // alloc::alloc::alloc and friends are required-definition symbols, but we want to be able to inline them.
pub fn handle_alloc_error(layout: Layout) -> ! {
    extern "Rust" {
        #[no_mangle]
        fn _ZNSt5alloc35__lccc_rust_handle_alloc_error_implE_ZN4core5alloc6LayoutE(_: Layout) -> !;
    }
    unsafe { _ZNSt5alloc35__lccc_rust_handle_alloc_error_implE_ZN4core5alloc6LayoutE(layout) }
}

#[lcrust::weak_def]
#[unstable(feature = "lccc_handle_alloc_error_impl")]
pub fn __lccc_rust_handle_alloc_error_impl(_: Layout) -> ! {
    ::__lccc::builtins::C::__builtin_trap()
}

#[unstable(feature = "allocator_api", issue = "32838")]
pub struct Global;

unsafe impl Allocator for Global {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(unsafe {
                NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(
                    layout.align() as *mut u8,
                    0,
                ))
            })
        } else {
            let ptr = unsafe { self::alloc(layout) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(ptr, layout.size()))
                }
            }
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        if layout.size() == 0 {
            ()
        } else {
            self::dealloc(ptr.as_ptr(), layout)
        }
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(unsafe {
                NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(1usize as *mut u8, 0))
            })
        } else {
            let ptr = unsafe { self::alloc_zeroed(layout) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(ptr, layout.size()))
                }
            }
        }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        if old_layout.size() == 0 {
            self.alloc(new_layout)
        } else {
            let ptr = self::realloc(ptr.as_ptr(), old_layout, new_layout.size());
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(NonNull::new_unchecked(core::slice::from_raw_parts_mut(
                    ptr,
                    new_layout.size(),
                )))
            }
        }
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        if old_layout.size() == 0 {
            self.alloc(new_layout)
        } else {
            let ptr = self::realloc(ptr.as_ptr(), old_layout, new_layout.size());
            if ptr.is_null() {
                Err(AllocError)
            } else {
                ::__lccc::builtins::C::__builtin_memset(
                    ptr.add(old_layout.size()),
                    0,
                    new_layout.size() - old_layout.size(),
                );
                Ok(NonNull::new_unchecked(core::slice::from_raw_parts_mut(
                    ptr,
                    new_layout.size(),
                )))
            }
        }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        if new_layout.size() == 0 {
            self.dealloc(ptr, old_layout);
            Ok(unsafe {
                NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(1usize as *mut u8, 0))
            })
        } else {
            let ptr = self::realloc(ptr.as_ptr(), old_layout, new_layout.size());
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok
            }
        }
    }
}

#[unstable(feature = "lccc_alloc_align_safety")]
unsafe impl AlignSafeAllocator for Global {
    fn alloc_align_compatible(&self, new_align: usize, old_align: usize) -> bool {
        alloc_align_compatible(new_align, old_align)
    }
}
