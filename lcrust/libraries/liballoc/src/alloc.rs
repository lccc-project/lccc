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

pub use ::core::alloc::*;
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

extern "Rust" {
    #[no_mangle]
    static _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE:
        &dyn GlobalAlloc;
}

pub unsafe fn alloc(layout: Layout) -> *mut u8 {
    if core::ptr::raw_ref!(
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
    )
    .is_null()
    {
        handle_alloc_error(layout)
    } else {
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
            .alloc(layout)
    }
}

pub unsafe fn alloc_zeroed(layout: Layout) -> *mut u8 {
    if core::ptr::raw_ref!(
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
    )
    .is_null()
    {
        handle_alloc_error(layout)
    } else {
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
            .alloc_zeroed(layout)
    }
}

pub unsafe fn dealloc(ptr: *mut u8, layout: Layout) {
    if core::ptr::raw_ref!(
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
    )
    .is_null()
    {
    } else {
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
            .dealloc(ptr, layout)
    }
}

pub unsafe fn realloc(ptr: *mut u8, old_layout: Layout, new_size: usize) -> *mut u8 {
    if core::ptr::raw_ref!(
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
    )
    .is_null()
    {
        handle_alloc_error(layout)
    } else {
        _ZN5alloc5alloc29__lccc_rust_global_alloc_implRCu3dynIN5alloc5alloc11GlobalAllocE
            .realloc(ptr, old_layout, new_size)
    }
}

pub fn handle_alloc_error(layout: Layout) -> ! {
    extern "Rust" {
        #[no_mangle]
        fn _ZN5alloc5alloc35__lccc_rust_handle_alloc_error_implu5neverN4core5alloc6Layout(
            _: Layout,
        ) -> !;
    }
    unsafe {
        _ZN5alloc5alloc35__lccc_rust_handle_alloc_error_implu5neverN4core5alloc6Layout(layout)
    }
}

#[__lccc::weak]
#[__lccc::use_mangling("itanium")]
#[unstable(feature="lccc_handle_alloc_error_impl")]
pub fn __lccc_rust_handle_alloc_error_impl(_: Layout) -> ! {
    ::__lccc::builtins::C::__builtin_trap()
}

#[unstable(feature = "allocator_api", issue = "32838")]
pub struct Global;

unsafe impl Allocator for Global {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(unsafe {
                NonNull::new_unchecked(core::ptr::slice_from_raw_parts_mut(1usize as *mut u8, 0))
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
