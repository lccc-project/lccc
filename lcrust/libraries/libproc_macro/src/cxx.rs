/**
 * rust/libproc_macro/cxx.rs
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
use alloc::alloc::Allocator;

#[derive(Copy, Clone)]
pub struct CXXAllocator{
    _padding: ::core::mem::MaybeUninit<u8>
}

#[repr(C)]
struct NoThrowT {
    _padding: ::core::mem::MaybeUninit<u8>,
}

static no_throw: NoThrowT = NoThrow {
    _padding: MaybeUninit::zeroed(),
};

#[link = "xlang"]
extern "C" {
    pub fn xlang_allocate(sz: usize) -> *mut ::core::ffi::c_void;
    pub fn xlang_allocate_aligned(sz: usize, align: usize) -> *mut ::core::ffi::c_void;
    pub fn xlang_deallocate(ptr: *mut ::core::ffi::c_void);
    pub fn xlang_deallocate(ptr: *mut ::core::ffi::c_void, align: usize);
}

impl Allocator for CXXAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(layout.dangling().cast::<[u8; 0]>())
        } else if layout.align() <= core::mem::align_of::<::__lccc::platform::max_align_t>() {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { xlang_allocate(layout.size()) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        } else {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { xlang_allocate_aligned(layout.size(), layout.align(), &no_throw) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        if layout.size() == 0 {
        } else if layout.align() <= core::mem::align_of::<::__lccc::platform::max_align_t>() {
            // SAFETY:
            // The pointer was guaranteed to be returned from an allocate call on this allocator with the same layout
            // Thus, we know that the pointer was returned from the three-arg form of the global replaceable, nonthrowing, operator new.
            unsafe {
                xlang_deallocate_aligned(ptr.as_ptr() as *mut ::core::ffi::c_void, layout.align())
            }
        } else {
            // SAFETY:
            // The pointer was guaranteed to be returned from an allocate call on this allocator with the same layout
            // Thus, we know that the pointer was returned from the two-arg form of the global replaceable, nonthrowing, operator new
            unsafe { xlang_deallocate(ptr.as_ptr() as *mut ::core::ffi::c_void) }
        }
    }
}

#[cfg(target_exceptions = "itanium")]
mod itanium;

