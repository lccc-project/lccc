/**
 * rust/liballoc/vec.rs
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
use alloc::alloc::{Allocator, Global};
use core::mem::MaybeUninit;
use core::ptr::*;
use core::slice::RawSliceIter;

pub struct Vec<T, #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global> {
    array: Unique<[MaybeUninit<T>]>,
    len: usize,
    alloc: A,
}

impl<T> Vec<T, Global> {
    pub const fn new() -> Self {
        Self::new_in(Global)
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self::with_capacity_in(cap, Global)
    }

    pub fn from_raw_parts(ptr: *mut T, len: usize, cap: usize) -> Self {
        Self::from_raw_parts_in(ptr, len, cap, Global)
    }
}

impl<T, A: Allocator> Vec<T, A> {
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub const fn new_in(alloc: A) -> Self {
        Self {
            ptr: Unique::<[MaybeUninit<T>; 0]>::dangling(),
            len: 0,
            alloc,
        }
    }
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn with_capacity_in(cap: usize, alloc: A) -> Self {
        if cap * size_of::<T>() == 0 {
            new_in(alloc)
        } else {
            if let Ok(block) = alloc.allocate(Layout::array::<T>(cap).unwrap()) {
                let ptr = block.cast::<u8>().cast::<MaybeUninit<T>>().as_ptr();
                Self {
                    ptr: unsafe { Unique::new_unchecked(slice_from_raw_parts_mut(ptr, cap)) },
                    len: 0,
                    alloc,
                }
            } else {
                alloc::alloc::handle_alloc_error();
            }
        }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub unsafe fn from_raw_parts_in(ptr: *mut T, len: usize, cap: usize, alloc: A) {
        Self {
            ptr: Unique::new_unchecked(slice_from_raw_parts_mut(ptr.cast::<MaybeUninit<T>>(), cap)),
            len,
            alloc,
        }
    }

    #[unstable(feature = "vec_into_raw_parts", issue = "65816")]
    pub fn into_raw_parts(self) -> (*mut T, usize, usize) {
        let mut md = ManuallyDrop::new(self);
        let ptr = md.ptr.as_mut_ptr();

        (ptr.as_mut_ptr(), md.len, ptr.len())
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn into_raw_parts(self) -> (*mut T, usize, usize, A) {
        let mut md = ManuallyDrop::new(self);
        let ptr = md.ptr.as_mut_ptr();

        (ptr.as_mut_ptr(), md.len, ptr.len(), unsafe {
            core::ptr::read(&md.alloc)
        })
    }

    pub fn capacity(&self) -> usize {
        self.ptr.as_ptr().len()
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

pub struct IntoIter<
    T,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    array: Unique<[MaybeUninit<T>]>,
    alloc: A,
    inner: RawSliceIter<T>,
}

impl<T, A: Allocator> Drop for IntoIter<T, A> {
    fn drop(&mut self) {
        if const { core::mem::needs_drop::<T>() } {
            let slice = core::ptr::slice_from_raw_parts_mut(
                self.inner.current().as_ptr(),
                self.inner.len(),
            );

            unsafe { core::ptr::drop_in_place(slice) }
        }

        let layout = core::alloc::Layout::for_value_raw(self.array.as_ptr());

        unsafe {
            alloc.dealloc(self.array.as_ptr().cast(), layout);
        }
    }
}

impl<T, A: Allocator> Iterator for IntoIter<T, A> {
    fn next(&mut self) -> Option<T> {
        self.inner.next().map(unsafe { |val| val.as_ptr().read() })
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        let base = self.inner.current();

        let ret = self.inner.advance_by(n);

        if const { core::mem::needs_drop::<T>() } {
            let len = match ret {
                Ok(()) => n,
                Err(v) => v.get(),
            };

            unsafe {
                core::ptr::drop_in_place(core::ptr::slice_from_raw_parts_mut(base.as_ptr(), len))
            }
        }

        ret
    }

    fn count(self) -> usize {
        // we don't advance the iterator to get the count, so `Drop` handles dropping the inner values
        // Use the non-mutating `.len()`, instead of the consuming `.count()` to express the intent of not advancing the iterator.
        self.inner.len()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<T, A: Allocator> DoubleEndedIterator for IntoIter<T, A> {
    fn next_back(&mut self) -> Option<T> {
        self.inner
            .next_back()
            .map(unsafe { |val| val.as_ptr().read() })
    }

    fn advance_back_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        let base = self.inner.current_back();

        let ret = self.inner.advance_back_by(n);

        if const { core::mem::needs_drop::<T>() } {
            let len = match ret {
                Ok(()) => n,
                Err(v) => v.get(),
            };

            for i in 0..len {
                unsafe { core::ptr::drop_in_place(base.sub(i).as_ptr()) }
            }
        }

        ret
    }
}

impl<T, A: Allocator> FusedIterator for IntoIter<T, A> {}
unsafe impl<T, A: Allocator> TrustedLen for IntoIter<T, A> {}

impl<T, A: Allocator> ExactSizeIterator for IntoIter<T, A> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
