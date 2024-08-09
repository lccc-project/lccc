/**
 * rust/libcore/slice.rs
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
use crate::intrinsics::transmute;

use crate::iter::{
    DoubleEndedIterator, ExactSizeIterator, FusedIterator, IntoIterator, Iterator, TrustedLen,
};
use crate::ptr::NonNull;

// lcrust implementation detail. Might open an RFC to make this part of rust
#[doc(hidden)]
#[unstable(feature = "lccc_slice_layout")]
#[lang = "slice_ptr_layout"]
pub struct RawSlice<T> {
    ptr: *mut T,
    len: usize,
}

#[inline(always)]
pub unsafe fn from_raw_parts<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    transmute(RawSlice {
        ptr: ::__lccc::builtins::rust::limit(ptr, len) as *mut T,
        len,
    })
}

#[inline(always)]
pub fn from_mut<T>(obj: &mut T) -> &mut [T] {
    unsafe {
        transmute(RawSlice {
            ptr: obj as *mut T,
            len: 1,
        })
    }
}

#[inline(always)]
pub fn from_ref<T>(obj: &T) -> &'_ [T] {
    unsafe {
        transmute(RawSlice {
            ptr: obj as *const T as *mut (),
            len: 1,
        })
    }
}

#[inline(always)]
pub unsafe fn from_raw_parts_mut<'a, T>(ptr: *mut T, len: usize) -> &'a mut [T] {
    transmute(RawSlice {
        ptr: ::__lccc::builtins::rust::limit(ptr, len),
        len,
    })
}

#[lang = "slice"]
#[__lccc::mangle_as("std::slice::slice")]
impl<T> [T] {
    #[inline(always)]
    pub const fn size(&self) -> usize {
        unsafe { transmute::<_, RawSlice<T>>(self).len }
    }
    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        unsafe { transmute::<_, RawSlice<T>>(self).len == 0 }
    }

    pub fn first(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &self.get_unchecked() })
        }
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &mut self.get_unchecked_mut() })
        }
    }

    pub const fn as_ptr(&self) -> *const T {
        unsafe { transmute::<_, RawSlice<T>>(self).ptr as *const T }
    }

    pub fn as_mut_ptr(&self) -> *mut T {
        unsafe { transmute::<_, RawSlice<T>>(self).ptr }
    }
}

mod sealed {
    pub trait SealedSliceIndex<T: ?Sized> {}
}

pub unsafe trait SliceIndex<T: ?Sized>: sealed::SealedSliceIndex<T> {
    type Output: ?Sized;

    #[unstable(feature = "slice_index_methods")]
    fn get(self, slice: &T) -> Option<&Self::Output>;
    #[unstable(feature = "slice_index_methods")]
    fn get_mut(self, slice: &mut T) -> Option<&mut Self::Output>;
    #[unstable(feature = "slice_index_methods")]
    unsafe fn get_unchecked(self, slice: *const T) -> *const Self::Output;
    #[unstable(feature = "slice_index_methods")]
    unsafe fn get_unchecked_mut(self, slice: *mut T) -> *mut Self::Output;
    #[unstable(feature = "slice_index_methods")]
    fn index(self, slice: &T) -> &Self::Output;
    #[unstable(feature = "slice_index_methods")]
    fn index_mut(self, slice: &mut T) -> &mut Self::Output;
}

impl<T> sealed::SealedSliceIndex<[T]> for usize {}
impl<T> SliceIndex<[T]> for usize {
    type Output = T;

    #[inline]
    fn get(self, slice: &[T]) -> Option<&T> {
        if self <= slice.len() {
            // SAFETY:
            // Length is checked above
            Some(unsafe { &*slice.as_ptr().offset(self) })
        } else {
            None
        }
    }

    #[inline]
    fn get_mut(self, slice: &mut [T]) -> Option<&mut T> {
        if self <= slice.len() {
            // SAFETY:
            // Length is checked above
            Some(unsafe { &*slice.as_mut_ptr().offset(self) })
        } else {
            None
        }
    }

    #[inline(always)]
    unsafe fn get_unchecked(self, slice: *const [T]) -> *const T {
        slice.as_ptr().offset(self)
    }

    #[inline(always)]
    unsafe fn get_unchecked_mut(self, slice: *mut [T]) -> *mut T {
        slice.as_mut_ptr().offset(self)
    }

    #[inline]
    fn index(self, slice: &[T]) -> &T {
        if self <= slice.len() {
            // SAFETY:
            // Length is checked above
            unsafe { &*slice.as_ptr().offset(self) }
        } else {
            panic!();
        }
    }

    #[inline]
    fn index_mut(self, slice: &mut [T]) -> &mut T {
        if self <= slice.len() {
            // SAFETY:
            // Length is checked above
            unsafe { &mut *slice.as_mut_ptr().offset(self) }
        } else {
            panic!();
        }
    }
}

pub(crate) struct RawSliceIter<T> {
    base: core::ptr::NonNull<T>,
    end_or_len: *const T,
}

impl<T> core::iter::Iterator for RawSliceIter<T> {
    type Item = core::ptr::NonNull<T>;

    fn next(&mut self) -> Option<core::ptr::NonNull<T>> {
        if const { core::mem::size_of::<T>() == 0 } {
            if self.end_or_len.addr() == 0 {
                None
            } else {
                self.end_or_len = self.end_or_len.wrapping_sub(1);
                Some(self.base)
            }
        } else {
            if self.base == self.end_or_len {
                None
            } else {
                let val = self.base;
                self.base = unsafe { val.add(1) };
                Some(val)
            }
        }
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        if const { core::mem::size_of::<T>() == 0 } {
            if self.end_or_len.addr() < n {
                Err(unsafe { NonZero::new_unchecked(n - self.end_or_len.addr()) })
            } else {
                self.end_or_len = self.end_or_len.wrapping_sub(n);
                Ok(())
            }
        } else {
            let cur_len = unsafe { self.end_or_len.offset_from(self.base.as_ptr()) };
            if cur_len < n {
                Err(unsafe { NonZero::new_unchecked(n - cur_len) })
            } else {
                self.base = unsafe { self.base.add(n) };
                Ok(())
            }
        }
    }

    fn count(self) -> usize {
        self.len()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T> FusedIterator for RawSliceIter<T> {}

impl<T> ExactSizeIterator for RawSliceIter<T> {
    fn len(&self) -> usize {
        if const { core::mem::size_of::<T>() == 0 } {
            self.end_or_len.addr()
        } else {
            unsafe { self.end_or_len.offset_from(self.base.as_ptr()) }
        }
    }
}

unsafe impl<T> TrustedLen for RawSliceIter<T> {}
