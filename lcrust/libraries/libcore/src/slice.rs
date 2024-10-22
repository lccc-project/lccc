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
pub struct RawSlice<T> {
    ptr: *mut T,
    len: usize,
}

#[inline(always)]
pub unsafe fn from_raw_parts<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    transmute(RawSlice { ptr, len })
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
    transmute(RawSlice { ptr, len })
}

#[lang = "slice"]
#[__lccc::mangle_as("std::slice::slice")]
impl<T> [T] {
    #[inline(always)]
    pub const fn len(&self) -> usize {
        unsafe { transmute::<_, RawSlice<T>>(self).len }
    }
    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        unsafe { transmute::<_, RawSlice<T>>(self).len == 0 }
    }

    pub const fn first(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &self.get_unchecked(0) })
        }
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        if self.is_empty() {
            None
        } else {
            Some(unsafe { &mut self.get_unchecked_mut(0) })
        }
    }

    pub const fn as_ptr(&self) -> *const T {
        self as *const [T] as *const T
    }

    pub fn as_mut_ptr(&self) -> *mut T {
        self as *mut [T] as *mut T
    }

    pub fn iter(&self) -> Iter<T> {
        Iter(
            unsafe { RawSliceIter::new(NonNull::from(self)) },
            PhantomData,
        )
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut(
            unsafe { RawSliceIter::new(NonNull::from(self)) },
            PhantomData,
        )
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

#[unstable(feature = "lccc_raw_slice_iter")]
pub struct RawSliceIter<T> {
    base: core::ptr::NonNull<T>,
    end_or_len: *const T,
}

impl<T> RawSliceIter<T> {
    pub unsafe fn new(ptr: core::ptr::NonNull<[T]>) -> Self {
        if const { core::mem::size_of::<T>() == 0 } {
            Self {
                base: ptr.cast(),
                end_or_len: core::ptr::without_provenance(ptr.len()),
            }
        } else {
            let begin = ptr.cast();
            let end = unsafe { begin.add(ptr.len()) };

            Self {
                base: begin,
                end_or_len: end.as_ptr(),
            }
        }
    }

    pub fn current(&self) -> core::ptr::NonNull<T> {
        self.base
    }

    pub fn current_back(&self) -> core::ptr::NonNull<T> {
        if const { core::mem::size_of::<T>() == 0 } {
            self.base
        } else {
            unsafe { core::ptr::NonNull::new_unchecked(self.end_or_len) }
        }
    }
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
unsafe impl<T> TrustedLen for RawSliceIter<T> {}

impl<T> ExactSizeIterator for RawSliceIter<T> {
    fn len(&self) -> usize {
        if const { core::mem::size_of::<T>() == 0 } {
            self.end_or_len.addr()
        } else {
            unsafe { self.end_or_len.offset_from(self.base.as_ptr()) }
        }
    }
}

impl<T> DoubleEndedIterator for RawSliceIter<T> {
    fn next_back(&mut self) -> Option<core::ptr::NonNull<T>> {
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
                let val = self.end_or_len;
                self.end_or_len = unsafe { val.sub(1) };
                Some(unsafe { core::ptr::NonNull::new_unchecked(val) })
            }
        }
    }

    fn advance_back_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
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
                self.end_or_len = unsafe { self.end_or_len.sub(n) };
                Ok(())
            }
        }
    }
}

pub struct Iter<'a, T>(RawSliceIter<T>, PhantomData<&'a [T]>);

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        self.0.next().map(unsafe { |nn| nn.as_ref() })
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        self.0.advance_by(n)
    }

    fn count(self) -> usize {
        self.0.count()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<&'a T> {
        self.0.next_back().map(unsafe { |nn| nn.as_ref() })
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        self.0.advance_back_by(n)
    }
}

impl<'a, T> FusedIterator for Iter<'a, T> {}
unsafe impl<'a, T> TrustedLen for Iter<'a, T> {}

impl<'a, T> ExactSizeIterator for Iter<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

pub struct IterMut<'a, T>(RawSliceIter<T>, PhantomData<&'a mut [T]>);

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<&'a mut T> {
        self.0.next().map(unsafe { |mut nn| nn.as_mut_ref() })
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        self.0.advance_by(n)
    }

    fn count(self) -> usize {
        self.0.count()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, T> DoubleEndedIterator for IterMut<'a, T> {
    fn next_back(&mut self) -> Option<&'a T> {
        self.0.next_back().map(unsafe { |mut nn| nn.as_mut_ref() })
    }

    fn advance_by(&mut self, n: usize) -> Result<(), core::num::NonZero<usize>> {
        self.0.advance_back_by(n)
    }
}

impl<'a, T> FusedIterator for IterMut<'a, T> {}
unsafe impl<'a, T> TrustedLen for IterMut<'a, T> {}

impl<'a, T> ExactSizeIterator for IterMut<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
