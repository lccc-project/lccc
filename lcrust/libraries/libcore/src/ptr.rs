/**
 * rust/libcore/ptr.rs
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
use crate::clone::Clone;
use crate::convert::From;
use crate::marker::PhantomData;
use crate::mem::MaybeUninit;
use crate::Option::{None, Some};
use crate::{Copy, Option, Sized};

#[lang = "drop_in_place"]
#[inline(always)]
pub unsafe fn drop_in_place<T: ?Sized>(x: *mut T) {
    ::__lccc::__maybe_adl__::__evaluate_destructor_at(x)
}

#[lang = "const_ptr"]
#[__lccc::mangle_as("std::ptr::const_ptr")]
impl<T: ?Sized> *const T {
    pub const fn is_null(self) -> bool {
        (::__lccc::xir!("convert reinterpret *uint(8)":[self: *const T]:[yield: *const u8]))
            == unsafe { crate::mem::zeroed() }
    }
}

#[lang = "mut_ptr"]
#[__lccc::mangle_as("std::ptr::mut_ptr")]
impl<T: ?Sized> *mut T {
    pub const fn is_null(self) -> bool {
        (::__lccc::xir!("convert reinterpret *uint(8)":[self: *const T]:[yield: *const u8]))
            == unsafe { crate::mem::zeroed() }
    }
}

impl<T> *mut [T] {
    #[unstable(feature = "slice_ptr_len", issue = "71146")]
    pub fn len(self) -> usize {
        let raw: crate::slice::RawSlice<T> = crate::mem::transmute(self);
        raw.len
    }

    #[unstable(feature = "slice_ptr_get", issue = "74265")]
    pub fn as_mut_ptr(self) -> *mut T {
        let raw: crate::slice::RawSlice<T> = crate::mem::transmute(self);
        raw.ptr as *mut T
    }
}

impl<T> *const [T] {
    #[unstable(feature = "slice_ptr_len", issue = "71146")]
    pub fn len(self) -> usize {
        let raw: crate::slice::RawSlice<T> = crate::mem::transmute(self);
        raw.len
    }

    #[unstable(feature = "slice_ptr_get", issue = "74265")]
    pub fn as_ptr(self) -> *const T {
        let raw: crate::slice::RawSlice<T> = crate::mem::transmute(self);
        raw.ptr as *const T
    }
}

pub fn slice_from_raw_parts(ptr: *const T, len: usize) -> *const [T] {
    crate::mem::transmute(crate::slice::RawSlice::<T> {
        ptr: ptr as *mut T,
        len,
    })
}

pub fn slice_from_raw_parts_mut(ptr: *mut T, len: usize) -> *mut [T] {
    crate::mem::transmute(crate::slice::RawSlice::<T> { ptr, len })
}

#[repr(transparent)]
pub struct NonNull<T: ?Sized> {
    #[lcrust::nonnull_pointer]
    ptr: *const T,
}

impl<T: ?Sized> Clone for NonNull<T> {
    fn clone(&self) -> Self {
        NonNull { ptr: self.ptr }
    }
}

impl<T: ?Sized> Copy for NonNull<T> {}

impl<T> NonNull<T> {
    pub const fn dangling() -> Self {
        core::alloc::Layout::of::<T>().dangling().cast()
    }
}

impl<T: ?Sized> NonNull<T> {
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Self {
        NonNull { ptr }
    }
    pub fn new(ptr: *mut T) -> Option<Self> {
        if ptr.is_null() {
            Some(NonNull { ptr })
        } else {
            None
        }
    }
    pub fn as_ptr(self) -> *mut T {
        self.ptr as *mut _
    }

    pub unsafe fn as_ref(&self) -> &T {
        &*self.ptr
    }

    pub unsafe fn as_mut(&mut self) -> &mut T {
        &mut *(self.ptr as *mut _)
    }

    pub const fn cast<U>(self) -> NonNull<U> {
        NonNull {
            ptr: self.ptr as *const U,
        }
    }
}

impl<T> NonNull<[T]> {
    pub const fn len(self) -> usize {
        self.ptr.len()
    }
}

impl<T: ?Sized> From<&T> for NonNull<T> {
    fn from(t: &T) -> Self {
        NonNull { ptr: t }
    }
}

impl<T: ?Sized> From<&mut T> for NonNull<T> {
    fn from(t: &mut T) -> Self {
        NonNull {
            ptr: t as *mut T as *const T,
        }
    }
}

#[repr(transparent)]
#[unstable(feature = "lccc_unique_ptr")]
pub struct Unique<T: ?Sized> {
    #[__lccc::xlang_pointer_attributes(aligned)]
    ptr: NonNull<T>,
    data: PhantomData<T>,
}

unsafe impl<T: ?Sized + Send> Send for Unique<T> {}
unsafe impl<T: ?Sized + Sync> Sync for Unique<T> {}

impl<T> Unique<T> {
    pub const fn dangling() -> Self {
        Self {
            ptr: NonNull::dangling(),
            data: PhantomData,
        }
    }
}

impl<T: ?Sized> Unique<T> {
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Unique<T> {
        Self {
            ptr: NonNull::new_unchecked(ptr),
            data: PhantomData,
        }
    }
    pub unsafe fn new_unchecked_nullable(ptr: *mut T) -> Option<Unique<T>> {
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            data: PhantomData,
        })
    }

    pub fn as_ptr(&self) -> *const T {
        unsafe { ::__lccc::builtins::rust::kill_mutation(self.ptr.as_ptr() as *const T) }
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }

    pub unsafe fn as_ref(&self) -> &T {
        &*self.as_ptr()
    }

    pub unsafe fn as_mut(&mut self) -> &mut T {
        &*self.as_mut()
    }

    pub fn as_non_null_mut(&mut self) -> NonNull<T> {
        self.ptr
    }

    pub fn into_inner(self) -> NonNull<Self> {
        self.ptr
    }

    pub fn cast<U>(self) -> Unique<U> {
        Unique {
            ptr: self.ptr.cast::<U>(),
            data: PhantomData,
        }
    }
}

pub unsafe fn swap<T>(x: *mut T, y: *mut T) {
    ::__lccc::xir!("swap":[ref *x, ref *y]);
}

pub use crate::intrinsics::copy;
pub use crate::intrinsics::copy_nonoverlapping;
pub use crate::intrinsics::read;
pub use crate::intrinsics::swap_nonoverlapping;
pub use crate::intrinsics::write;

#[allow_internal_unstable(raw_ref)]
pub macro addr_of($e:expr){
    (&raw e)
}

#[allow_internal_unstable(raw_ref)]
pub macro addr_of_mut($e:expr) {
    (&raw mut e)
}
