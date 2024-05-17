/**
 * rust/libcore/mem.rs
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
use crate::default::Default;
use crate::intrinsics;
use crate::Sized;

pub unsafe fn zeroed<T>() -> T {
    ::__lccc::builtins::rust::panic_if_uninhabited::<T>();
    ::__lccc::builtins::rust::zeroed()
}

#[repr(transparent)]
pub union MaybeUninit<T> {
    valid: ManuallyDrop<T>,
    uninit: (),
}

impl<T> MaybeUninit<T> {
    pub const fn new(x: T) -> Self {
        Self {
            valid: ManuallyDrop::new(x),
        }
    }
    pub const fn uninit() -> Self {
        Self { uninit: () }
    }
    pub const fn zeroed() -> Self {
        unsafe { ::__lccc::builtins::rust::zeroed() }
    }
    pub unsafe fn assume_init(self) -> T {
        self.valid.into_inner()
    }
    #[unstable(feature = "maybe_uninit_uninit_array")]
    pub fn uninit_array<const LEN: usize>() -> [Self; LEN] {
        unsafe { MaybeUninit::uninit().assume_init() }
    }
    #[unstable(feature = "internal_uninit_const")]
    pub const UNINIT: Self = Self::uninit();

    pub fn write(&mut self, val: T) -> &mut T {
        unsafe {
            core::ptr::write(self.as_mut_ptr(), val);
            self.get_mut()
        }
    }

    pub fn as_ptr(&self) -> *const T {
        self as *const Self as *const T
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self as *mut Self as *mut T
    }

    pub unsafe fn read(&self) -> T {
        core::ptr::read(self.as_ptr())
    }

    pub unsafe fn get_ref(&self) -> &T {
        &*self.as_ptr()
    }
    pub unsafe fn get_mut(&mut self) -> &mut T {
        &mut *self.as_mut_ptr()
    }

    #[unstable(feature = "maybe_uninit_slice_assume_init")]
    pub unsafe fn slice_get_ref(slice: &[Self]) -> &[T] {
        core::mem::transmute(slice)
    }

    #[unstable(feature = "maybe_uninit_slice_assume_init")]
    pub unsafe fn slice_get_mut(slice: &mut [Self]) -> &mut [T] {
        core::mem::transmute(slice)
    }

    #[unstable(feature = "maybe_uninit_slice", issue = "63569")]
    pub fn first_ptr(slice: &[Self]) -> *const T {
        slice.as_ptr() as *const T
    }

    #[unstable(feature = "maybe_uninit_slice", issue = "63569")]
    pub fn first_ptr_mut(slice: &mut [Self]) -> *mut T {
        slice.as_mut_ptr() as *mut T
    }
}

unsafe impl<T> TrivialDestruction for MaybeUninit<T> {}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ManuallyDrop<T: ?Sized> {
    inner: __lccc::trivial<T>,
}

impl<T> ManuallyDrop<T> {
    pub const fn new(value: T) -> ManuallyDrop<T> {
        // SAFETY:
        // convert weak from T to trivial<T> is always defined.
        // The result cannot be an invalid value of trivial<T> because  it was a valid value of T.
        ManuallyDrop {
            inner: unsafe { __lccc::xir!("convert weak":[value]:[yield:__lccc::trivial::<T>]) },
        }
    }
    pub const fn into_inner(slot: ManuallyDrop<T>) -> T {
        // SAFETY:
        // convert weak from trivial<T> to T is always defined.
        // The result cannot be an invalid value of T because it was a valid value of trivial<T>.
        unsafe { __lccc::xir!("convert weak":[slot.inner]:[yield: T]) }
    }
    #[must_use = "if you don't need the value, use `ManuallyDrop::drop` instead"]
    pub unsafe fn take(slot: &mut ManuallyDrop<T>) -> T {
        core::ptr::read(slot as *mut _ as *mut T)
    }
}

impl<T: ?Sized> ManuallyDrop<T> {
    pub unsafe fn drop(slot: &mut ManuallyDrop<T>) {
        crate::ptr::drop_in_place(slot as *mut _ as *mut T)
    }
}

pub fn forget<T>(x: T) {
    ManuallyDrop::new(x);
}

pub fn take<T: Default>(r: &mut T) -> T {
    replace(r, Default::default())
}

pub fn replace<T>(r: &mut T, val: T) -> T {
    unsafe {
        let ret = ptr::read(r);
        ptr::write(r, val);
        ret
    }
}

unsafe impl<T: ?Sized> TrivialDestruction for ManuallyDrop<T> {}

#[deprecated("use MaybeUninit instead")]
pub unsafe fn uninitialized<T>() -> T {
    MaybeUninit::uninit().assume_init()
}

#[__lccc::allow_constant_promotion]
pub const fn size_of<T>() -> usize {
    intrinsics::__builtin_size_of::<T>()
}

#[__lccc::allow_constant_promotion]
pub const fn align_of<T>() -> usize {
    intrinsics::__builtin_align_of::<T>()
}

pub const fn size_of_val<T: ?Sized>(x: &T) -> usize {
    intrinsics::__builtin_size_of_val(x)
}
