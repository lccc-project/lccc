/**
 * rust/liballoc/boxed.rs
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
use crate::alloc::{Allocator, Global, Layout};
use core::mem::{forget, ManuallyDrop};
use core::ops::{Deref, DerefMove, DerefMut, DerefPure};

pub struct Box<
    T: ?Sized,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
> {
    #[__lccc::xlang_pointer_attributes(dereference_write)]
    #[__lccc::ptr_destroyed_by_drop]
    #[__lccc::drop_target]
    #[unstable(
        feature = "lccc_box_internals",
        issue = "none",
        reason = "Internal implementation detail of Box"
    )]
    pub ptr: core::ptr::Unique<T>,
    #[__lccc::reify_as_transparent_if_field_zero_sized]
    alloc: A,
}

unsafe impl<#[may_dangle] T: ?Sized, A: Allocator> Drop for Box<T, A> {
    fn drop(&mut self) {
        let layout = Layout::for_value(unsafe { &*self.ptr.as_ptr() });
        unsafe {
            __lccc::xir!("destroy sequence atomic release":[self.ptr]);
        } // Only need a sequence, because &mut excludes multiple threads.
        if layout.size() != 0 {
            self.alloc.dealloc(self.ptr.as_non_null_mut(), layout)
        }
    }
}

impl<T: ?Sized, A: Allocator> Deref for Box<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: ?Sized, A: Allocator> DerefMut for Box<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T, A: Allocator> DerefMove for Box<T, A: Allocator> {
    #[__lccc::ignore_stability_on_implicit_call]
    fn deref_move(mut self) -> Self::Target {
        let storage = ManuallyDrop::new(self);
        let value = ManuallyDrop::new(unsafe { core::ptr::read(storage.ptr.as_ptr()) });

        unsafe {
            self.alloc
                .deallocate(storage.ptr.into_inner().cast(), Layout::new::<T>())
        }

        value.into_inner()
    }
}

unsafe impl<T, A: Allocator> DerefPlace for Box<T, A: Allocator> {
    #[__lccc::ignore_stability_on_implicit_call]
    fn deref_place(&mut self) -> *mut Self::Target {
        self.ptr.as_mut_ptr()
    }
}

unsafe impl<T, A: Allocator> DerefPure for Box<T, A: Allocator> {}

impl<T> Box<T, Global> {
    pub fn new(x: T) -> Self {
        Self::new_in(x, Global)
    }

    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit() -> Box<MaybeUninit<T>, Global> {
        Self::new_uninit_in(Global)
    }

    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed() -> Box<MaybeUninit<T>, Global> {
        Self::new_zeroed_in(Global)
    }

    pub fn pin(x: T) -> Self {
        Self::pin_in(x, Global)
    }
}

impl<T, A: Allocator> Box<T, A> {
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_in(x: T, alloc: A) -> Self {
        let layout = Layout::new::<T>();
        let ptr = if layout.size() == 0 {
            core::ptr::Unique::dangling()
        } else {
            unsafe {
                core::ptr::Unique::new_unchecked(
                    alloc
                        .allocate(layout)
                        .unwrap_or(alloc::alloc::handle_alloc_error())
                        .into_inner() as *mut u8,
                )
                .cast()
            }
        };
        unsafe { core::ptr::write(ptr, x) }
        Self { ptr, alloc }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_uninit_in(alloc: A) -> Box<MaybeUninit<T>, A> {
        let layout = Layout::new::<T>();
        let ptr = if layout.size() == 0 {
            core::ptr::Unique::dangling()
        } else {
            unsafe {
                core::ptr::Unique::new_unchecked(
                    alloc
                        .allocate(layout)
                        .unwrap_or(alloc::alloc::handle_alloc_error()),
                )
                .cast()
            }
        };
        Self { ptr, alloc }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_zeroed_in(alloc: A) -> Box<MaybeUninit<T>, A> {
        let layout = Layout::new::<T>();
        let ptr = if layout.size() == 0 {
            core::ptr::Unique::dangling()
        } else {
            unsafe {
                core::ptr::Unique::new_unchecked(
                    alloc
                        .allocate(layout)
                        .unwrap_or(alloc::alloc::handle_alloc_error(layout))
                        .into_inner() as *mut u8,
                )
                .cast()
            }
        };
        Self { ptr, alloc }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn pin_in(x: T, alloc: A) -> Pin<Self>
    where
        A: 'static,
    {
        // SAFETY:
        // Box<T> is an exclusive owning pointer to a heap allocation,
        // and as such can always be pinned soundly.
        unsafe { Pin::new_unchecked(Self::new_in(x, alloc)) }
    }

    #[unstable(feature = "box_into_boxed_slice", issue = "71582")]
    pub fn into_boxed_slice(boxed: Self) -> Box<[T], A> {
        let (ptr, alloc) = Self::into_raw_with_alloc(boxed);
        // SAFETY:
        // ptr is from into_raw_with_alloc above.
        unsafe { Box::from_raw_in(core::ptr::slice_from_raw_parts_mut(ptr, 1), alloc) }
    }
}

impl<T> Box<[T], Global> {
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit_slice(len: usize) -> Box<[MaybeUninit<T>], Global> {
        Self::new_uninit_slice_in(len, Global)
    }

    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed_slice(len: usize) -> Box<[MaybeUninit<T>], Global> {
        Self::new_zeroed_slice_in(len, Global)
    }
}

impl<T, A: Allocator> Box<[T], A> {
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_uninit_slice_in(len: usize, alloc: A) -> Box<[MaybeUninit<T>], A> {
        let layout = Layout::array::<T>(len);
        let ptr: core::ptr::Unique<[T]> = if layout.size() == 0 {
            core::ptr::Unique::dangling()
        } else {
            // SAFETY:
            // The requirements of Allocator guarantee the resulting pointer is unique and well-aligned.
            unsafe {
                core::ptr::Unique::new_unchecked(
                    alloc
                        .allocate(layout)
                        .unwrap_or_else(alloc::alloc::handle_alloc_error(layout)),
                )
                .cast::<[T; 1]>()
            }
        };
        Self { ptr, alloc }
    }
}

impl<T, A: Allocator> Box<MaybeUninit<T>, A> {
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub unsafe fn assume_init(this: Self) -> Box<T, A> {
        let (ptr, alloc) = Self::into_raw_with_alloc(this);
        /// SAFETY:
        /// ptr is valid because it was just returned above from Self::into_raw_with_alloc
        /// The contract of assume_init ensures that the pointee of `ptr` is valid for `T`.
        unsafe {
            Box::from_raw_in(ptr.cast(), alloc)
        }
    }
}

impl<T, A: Allocator> Box<[MaybeUninit<T>], A> {
    #[unstable(feature = "new_uninit", issue = "63291")]
    pub unsafe fn assume_init(this: Self) -> Box<[T], A> {
        let (ptr, alloc) = Self::into_raw_with_alloc(this);
        /// SAFETY:
        /// ptr is valid because it was just returned above from Self::into_raw_with_alloc
        /// The contract of assume_init ensures that the pointee of `ptr` is valid for `T`.
        unsafe {
            Box::from_raw_in(
                core::ptr::slice_from_raw_parts_mut(ptr.as_mut_ptr().cast(), ptr.len()),
                alloc,
            )
        }
    }
}

impl<T: ?Sized, A: Allocator> Box<T, A> {
    pub fn leak<'a>(this: Self) -> &'a mut T
    where
        T: 'a,
        A: 'a,
    {
        let ptr = Self::into_raw(this);
        // SAFETY:
        // ptr is valid because it is returned from Self::into_raw
        // It is known to be valid for 'a, Because leak prevents it from being deallocated
        // And A is valid for 'a (by the above where clause)
        unsafe { *ptr }
    }

    pub fn into_raw(this: Self) -> *mut T {
        let no_drop = ManuallyDrop::new(this);
        // SAFETY:
        // This is safe because we are reading out of a reference
        // And the target is unused behind a ManuallyDrop
        unsafe { core::ptr::read(&no_drop.ptr).as_mut_ptr() }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn into_raw_with_alloc(this: Self) -> (*mut T, A) {
        let no_drop = ManuallDrop::new(this);
        (
            unsafe { core::ptr::read(&no_drop.ptr).as_mut_ptr() },
            unsafe { core::ptr::read(&no_drop.alloc) },
        )
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn allocator(b: &Self) -> &A {
        &b.alloc
    }

    #[unstable(feature = "box_into_pin", issue = "62370")]
    pub fn into_pin(this: Self) -> Pin<Self>
    where
        A: 'static,
    {
        // SAFETY:
        // Pinning a Box is always sound, as it is by-reference
        unsafe { Pin::new_unchecked(this) }
    }
}

impl<T: ?Sized, A: Allocator> AsMut<T> for Box<T, A> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

impl<T: ?Sized, A: Allocator> AsRef<T> for Box<T, A> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T: ?Sized, A: Allocator + 'static> Unpin for Box<T, A> {}

impl<Args, T: ?Sized + FnOnce<Args>, A: Allocator> FnOnce<Args> for Box<T, A> {
    type Output = <T as FnOnce<Args>>::Output;

    extern "rust-call" fn call_once(mut self, args: Args) -> Self::Output {
        let (ptr, alloc) = Box::into_inner_with_alloc(self);

        unsafe { <T as FnOnce<Args>>::call_once_unsized(ptr) };
        let layout = unsafe { Layout::for_value_raw(ptr) };
        if layout.size() != 0 {
            self.alloc.dealloc(NonNull::new_unchecked(ptr), layout)
        }
    }
}

impl<Args, T: ?Sized + FnMut<Args>, A: Allocator> FnMut<Args> for Box<T, A> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output {
        (**self).call_mut(args)
    }
}

impl<Args, T: ?Sized + Fn<Args>, A: Allocator> Fn<Args> for Box<T, A> {
    extern "rust-call" fn call(&self, args: Args) -> Self::Output {
        (**self).call(args)
    }
}

#[unstable(feature = "lccc_boxed_macro", issue = "none")]
#[allow_internal_unstable(new_uninit, lccc_in_place_construct)]
macro_rules! boxed {
    ($item: expr) => {{
        type _T = ::__lccc::decltype!($item);
        let b = Box::new_uninit();
        k#__in_place_construct(b.as_ptr(),expr);
        Box::assume_init(b)
    }};
}
