/**
 * rust/liballoc/rc.rs
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

#[repr(C)]
struct RcControlBlock<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    value: T,
}

pub struct Rc<
    T: ?Sized,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
>(NonNull<RcControlBlock<T>>, A);

pub struct Weak<
    T: ?Sized,
    #[unstable(feature = "allocator_api", issue = "32838")] A: Allocator = Global,
>(NonNull<RcControlBlock<T>>, A);

impl<T: ?Sized, A> !Send for Rc<T, A> {}
impl<T: ?Sized, A> !Sync for Rc<T, A> {}

impl<T> Rc<T> {
    pub fn new(value: T) -> Self {
        new_in(value, Global)
    }

    #[unstable(feature = "arc_new_cyclic", issue = "75861")]
    pub fn new_cyclic(data_fn: impl FnOnce(&Weak<T>) -> T) -> Self {
        new_cyclic_in(data_fn, Global)
    }

    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_uninit() -> Rc<MaybeUninit<T>> {
        new_uninit_in(Global)
    }

    #[unstable(feature = "new_uninit", issue = "63291")]
    pub fn new_zeroed() -> Rc<MaybeUninit<T>> {
        new_zeroed_in(Global)
    }

    pub fn pin(x: T) -> Pin<Rc<T>> {
        pin_in(x, Global)
    }
}

impl<T, A: Allocator> Rc<T, A> {
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_uninit_in(alloc: A) -> Rc<MaybeUninit<T>> {
        if let Ok(p) = alloc.allocate(layout) {
            let r = p.cast::<RcControlBlock<MaybeUninit<T>>>();
            unsafe {
                r.cast::<[usize; 2]>().write([1, 0]);
            }
            Self(r, alloc)
        } else {
            self::alloc::handle_alloc_error()
        }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_zeroed_in(alloc: A) -> Rc<MaybeUninit<T>> {
        if let Ok(p) = alloc.allocate_zeroed(layout) {
            let r = p.cast::<RcControlBlock<MaybeUninit<T>>>();
            unsafe {
                r.cast::<[usize; 2]>().write([1, 0]);
            }
            Self(r, alloc)
        } else {
            self::alloc::handle_alloc_error()
        }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_in(x: T, alloc: A) -> Self {
        let cb = RcControlBlock {
            strong: Cell::new(1),
            weak: Cell::new(0),
            value: x,
        };
        if let Ok(p) = alloc.allocate(layout) {
            let r = p.cast::<RcControlBlock<T>>();
            unsafe {
                r.as_ptr().write(cb);
            }
            Self(r, alloc)
        } else {
            self::alloc::handle_alloc_error()
        }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn pin_in(x: T, alloc: A) -> Pin<Self> {
        unsafe { Pin::new_unchecked(Self::new_in(x, alloc)) }
    }

    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_cyclic_in<F: FnOnce(&Weak<T>) -> T>(f: F, alloc: Alloc) -> Self {
        if let Ok(p) = alloc.allocate(layout) {
            let r = p.cast::<RcControlBlock<T>>();
            unsafe {
                r.cast::<[usize; 2]>().write([0, 1]);
            }
        } else {
            self::alloc::handle_alloc_error()
        }
    }
}
