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
    strong_count: Cell<usize>,
    weak_count: Cell<usize>,
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

    pub fn pin(x: T) -> Rc<T> {
        pin_in(x, Global)
    }
}

impl<T, A: Allocator> Rc<T, A> {
    #[unstable(feature = "allocator_api", issue = "32838")]
    pub fn new_in(x: T, alloc: A) -> Self {}
}
