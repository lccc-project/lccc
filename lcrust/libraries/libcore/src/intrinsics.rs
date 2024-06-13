/*!
 * rust/libcore/intrsinics.rs
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

#![unstable(feature = "core_intrinsics", issue = "none")]

extern "rust-intrinsics" {
    pub unsafe fn transmute<T, U>(x: T) -> U;
    pub unsafe fn construct_in_place<T, F: FnOnce<Args> + FnPointer, Args: Tuple>(
        dest: *mut T,
        func: F,
        args: Args,
    );
    pub fn __builtin_cmp<T: core::marker::Copy + core::cmp::PartialOrd, U>(a: T, b: T) -> U;

    /// Same as [`Ord::max`][core::cmp::Ord::max] but only on primitives, and generally more efficient.
    /// Also functions on `f32` and `f64`, see the corresponding function on those types for behaviour surrounding NaNs.
    /// This may avoid a branch that may be present on the default function
    pub fn __builtin_max_val<T: core::marker::Copy + core::cmp::PartialOrd>(a: T, b: T) -> T;
    /// Same as [`Ord::min`][core::cmp::Ord::min] but only on primitives, and generally more efficient.
    /// Also functions on `f32` and `f64`, see the corresponding function on those types for behaviour surrounding NaNs.
    /// This may avoid a branch that may be present on the default function
    pub fn __builtin_min_val<T: core::marker::Copy + core::cmp::PartialOrd>(a: T, b: T) -> T;
    /// Same as [`Ord::clamp`][core::cmp::Ord::clamp] but only on primitives, and generally more efficient.
    /// Also functions on `f32` and `f64`, see the corresponding function on those types for behaviour surrounding NaNs.
    /// This may avoid a branch that may be present on the default function.
    /// 
    /// Note: This function may have unpredictable results, but does not have undefined behaviour, if `lower` > `upper`.
    pub fn __builtin_clamp_val<T: core::marker::Copy + core::cmp::PartialOrd>(
        val: T,
        lower: T,
        upper: T,
    ) -> T;

    pub unsafe fn __atomic_load<T: core::marker::Copy, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
    ) -> T;
    pub unsafe fn __atomic_store<T: core::marker::Copy, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val: T,
    );
    pub unsafe fn __atomic_compare_exchange_strong<
        T: core::marker::Copy,
        const Ord: core::sync::atomic::Ordering,
    >(
        dest: *mut T,
        expected: *mut T,
        new: T,
    ) -> bool;
    pub unsafe fn __atomic_compare_exchange_weak<
        T: core::marker::Copy,
        const Ord: core::sync::atomic::Ordering,
    >(
        dest: *mut T,
        expected: *mut T,
        new: T,
    ) -> bool;

    pub fn __atomic_interthread_fence<const Ord: core::sync::atomic::Ordering>();
    pub fn __atomic_intrathread_fence<const Ord: core::sync::atomic::Ordering>();

    pub unsafe fn __atomic_swap<T, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val: T,
    ) -> T;
    pub unsafe fn __atomic_fetch_add<T, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val: T,
    ) -> T;
    pub unsafe fn __atomic_fetch_sub<T, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val: T,
    ) -> T;

    pub unsafe fn __atomic_read_transactional<T, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val_out: *mut T,
    ) -> bool;
    pub unsafe fn __atomic_write_commit<T, const Ord: core::sync::atomic::Ordering>(
        dest: *mut T,
        val: T,
    ) -> i32;
    pub unsafe fn __atomic_abort_transaction<T>(dest: *mut T);
}

#[track_caller]
#[inline]
#[__lccc::mangle_as = "std::instrinics::caller_location"]
#[__lccc::abi_tag = ""] // Disable the abi tag
pub fn caller_location() -> &'static core::panic::Location<'static> {
    core::panic::Location::caller()
}
