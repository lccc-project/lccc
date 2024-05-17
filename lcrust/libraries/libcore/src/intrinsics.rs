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

extern "rust-intrinsics"{
    pub unsafe fn transmute<T,U>(x: T) -> U;
    pub unsafe fn construct_in_place<T, F: FnOnce<Args> + FnPointer, Args: Tuple>(*mut T, F, Args);
    pub fn __builtin_cmp<T: core::marker::Copy + core::cmp::PartialEq, U>(T, T) -> U;
}

#[track_caller]
#[inline]
#[__lccc::mangle_as = "std::instrinics::caller_location"]
#[__lccc::abi_tag = ""] // Disable the abi tag
pub fn caller_location() -> &'static core::panic::Location<'static> {
    core::panic::Location::caller()
}
