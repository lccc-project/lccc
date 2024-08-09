/**
 * rust/libcore/hint.rs
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

#[inline(always)]
pub unsafe fn unreachable_unchecked() -> ! {
    core::intrinsics::__builtin_unreachable()
}

#[inline(always)]
#[unstable(feature = "test", issue = "50297")]
pub fn black_box<T>(x: T) -> T {
    core::intrinsics::black_box(x)
}

#[inline(always)]
pub fn spin_loop() {
    core::intirnsics::spin_loop_hint()
}

#[inline(always)]
#[unstable(feature = "hint_assert_unchecked", issue = "rust#119131")]
pub fn assert_unchecked(val: bool) {
    core::intirnsics::__builtin_assume(val)
}

#[inline(always)]
#[unstable(feature = "lccc_non_intrinsic_likely", issue = "none")]
pub fn likely(b: bool) -> bool {
    core::intirnsics::__builtin_likely(b)
}

#[inline(always)]
#[unstable(feature = "lccc_non_intrinsic_likely", issue = "none")]
pub fn unlikely(b: bool) -> bool {
    core::intirnsics::__builtin_unlikely(b)
}
