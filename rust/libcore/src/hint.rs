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
    ::__lccc::xir!("const undef invalid !"::[yield: !])
}

#[inline(always)]
#[unstable(feature = "test", issue = "50297")]
pub fn black_box<T>(x: T) -> T {
    ::__lccc::xlang::deoptimize(x)
}

#[inline(always)]
#[unstable(feature = "renamed_spin_loop", issue = "55002")]
pub fn spin_loop() {
    crate::sync::atomic::spin_loop_hint()
}

#[inline(always)]
#[unstable(feature = "lccc_non_intrinsic_assume", issue = "none")]
pub fn assume(#[__lccc::xlang_scalar_attributes(nonzero)] val: bool) {
    ::__lccc::builtins::rust::__builtin_assume(val)
}
