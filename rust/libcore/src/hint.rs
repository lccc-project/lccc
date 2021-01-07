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
