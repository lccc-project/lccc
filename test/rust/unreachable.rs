extern "rust-intrinsic" {
    fn __builtin_unreachable() -> !;
}

fn main() {
    unsafe { __builtin_unreachable() }
}
