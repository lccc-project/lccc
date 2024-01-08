extern "rust-intrinsic" {
    pub fn __builtin_abort() -> !;
}

fn main() -> ! {
    __builtin_abort()
}
