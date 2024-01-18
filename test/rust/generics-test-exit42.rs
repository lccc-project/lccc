extern "rust-intrinsic" {
    fn __builtin_size_of<T>() -> usize;
}

extern "C" {
    fn exit(code: i32) -> !;
}

#[repr(packed(2))]
struct Foo {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
    f: u16,
}

fn main() -> ! {
    unsafe { exit(__builtin_size_of::<Foo>() as i32) }
}
