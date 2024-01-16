extern "rust-intrinsic" {
    fn __builtin_size_of<T>() -> usize;
}

extern "C" {
    fn exit(code: i32) -> !;
}

struct Foo;

fn main() -> ! {
    unsafe { exit(__builtin_size_of::<Foo>() as i32) }
}
