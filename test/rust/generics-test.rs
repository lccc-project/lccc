extern "rust-intrinsic" {
    fn __builtin_read_freeze<T>(x: *const T) -> T;
}

extern "C" {
    fn exit(x: i32) -> !;
}

#[repr(transparent)]
union MaybeUninit<T> {
    init: T,
    uninit: (),
}

fn main() {
    let x = MaybeUninit { uninit: () };
    unsafe { exit(__builtin_read_freeze(&x.init)) }
}
