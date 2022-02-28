extern "C" {
    fn puts(str: *const u8) -> i32;
}

struct Holder {
    x: *const u8,
}

fn main() {
    let holder = Holder { x: b"Hello, world!" as *const u8 };
    unsafe {
        puts(holder.x);
    }
}
