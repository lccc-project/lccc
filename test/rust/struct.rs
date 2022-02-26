extern "C" {
    fn puts(str: *const u8) -> i32;
}

struct Holder {
    x: &[u8],
}

fn main() {
    let holder = Holder { x: b"Hello, world!" };
    puts(holder.x as *const u8);
}
