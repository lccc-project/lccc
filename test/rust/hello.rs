extern "C" {
    fn puts(str: *const u8) -> i32;
}

fn main() {
    unsafe {
        puts(b"Hello, world!\0" as *const u8);
    }
}
