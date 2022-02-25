extern "C" {
    fn puts(str: *const u8) -> i32;
}

fn main() {
    let output = b"Hello, world!\0";
    unsafe {
        puts(output as *const u8);
    }
}
