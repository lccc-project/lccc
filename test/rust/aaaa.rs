extern "C" {
    fn putchar(x: i32) -> i32;
    fn puts(c: *mut u8) -> i32;
}

fn uh_oh(x: i32) {
    unsafe {
        putchar(x);
        putchar(x);
        putchar(x);
        putchar(x);
        puts(b"\0" as *mut u8);
    }
}

fn main() {
    let x = b'A' as _;
    uh_oh(x);
}
