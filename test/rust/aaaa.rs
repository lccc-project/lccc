extern "C" {
    fn putchar(x: i32) -> i32;
}

fn uh_oh(x: i32) {
    unsafe {
        putchar(x);
        putchar(x);
        putchar(x);
        putchar(x);
    }
}

fn main() {
    let x = b'A' as _;
    uh_oh(x);
}
