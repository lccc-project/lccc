extern "C" {
    fn puts(c: *const u8) -> i32;
}

fn foo(x: i32) -> *const u8 {
    match x {
        0 => b"even\0" as *const u8,
        1 => b"odd\0" as *const u8,
        2 => b"even\0" as *const u8,
        3 => b"odd\0" as *const u8,
        _ => b"too large\0" as *const u8,
    }
}

fn main() {
    unsafe { puts(foo()) }
}
