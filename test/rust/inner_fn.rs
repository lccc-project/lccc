extern "C" {
    fn puts(c: *const u8) -> i32;
}

fn main() {
    fn foo() {
        unsafe { puts(b"Hello, World!" as *const u8) }
    }
    foo()
}
