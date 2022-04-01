extern "C" {
    fn puts(str: *const u8) -> i32;
}
fn main() {
    let x = [b"Hello World!\0"];
    unsafe {
        puts(x[0] as *const u8);
    }
}
