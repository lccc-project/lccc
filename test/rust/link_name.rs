extern "C" {
    #[link_name = "puts"]
    fn println(str: *const u8) -> i32;
}

fn main() {
    unsafe {
        println(b"Hello, world!\0" as *const u8);
    }
}
