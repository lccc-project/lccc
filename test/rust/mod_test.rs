mod inline_mod {
    extern "C" {
        pub fn puts(x: *const u8) -> i32;
    }
}

fn main() {
    inline_mod::puts(b"Hello World\0" as *const u8);
}
