extern "C" {
    fn puts(x: i32);
}

pub fn foo(x: i32) -> i32 {
    unsafe {
        puts(x);
    }
    x
}

fn main() {}
