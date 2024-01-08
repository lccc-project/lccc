extern "C" {
    fn exit(status: i32) -> !;
}

fn main() -> ! {
    unsafe { exit(42) }
}
