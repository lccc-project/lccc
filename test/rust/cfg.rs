extern "C" {
    pub fn exit(code: i32) -> !;
}

#[cfg(unix)]
pub fn main() -> ! {
    exit(0)
}

#[cfg(not(unix))]
pub fn main() -> ! {
    exit(1)
}
