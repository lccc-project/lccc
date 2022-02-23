#[cfg(unix)]
pub fn main() {}

#[cfg(not(unix))]
pub fn main() {}
