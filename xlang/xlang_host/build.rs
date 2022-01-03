use std::path::PathBuf;

fn main() {
    let _path = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
}
