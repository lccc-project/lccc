use std::env;

fn main() {
    println!(
        "cargo:rustc-env=CARGO_IN_USE={}",
        env::var("CARGO").unwrap()
    );
}
