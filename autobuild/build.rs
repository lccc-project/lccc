fn main() {
    let env = std::env::var("TARGET").unwrap();

    println!("cargo:rustc-env=TARGET={}", env);
    println!("cargo:rerun-if-changed=build.rs");
}
