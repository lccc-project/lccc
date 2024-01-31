use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let rustc = std::env::var_os("RUSTC").unwrap();
    let rustflags = std::env::var("CARGO_ENCODED_RUSTFLAGS")
        .ok()
        .unwrap_or_else(String::new);

    let flags = rustflags.split('\u{1f}').collect::<Vec<&str>>();

    let path = std::env::var_os("OUT_DIR").unwrap();

    let path = PathBuf::from(path);

    let test_file_path = {
        let mut path = path.clone();

        path.push("test.rs");
        path
    };

    let dummy_output_path = {
        let mut path = path.clone();

        path.push("test.out");
        path
    };

    std::fs::write(&test_file_path, "pub extern \"C-unwind\" fn foo(){}").unwrap();

    let mut has_feature_c_unwind = None;

    if std::process::Command::new(&rustc)
        .args(&flags)
        .args(["--crate-type", "rlib"])
        .args(["--crate-name", "test"])
        .arg("--emit")
        .arg(format!("metadata={}", dummy_output_path.display()))
        .arg(&test_file_path)
        .status()
        .unwrap()
        .success()
    {
        has_feature_c_unwind = Some("stable");
    } else {
        std::fs::write(
            &test_file_path,
            "#![feature(c_unwind)] pub extern \"C-unwind\" fn foo(){}",
        )
        .unwrap();
        if std::process::Command::new(&rustc)
            .args(&flags)
            .args(["--crate-type", "rlib"])
            .args(["--crate-name", "test"])
            .arg("--emit")
            .arg(format!("metadata={}", dummy_output_path.display()))
            .arg(&test_file_path)
            .status()
            .unwrap()
            .success()
        {
            has_feature_c_unwind = Some("feature");
        }
    }

    if let Some(has_feature_c_unwind) = has_feature_c_unwind {
        println!(
            "cargo:rustc-cfg=has_feature_c_unwind=\"{}\"",
            has_feature_c_unwind
        );
    }
}
