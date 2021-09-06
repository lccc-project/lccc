use std::path::PathBuf;

use target_tuples::Target;

fn main() {
    println!("cargo:rerun-if-env-changed=CC");
    println!("cargo:rerun-if-env-changed=LCRUST");
    println!("cargo:rerun-if-env-change=RUSTC");

    let target = Target::parse(&std::env::var("TARGET").unwrap());
    let host = Target::parse(&std::env::var("HOST").unwrap());
    let cc = std::env::var_os("CC").map(PathBuf::from);

    let cc = match cc {
        Some(p) => p,
        None => which::which("lccc")
            .or_else(|e| {
                if target == host {
                    which::which("cc")
                } else {
                    Err(e)
                }
            })
            .or_else(|_| which::which(String::from(target.get_name()) + "-cc"))
            .unwrap(),
    };

    let tmpdir = std::env::var_os("OUT_DIR")
        .map(PathBuf::from)
        .map(|mut e| {
            e.push("tmp");
            e
        })
        .unwrap();
}

fn try_thread_checks(cc: &Path, tmpdir: &Path) {}
