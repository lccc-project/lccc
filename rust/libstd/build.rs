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

    if let Some(_) = std::env::var("CARGO_CFG_WINDOWS") {
        println!("cargo:rustc-cfg=thread_impl=\"windows\"");
    } else if Some("phantomos") == std::env::var("CARGO_CFG_TARGET_OS") {
        println!("cargo:rustc-cfg=thread_impl=\"phantomos\"");
    } else {
        let pthread_test_c = {
            let mut path = tmpdir.clone();
            path.push("pthread-test.c");
            path
        };
        let pthread_test_bin = {
            let mut path = tmpdir.clone();
            path.push("pthread-test");
            path
        };
        std::fs::write(
            &pthread_test_c,
            r"##
#include <pthread.h>

void new_thread(void* v){}

int main(void){
    pthread_t pthread;
    pthread_create(&pthread,0,new_thread,0);
}
  
##",
        )
        .unwrap();
        match std::process::Command::new(cc)
            .arg("-o")
            .arg(pthread_test_bin)
            .arg(pthread_test_c)
            .arg("-pthread")
            .status()
        {
            Ok(s) if s.success() => {
                println!("cargo:rustc-cfg=thread_impl=\"pthread\"");
                println!("cargo:rustc-link-lib=pthread");
                return;
            }
            _ => {}
        };
    }
}
