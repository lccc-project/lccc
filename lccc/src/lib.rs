//! Helper library for the lccc driver, and other driver clis (such as lcrust)
//!

use std::{
    borrow::Borrow,
    hash::{BuildHasher, Hash},
    option::Option as StdOption,
    path::{Path, PathBuf},
};

use target_tuples::Target;
use xlang::{
    abi::{alloc::Allocator, string::StringView},
    host::rustcall,
    plugin::{XLangCodegen, XLangFrontend, XLangPlugin},
    prelude::v1::*,
};

pub static FRONTENDS: [&str; 3] = ["c", "rust", "xir"];
pub static CODEGENS: [&str; 1] = ["x86"];

pub static OPTIMIZERS: [&[&str]; 3] = [&[], &[], &[]];

pub type FrontendInit = rustcall!(extern "rustcall" fn() -> DynBox<dyn XLangFrontend>);
pub type CodegenInit = rustcall!(extern "rustcall" fn() -> DynBox<dyn XLangCodegen>);
pub type PluginInit = rustcall!(extern "rustcall" fn() -> DynBox<dyn XLangPlugin>);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    Preprocess,
    TypeCheck,
    Xir,
    Asm,
    CompileOnly,
    Link,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LinkOutput {
    Shared,
    Static,
    Executable,
    Pie,
    Manifest,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptimizeLevel {
    Integer(u8),
    Debug,
    Size,
    Zize,
    Fast,
    Extra,
}

pub fn find_libraries<K: Borrow<str> + Eq + Hash, V: AsRef<Path>, H: BuildHasher, A: Allocator>(
    search_paths: &[PathBuf],
    names: &[&str],
    prefix: StdOption<&str>,
    overrides: &HashMap<K, V, H, A>,
) -> Vec<PathBuf> {
    let mut result = Vec::new();
    for &library in names {
        let name = prefix
            .map(<str>::to_owned)
            .map(|s| s + "_" + library)
            .unwrap_or_else(|| library.to_owned());

        let library_name = if cfg!(windows) {
            name.clone() + ".dll"
        } else if cfg!(target_os = "linux") {
            "lib".to_owned() + &name + ".so"
        } else if cfg!(target_os = "macos") {
            "lib".to_owned() + &name + ".dylib"
        } else {
            panic!("unrecognized target OS; can't get frontend library name")
        };

        let mut path = None;

        if let StdOption::Some(over) = overrides.get(&*name) {
            path = Some(over.as_ref().to_owned());
        } else if name.contains("/") {
            path = Some(PathBuf::from(&name));
        } else {
            for search_path in search_paths {
                let mut library_path = search_path.clone();
                library_path.push(&library_name);
                if library_path.exists() {
                    path = Some(library_path);
                    break;
                }
            }
        }

        if let Some(path) = path {
            result.push(path);
        } else {
            eprintln!("warning: couldn't locate library to load for {}", name);
        }
    }
    result
}

pub const XLANG_PLUGIN_DIR: &str = std::env!("xlang_plugin_dir");

pub fn find_tool(target: &Target, name: StringView) -> Option<PathBuf> {
    let s = {
        let mut s = String::new();
        s += target.get_name();
        s += "-";
        s += name;
        s
    };

    if let Ok(s) = which::which(s) {
        Some(s)
    } else if target.get_name() == std::env!("host") {
        Some((&name).into())
    } else {
        None
    }
}
