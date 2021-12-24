#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
mod argparse;

use crate::argparse::{parse_args, ArgSpec, TakesArg};
use std::fs::File;
use xlang::abi::io::ReadAdapter;
use xlang::abi::string::StringView;
use xlang::plugin::XLangFrontend;
use xlang::prelude::v1::*;
use xlang_host::dso::Handle;

static FRONTENDS: [&str; 1] = ["c"];
type FrontendInit = extern "C" fn() -> DynBox<dyn XLangFrontend>;

fn main() {
    let s = String::from("Hello World");
    println!("{}", s);

    let argspecs = xlang::vec![
        ArgSpec::new(
            "output",
            Vec::new(),
            xlang::vec!['o'],
            TakesArg::Always,
            true,
        ),
        ArgSpec::new(
            "intree",
            xlang::vec!["intree", "in-tree"],
            Vec::new(),
            TakesArg::Never,
            true
        )
    ];

    let args = parse_args(&argspecs);
    println!("{:?}", args);

    let mut search_paths = Vec::new();

    let intree = args
        .0
        .iter()
        .any(|arg| arg.name == "intree" && arg.value != Some(String::from("false")));
    if intree {
        let executable_path = std::env::current_exe()
            .expect("Unable to find executable location; can't use --intree");
        search_paths.push(executable_path.parent().unwrap().to_owned());
    }

    if cfg!(windows) {
        todo!();
    } else if cfg!(target_os = "linux") {
        // TODO: Add target-inclusive paths (e.g. /usr/lib/x86_64-linux-gnu/xlang)
        search_paths.push("/usr/local/lib/xlang".into());
        search_paths.push("/usr/lib/xlang".into());
        search_paths.push("/lib/xlang".into());
    } else if cfg!(target_os = "macos") {
        todo!();
    } else {
        panic!("unrecognized target OS; can't build plugin list");
    }

    let mut frontend_paths = Vec::new();
    for &frontend in &FRONTENDS {
        let library_name = if cfg!(windows) {
            "frontend_".to_owned() + frontend + ".dll"
        } else if cfg!(target_os = "linux") {
            "libfrontend_".to_owned() + frontend + ".so"
        } else if cfg!(target_os = "macos") {
            "libfrontend_".to_owned() + frontend + ".dylib"
        } else {
            panic!("unrecognized target OS; can't get frontend library name")
        };

        let mut path = None;
        for search_path in &search_paths {
            let mut library_path = search_path.clone();
            library_path.push(&library_name);
            if library_path.exists() {
                path = Some(library_path);
                break;
            }
        }

        if let Some(path) = path {
            frontend_paths.push(path);
        } else {
            eprintln!(
                "warning: couldn't locate library to load for frontend \"{}\"",
                frontend
            );
        }
    }

    let mut frontend_handles = Vec::new();
    for frontend_path in &frontend_paths {
        frontend_handles.push(Handle::open(frontend_path).expect("couldn't load frontend library"));
    }

    let mut frontends = Vec::new();
    for frontend_handle in &frontend_handles {
        let initializer: FrontendInit =
            unsafe { frontend_handle.function_sym("xlang_frontend_main") }
                .expect("frontend library missing required entry point");
        frontends.push(initializer());
    }

    for file in &args.1 {
        let file_view = StringView::new(file);
        let mut frontend = None;
        for fe in &mut frontends {
            if fe.file_matches(file_view) {
                frontend = Some(fe);
                break;
            }
        }
        if let Some(frontend) = frontend {
            frontend.set_file_path(file_view);
            let mut read_adapter = ReadAdapter::new(File::open(&**file).expect("can't read input file"));
            frontend.read_source(DynMut::unsize_mut(&mut read_adapter)).unwrap();
        } else {
            panic!("couldn't find a frontend to process {}", file);
        }
    }
}
