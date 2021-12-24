#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
mod argparse;

use crate::argparse::{parse_args, ArgSpec, TakesArg};
use std::fs::File;
use std::path::PathBuf;
use target_tuples::Target;
use xlang::abi::io::{ReadAdapter, WriteAdapter};
use xlang::abi::string::StringView;
use xlang::plugin::{XLangCodegen, XLangFrontend, XLangPlugin};
use xlang::prelude::v1::*;
use xlang_host::dso::Handle;

static FRONTENDS: [&str; 1] = ["c"];
static CODEGENS: [&str; 1] = ["x86"];
type FrontendInit = extern "C" fn() -> DynBox<dyn XLangFrontend>;
type CodegenInit = extern "C" fn() -> DynBox<dyn XLangCodegen>;

fn find_libraries(search_paths: &[PathBuf], names: &[&str], prefix: &str) -> Vec<PathBuf> {
    let mut result = Vec::new();
    for &library in names {
        let library_name = if cfg!(windows) {
            prefix.to_owned() + "_" + library + ".dll"
        } else if cfg!(target_os = "linux") {
            "lib".to_owned() + prefix + "_" + library + ".so"
        } else if cfg!(target_os = "macos") {
            "lib".to_owned() + prefix + "_" + library + ".dylib"
        } else {
            panic!("unrecognized target OS; can't get frontend library name")
        };

        let mut path = None;
        for search_path in search_paths {
            let mut library_path = search_path.clone();
            library_path.push(&library_name);
            if library_path.exists() {
                path = Some(library_path);
                break;
            }
        }

        if let Some(path) = path {
            result.push(path);
        } else {
            eprintln!(
                "warning: couldn't locate library to load for {} \"{}\"",
                prefix, library
            );
        }
    }
    result
}

const XLANG_PLUGIN_DIR: &str = std::env!("xlang_plugin_dir");

#[allow(clippy::too_many_lines)]
fn main() {
    let mut target = target_tuples::from_env!("default_target");
    println!("Target: {} ({})", target, target.get_name());
    let mut output = String::from("a.o");

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
        ),
        ArgSpec::new(
            "plugindirs",
            xlang::vec!["plugin-dirs"],
            Vec::new(),
            TakesArg::Always,
            false
        ),
        ArgSpec::new(
            "target",
            xlang::vec!["target"],
            Vec::new(),
            TakesArg::Always,
            true
        )
    ];

    let (args, files) = parse_args(&argspecs);
    println!("{:?} {:?}", args, files);

    let mut search_paths = Vec::new();

    let mut intree = false;

    for arg in &args {
        match arg.name {
            "intree" => intree = true,
            "plugindirs" => search_paths.push(PathBuf::from(arg.value.as_deref().unwrap())),
            "target" => target = Target::parse(arg.value.as_ref().unwrap()),
            "output" => output = arg.value.as_ref().unwrap().clone(),
            _ => panic!(),
        }
    }
    if intree {
        let executable_path = std::env::current_exe()
            .expect("Unable to find executable location; can't use --intree");
        search_paths.push(executable_path.parent().unwrap().to_owned());
    }

    search_paths.push(XLANG_PLUGIN_DIR.into());

    let frontend_paths = find_libraries(&search_paths, &FRONTENDS, "frontend");

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

    let codegen_paths = find_libraries(&search_paths, &CODEGENS, "codegen");

    let mut codegen_handles = Vec::new();
    for codegen_path in &codegen_paths {
        codegen_handles.push(Handle::open(codegen_path).expect("couldn't load frontend library"));
    }
    let mut codegens = Vec::new();
    for codegen_handle in &codegen_handles {
        let initializer: CodegenInit = unsafe { codegen_handle.function_sym("xlang_backend_main") }
            .expect("frontend library missing required entry point");
        codegens.push(initializer());
    }

    let xtarget = xlang::targets::Target::from(target);

    let mut codegen = None;

    for cg in &mut codegens {
        if cg.target_matches(&xtarget) {
            codegen = Some(cg);
            break;
        }
    }

    let codegen = if let Some(cg) = codegen {
        cg
    } else {
        panic!(
            "couldn't find a backend for target {}",
            Target::from(&xtarget)
        )
    };

    for file in &files {
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
            let mut read_adapter =
                ReadAdapter::new(File::open(&**file).expect("can't read input file"));
            frontend
                .read_source(DynMut::unsize_mut(&mut read_adapter))
                .unwrap();
            let mut file = xlang::ir::File {
                target: xtarget.clone(),
                root: xlang::ir::Scope::default(),
            };

            frontend.accept_ir(&mut file).unwrap();
            dbg!(&file);
            codegen.accept_ir(&mut file).unwrap();

            let mut write_adapter =
                WriteAdapter::new(File::create(&*output).expect("Can't create output file"));
            codegen
                .write_output(DynMut::unsize_mut(&mut write_adapter))
                .unwrap();
        } else {
            panic!("couldn't find a frontend to process {}", file);
        }
    }
}
