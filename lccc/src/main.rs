#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

use lccc::argparse::{parse_args, ArgSpec, TakesArg};
use lccc::{LinkOutput, Mode, OptimizeLevel};
use std::fs::File;
use std::io::ErrorKind;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;
use target_tuples::Target;
use xlang::abi::collection::HashSet;
use xlang::abi::io::{ReadAdapter, ReadSeekAdapter, WriteAdapter};
use xlang::abi::span::Span;
use xlang::abi::string::StringView;
use xlang::plugin::{self, OutputMode, XLangCodegen, XLangFrontend, XLangPlugin};
use xlang::prelude::v1::*;
use xlang_host::dso::Handle;

pub struct DriverCallbacks {
    input_file_dir: PathBuf,
}

impl plugin::DriverCallbacks for DriverCallbacks {
    fn read_relative_file(
        &mut self,
        path: StringView,
    ) -> xlang::abi::io::Result<DynBox<dyn xlang::abi::io::ReadSeek>> {
        let mut abs_path = self.input_file_dir.clone();
        abs_path.push(path);
        let file = xlang::abi::try_!(xlang::abi::result::Result::from(
            std::fs::File::open(abs_path).map_err(xlang::abi::io::Error::from)
        ));

        let val = xlang::abi::boxed::Box::new(ReadSeekAdapter::new(file));

        xlang::abi::result::Ok(DynBox::unsize_box(val))
    }
}

pub enum DumpMode {
    Target,
    Machine,
    Features,
    MachineProperties,
    TargetProperties,
    OsProperties,
    ArchProperties,
    PluginDirs,
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
fn main() {
    let time = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_millis() as u64;

    let seed = std::env::var("LCCC_SEED")
        .ok()
        .map(|x| x.parse::<u64>().ok())
        .flatten()
        .unwrap_or(time);

    lccc::init_rng(seed); // we have to call this before initializing a hashtable

    let mut target = target_tuples::from_env!("default_target");
    let mut output = None;

    let mut tmpfiles = Vec::new();

    let mut mode = Mode::Link;
    let mut link_output = LinkOutput::Executable;

    let mut plugin_overrides = HashMap::<_, _>::new();
    let mut userplugins = Vec::new();

    let mut libdirs = Vec::new();

    let argspecs = xlang::vec![
        ArgSpec::new(
            "output",
            Vec::new(),
            xlang::vec!['o'],
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "optimize",
            Vec::new(),
            xlang::vec!['O'],
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "intree",
            xlang::vec!["intree", "in-tree"],
            Vec::new(),
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "pluginpath",
            xlang::vec!["plugin-path"],
            Vec::new(),
            TakesArg::Always,
            false
        ),
        ArgSpec::new(
            "plugindirs",
            xlang::vec!["plugin-dirs"],
            Vec::new(),
            TakesArg::Always,
            false
        ),
        ArgSpec::new(
            "plugin",
            xlang::vec!["plugin"],
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
        ),
        ArgSpec::new(
            "compile",
            xlang::vec!["compile-only"],
            xlang::vec!['c'],
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "typeck",
            xlang::vec!["type-check"],
            Vec::new(),
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "printxir",
            xlang::vec!["print-xir"],
            Vec::new(),
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "shared",
            xlang::vec!["shared"],
            Vec::new(),
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "ldout",
            xlang::vec!["linker-output"],
            Vec::new(),
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "version",
            xlang::vec!["version"],
            xlang::vec!['V'],
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "mach-specific",
            Vec::new(),
            xlang::vec!['m'],
            TakesArg::Always,
            false
        ),
        ArgSpec::new(
            "dump",
            xlang::vec!["dump"],
            Vec::new(),
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "lib-dir",
            Vec::new(),
            xlang::vec!['L'],
            TakesArg::Always,
            false
        ),
        ArgSpec::new(
            "wopt",
            Vec::new(),
            xlang::vec!['W'],
            TakesArg::Always,
            false
        )
    ];

    let (args, files) = parse_args(&argspecs);
    println!("{:?} {:?}", args, files);

    let mut search_paths = Vec::new();

    let mut intree = false;

    let mut opt_mode = OptimizeLevel::Integer(0);

    let mut arch_machine = None;
    let mut tune_machine = None;
    let mut feature_opts = HashMap::<_, _>::new();
    let mut abi = None;

    let mut dump_modes = Vec::new();

    for arg in &args {
        match arg.name {
            "intree" => intree = true,
            "plugindirs" => search_paths.push(PathBuf::from(arg.value.as_deref().unwrap())),
            "target" => target = Target::parse(arg.value.as_ref().unwrap()),
            "output" => output = arg.value.clone(),
            "compile" => mode = Mode::CompileOnly,
            "typeck" => mode = Mode::TypeCheck,
            "printxir" => mode = Mode::Xir,
            "pluginpath" => {
                let arg = arg.value.as_ref().unwrap();
                let (name, path) = arg.split_once('=').unwrap();
                let path = PathBuf::from(path);
                plugin_overrides.insert(name.to_string(), path);
            }
            "plugin" => {
                let arg = arg.value.as_ref().unwrap();
                userplugins.push(arg.clone());
            }
            "optimize" => {
                let arg = arg.value.as_ref().unwrap();
                match &**arg {
                    "s" => opt_mode = OptimizeLevel::Size,
                    "z" => opt_mode = OptimizeLevel::Zize,
                    "g" => opt_mode = OptimizeLevel::Debug,
                    "fast" => opt_mode = OptimizeLevel::Fast,
                    "extra" => opt_mode = OptimizeLevel::Extra,
                    x => opt_mode = OptimizeLevel::Integer(x.parse().unwrap()),
                }
            }
            "ldout" => {
                link_output = match arg.value.as_deref() {
                    Some("shared") => LinkOutput::Shared,
                    Some("static") => LinkOutput::Static,
                    Some("pie") => LinkOutput::Pie,
                    Some("executable") => LinkOutput::Executable,
                    Some("manifest") => LinkOutput::Manifest,
                    Some(val) => panic!("Invalid or unknown type {}", val),
                    None => unreachable!(),
                };
            }
            "version" => {
                println!("lccc {}", std::env!("CARGO_PKG_VERSION"));
                println!("Copyright (C) 2021-2022 Lightning Creations");
                println!("Released under the Terms of the 2 Clause BSD license, with explicit patent grant");
                return;
            }
            "machine-specific" => match arg.value.as_deref().unwrap() {
                x @ ("32" | "x32" | "64") => {
                    abi = Some(x);
                }
                x if x.starts_with("arch") => {
                    let (_, arch) = x.split_once('=').unwrap();
                    arch_machine = Some(arch);
                }
                x if x.starts_with("tune") => {
                    let (_, tune) = x.split_once('=').unwrap();
                    tune_machine = Some(tune);
                }
                x if x.starts_with("no") => {
                    let (_, feature) = x.split_once('-').unwrap();
                    feature_opts.insert(StringView::new(feature), false);
                }
                x => {
                    feature_opts.insert(StringView::new(x), true);
                }
            },

            "dump" => {
                for i in arg.value.as_deref().unwrap().split(',') {
                    match i {
                        "target" => dump_modes.push(DumpMode::Target),
                        "machine" => dump_modes.push(DumpMode::Machine),
                        "features" => dump_modes.push(DumpMode::Features),
                        "target-properties" => dump_modes.push(DumpMode::TargetProperties),
                        "machine-properties" => dump_modes.push(DumpMode::MachineProperties),
                        "arch-properties" => dump_modes.push(DumpMode::ArchProperties),
                        "os-properties" => dump_modes.push(DumpMode::OsProperties),
                        "plugin-dirs" => dump_modes.push(DumpMode::PluginDirs),
                        m => {
                            eprintln!("Unrecognized dump mode {}", m);
                            std::process::exit(1)
                        }
                    }
                }
            }
            "lib-dir" => {
                libdirs.push(arg.value.clone().unwrap());
            }
            "wopt" => {}
            _ => panic!(),
        }
    }
    if intree {
        let executable_path = std::env::current_exe()
            .expect("Unable to find executable location; can't use --intree");
        search_paths.push(executable_path.parent().unwrap().to_owned());
    }

    search_paths.push(lccc::XLANG_PLUGIN_DIR.into());
    let mut upaths = userplugins.iter().map(Deref::deref).collect::<Vec<_>>();
    match opt_mode {
        OptimizeLevel::Integer(0) => {}
        OptimizeLevel::Integer(n) => {
            upaths.extend(
                lccc::OPTIMIZERS[..(n as usize).max(lccc::OPTIMIZERS.len())]
                    .iter()
                    .copied()
                    .flatten()
                    .copied(),
            );
        }
        _ => todo!(),
    }

    let xtarget = StringView::new(target.get_name());
    println!("building for {:?}", xtarget);
    let properties = lccc::targets::get_properties(xtarget).unwrap();

    let arch_mach = match arch_machine {
        Some("generic") | None => properties.arch.default_machine,
        Some("native") => todo!("-march=native"),
        Some(mach) => properties
            .arch
            .machines
            .into_iter()
            .copied()
            .find(|Pair(name, _)| (*name) == mach)
            .map_or_else(
                || {
                    eprintln!("Unknown machine {} for target {}", mach, target);
                    std::process::exit(1)
                },
                |Pair(_, m)| m,
            ),
    };

    if let Some(tune) = tune_machine {
        todo!("-mtune={}", tune);
    }

    if let Some(abi) = abi {
        todo!("-m{}", abi)
    }

    let mut features = HashSet::<_>::new();

    for feature in arch_mach.default_features {
        let _ = features.insert(*feature);
    }

    for Pair(feature, enabled) in properties.enabled_features {
        if *enabled {
            let _ = features.insert(*feature);
        } else {
            let _ = features.remove(feature);
        }
    }

    for Pair(feature, enabled) in &feature_opts {
        if *enabled {
            let _ = features.insert(*feature);
        } else {
            let _ = features.remove(feature);
        }
    }

    let features = features.iter().copied().collect::<Vec<_>>();

    if !dump_modes.is_empty() {
        for m in dump_modes {
            match m {
                DumpMode::Target => println!("Target: {}", target),
                DumpMode::Machine => match arch_machine {
                    Some("generic") | None => {
                        for Pair(name, mprop) in properties.arch.machines {
                            if core::ptr::eq(*mprop, arch_mach) {
                                println!("Machine: {}", name);
                                break;
                            }
                        }
                    }
                    Some("native") => todo!("-march=native"),
                    Some(name) => println!("Machine: {}", name),
                },
                DumpMode::Features => {
                    println!("Features:");
                    for feature in &features {
                        println!("\t{}", feature);
                    }
                }
                DumpMode::MachineProperties => println!("Machine Properties:\n{:#?}", arch_mach),
                DumpMode::TargetProperties => println!("Target Properties:\n{:#?}", properties),
                DumpMode::OsProperties => println!("OS Properties:\n{:#?}", properties.os),
                DumpMode::ArchProperties => println!("Arch Properties:\n{:#?}", properties.arch),
                DumpMode::PluginDirs => println!("XLang Plugin Dirs:\n{:?}", search_paths),
            }
        }
        std::process::exit(1);
    }

    let userpaths = lccc::find_libraries(
        &search_paths,
        &upaths,
        std::option::Option::None,
        &plugin_overrides,
    );

    let mut user_handles = Vec::new();

    for upath in &userpaths {
        user_handles.push(Handle::open(upath).expect("Could not load plugin"));
    }

    let mut userplugins = Vec::new();
    for h in &user_handles {
        let init = unsafe { h.function_sym("xlang_plugin_main") };
        let init: lccc::PluginInit = init.expect("plugin libray missing required entry point");
        userplugins.push(init());
    }

    let frontend_paths = lccc::find_libraries(
        &search_paths,
        &lccc::FRONTENDS,
        std::option::Option::Some("frontend"),
        &plugin_overrides,
    );

    let mut frontend_handles = Vec::new();
    for frontend_path in &frontend_paths {
        frontend_handles.push(Handle::open(frontend_path).expect("couldn't load frontend library"));
    }

    let mut frontends = Vec::new();
    for frontend_handle in &frontend_handles {
        let initializer: lccc::FrontendInit =
            unsafe { frontend_handle.function_sym("xlang_frontend_main") }
                .expect("frontend library missing required entry point");
        frontends.push(initializer());
    }

    let codegen_paths = lccc::find_libraries(
        &search_paths,
        &lccc::CODEGENS,
        std::option::Option::Some("codegen"),
        &plugin_overrides,
    );

    let mut codegen_handles = Vec::new();

    if mode > Mode::Xir {
        for codegen_path in &codegen_paths {
            println!("Opening codegen plugin: {}", codegen_path.display());
            codegen_handles
                .push(Handle::open(codegen_path).expect("couldn't load frontend library"));
        }
    }

    let mut codegens = Vec::new();
    for codegen_handle in &codegen_handles {
        let initializer: lccc::CodegenInit =
            unsafe { codegen_handle.function_sym("xlang_backend_main") }
                .expect("codegen library missing required entry point");
        codegens.push(initializer());
    }

    let mut file_pairs = Vec::new();

    let mut codegen = None;

    for cg in &mut codegens {
        if cg.target_matches(xtarget) {
            codegen = Some(cg);
            break;
        }
    }

    for file in &files {
        let mut file_path = Path::new(file).canonicalize().unwrap();
        file_path.pop();
        let callbacks = xlang::abi::boxed::Box::new(DriverCallbacks {
            input_file_dir: file_path,
        });
        let file_view = StringView::new(file);

        let mut frontend = None;
        for fe in &mut frontends {
            if fe.file_matches(file_view) {
                frontend = Some(fe);
                break;
            }
        }
        if let Some(frontend) = frontend {
            frontend.set_callbacks(DynBox::unsize_box(callbacks));
            let outputfile = if mode < Mode::Link {
                if let Some(ref output) = output {
                    output.clone()
                } else {
                    let mut filename = &**file;
                    if let std::option::Option::Some(offset) = filename.rfind('.') {
                        filename = &filename[..offset];
                    }
                    let mut name = String::from(filename);
                    if mode == Mode::CompileOnly {
                        name += properties.os.obj_suffix;
                    } else if mode == Mode::Asm {
                        name += ".s";
                    } else if mode == Mode::Xir {
                        name += ".xir";
                    }
                    name
                }
            } else {
                let tmpfile = loop {
                    let tmpfile = temp_file::TempFile::with_prefix("lcccobj");

                    match tmpfile {
                        Ok(e) => break e,
                        Err(e) if e.kind() == ErrorKind::AlreadyExists => continue,
                        Err(e) => panic!("Cannot create object file: {}", e),
                    }
                };
                let path = tmpfile.path().as_os_str().to_str().unwrap().into();
                tmpfiles.push(tmpfile);
                path
            };
            file_pairs.push((file.clone(), outputfile.clone()));
            frontend.set_file_path(file_view);
            frontend.set_target(properties);
            frontend.set_machine(arch_mach);
            let mut read_adapter =
                ReadAdapter::new(File::open(&file).expect("can't read input file"));
            frontend
                .read_source(DynMut::unsize_mut(&mut read_adapter))
                .unwrap();
            let mut file = xlang::ir::File {
                target: xtarget.into(),
                root: xlang::ir::Scope::default(),
            };

            frontend.accept_ir(&mut file).unwrap();

            let required_names = frontend.required_plugins();

            let required_names = required_names.iter().map(Deref::deref).collect::<Vec<_>>();

            let required_paths = lccc::find_libraries(
                &search_paths,
                &required_names,
                std::option::Option::None,
                &plugin_overrides,
            );

            let mut required_handles = Vec::new();

            for upath in &required_paths {
                required_handles.push(Handle::open(upath).expect("Could not load plugin"));
            }

            let mut required_plugins = Vec::new();
            for h in &required_handles {
                let init = unsafe { h.function_sym("xlang_plugin_main") };
                let init: lccc::PluginInit =
                    init.expect("plugin libray missing required entry point");
                required_plugins.push(init());
            }

            for plugin in &mut required_plugins {
                plugin.set_target(properties);
                plugin.accept_ir(&mut file);
            }

            for plugin in &mut userplugins {
                plugin.set_target(properties);
                plugin.accept_ir(&mut file);
            }

            if mode >= Mode::Asm {
                let codegen = if let Some(cg) = &mut codegen {
                    cg
                } else {
                    panic!("couldn't find a backend for target {}", xtarget)
                };
                codegen.set_target(properties);
                codegen.set_features(Span::new(&features));
                codegen.accept_ir(&mut file).unwrap();
                let mut write_adapter =
                    WriteAdapter::new(File::create(outputfile).expect("Can't create output file"));
                let mode = if mode == Mode::Asm {
                    OutputMode::Asm
                } else {
                    OutputMode::Obj
                };
                codegen
                    .write_output(DynMut::unsize_mut(&mut write_adapter), mode)
                    .unwrap();
                // TODO: Handle `-S` and write assembly instead of an object
            } else if mode == Mode::Xir {
            }
        } else {
            file_pairs.push((file.clone(), file.clone()));
        }
    }

    if mode == Mode::Link {
        match link_output {
            LinkOutput::Shared => todo!(),
            LinkOutput::Static => {
                let outputs = file_pairs.iter().map(|(_, output)| output);

                match Command::new(
                    lccc::find_tool(&target, const_sv!("ar")).expect("Could not find program `ar`"),
                )
                .arg("-rcs")
                .arg(output.as_ref().unwrap())
                .args(outputs)
                .status()
                {
                    Ok(_) => {}
                    Err(e) => panic!(
                        "Could not run command ar rcs {}: {}",
                        file_pairs
                            .iter()
                            .map(|(_, output)| output)
                            .map(Deref::deref)
                            .collect::<std::string::String>(),
                        e
                    ),
                }
            }
            LinkOutput::Executable | LinkOutput::Pie => {
                let mut link_args = Vec::<String>::new();

                let mut libdirs = libdirs.iter().map(PathBuf::from).collect::<Vec<_>>();

                for basedir in properties.os.base_dirs {
                    for libdir in properties.link.libdirs {
                        let targ1 = target.to_string();
                        let targ2 = {
                            let arch = target.arch_name();
                            let os = target.operating_system().map(|o| o.canonical_name());
                            let env = target.environment().map(|o| o.canonical_name());
                            let of = target.object_format().map(|o| o.canonical_name());
                            let mut name = String::from(arch);
                            name.push_str("-");
                            if let std::option::Option::Some(os) = os {
                                name.push_str(os);
                                name.push_str("-"); // Assume, for now, there's at least one more component
                            }

                            if let std::option::Option::Some(env) = env {
                                name.push_str(env);
                            }

                            if let std::option::Option::Some(of) = of {
                                name.push_str(of);
                            }

                            name
                        };

                        for &targ in &["", &targ1, &targ2] {
                            let mut path = PathBuf::from(basedir);
                            path.push(libdir);
                            path.push(targ);
                            libdirs.push(path);
                        }
                        for &targ in &[&*targ1, &targ2] {
                            let mut path = PathBuf::from(basedir);
                            path.push(targ);
                            path.push(libdir);
                            libdirs.push(path);
                        }
                    }
                }

                if link_output == LinkOutput::Pie {
                    link_args.push(String::from("-pie"));
                }

                let mut interp = None;
                eprintln!("{:?}", libdirs);

                eprintln!("Interp: {}", properties.link.interp);

                for libdir in &libdirs {
                    let mut path = libdir.clone();
                    path.push(properties.link.interp);
                    if path.exists() {
                        eprintln!("Found interpreter: {}", path.display());
                        interp = Some(path);
                        break;
                    }
                }

                let mut startfiles = Vec::new();
                for file in properties.link.startfiles {
                    let mut found = false;
                    for libdir in &libdirs {
                        let mut path = libdir.clone();
                        path.push(file);
                        if path.exists() {
                            startfiles.push(path);
                            found = true;
                            break;
                        }
                    }

                    #[allow(clippy::manual_assert)]
                    // This will be a proper error message at some point
                    if !found {
                        panic!("Could not find startfile {}", file);
                    }
                }

                let mut endfiles = Vec::new();
                for file in properties.link.endfiles {
                    let mut found = false;
                    for libdir in &libdirs {
                        let mut path = libdir.clone();
                        path.push(file);
                        if path.exists() {
                            endfiles.push(path);
                            found = true;
                            break;
                        }
                    }

                    #[allow(clippy::manual_assert)]
                    // This will be a proper error message at some point
                    if !found {
                        panic!("Could not find endfile {}", file);
                    }
                }
                let mut cmd = Command::new("ld");
                cmd.args(&link_args);
                if let Some(interp) = &interp {
                    eprintln!("Passing -dynamic-linker {}", interp.display());
                    cmd.arg("-dynamic-linker").arg(interp);
                }
                match cmd
                    .args(
                        libdirs
                            .iter()
                            .map(|p| String::from("-L") + p.as_os_str().to_str().unwrap()),
                    )
                    .arg("-o")
                    .arg(output.as_deref().unwrap_or("a.out"))
                    .args(&startfiles)
                    .args(file_pairs.iter().map(|(_, s)| s))
                    .arg("--as-needed")
                    .args(
                        properties
                            .link
                            .default_libs
                            .iter()
                            .map(|s| String::from("-l") + s),
                    )
                    .args(&endfiles)
                    .status()
                {
                    Ok(_) => {}
                    Err(e) => panic!(
                        "Failed to execute command ld {}: {}",
                        link_args
                            .iter()
                            .map(Deref::deref)
                            .collect::<std::string::String>(),
                        e
                    ),
                }
            }
            LinkOutput::Manifest => panic!("Manifest Handled elsewhere"),
        }
    }
}
