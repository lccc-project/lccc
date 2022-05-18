#![allow(clippy::collapsible_else_if)]

#[macro_use]
extern crate serde_derive;

use install_dirs::dirs::{CanonicalizationError, InstallDirs};
use std::collections::HashMap;
use std::env;
use std::io::{self, BufRead, Write};
use std::ops::Deref;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug, Deserialize, Serialize)]
struct Directories {
    lcccdir: PathBuf,
    xlangpluginsdir: PathBuf,
    libsrcdir: PathBuf,
    #[serde(flatten)]
    install_dirs: InstallDirs,
}

impl Directories {
    fn canonicalize(mut self) -> Result<Self, CanonicalizationError> {
        self.install_dirs = self.install_dirs.canonicalize()?;
        self.lcccdir = InstallDirs::canonicalize_dir(&self.install_dirs.libdir, self.lcccdir);
        self.xlangpluginsdir = InstallDirs::canonicalize_dir(&self.lcccdir, self.xlangpluginsdir);
        self.libsrcdir = InstallDirs::canonicalize_dir(&self.lcccdir, self.libsrcdir);
        Ok(self)
    }
}

impl Default for Directories {
    fn default() -> Self {
        let mut install_dirs = InstallDirs::with_project_name("lccc");
        install_dirs.read_env();
        Directories {
            install_dirs,
            lcccdir: "lccc".into(),
            xlangpluginsdir: "xlang/plugins".into(),
            libsrcdir: "libsrc".into(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct Configuration {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    enable_languages: Vec<String>,
    directories: Directories,
}

impl Default for Configuration {
    fn default() -> Self {
        let mut install_dirs = InstallDirs::with_project_name("lccc");
        install_dirs.read_env();
        Configuration {
            directories: Directories {
                install_dirs,
                ..Directories::default()
            },
            enable_languages: vec![
                "c".into(),
                "rust".into(),
                "xir".into(),
            ],
        }
    }
}

#[derive(PartialEq)]
enum Mode {
    Build(Vec<String>),
    Config(HashMap<String, String>),
    ConfigAndBuild(HashMap<String, String>, Vec<String>),
    Interactive,
}

impl Default for Mode {
    fn default() -> Self {
        Self::Interactive
    }
}

fn prompt(name: &str, default: Option<&str>) -> String {
    if let Some(default) = default {
        print!("{} [{}]: ", name, default);
        io::stdout().flush().unwrap();
        let line = io::stdin().lock().lines().next().unwrap().unwrap(); // :)
        if line.is_empty() {
            default.to_owned()
        } else {
            line
        }
    } else {
        todo!()
    }
}

fn main() {
    let mut mode = Mode::default();
    let mut args = env::args();
    let _ = args.next();

    for arg in args {
        match arg {
            x if x.starts_with('-') => todo!(), // Possibly config flag, possibly an option to the configurator itself
            x => {
                if x == "lccc" || x == "lcrustc" {
                    todo!(); // Run target
                } else {
                    if let Mode::Build(targets) | Mode::ConfigAndBuild(_, targets) = &mut mode {
                        targets.push(x)
                    } else if let Mode::Config(flags) = mode {
                        mode = Mode::ConfigAndBuild(flags, vec![x])
                    } else {
                        mode = Mode::Build(vec![x])
                    }
                }
            }
        }
    }

    let mut config = std::fs::read("lccc-config.toml")
        .ok()
        .and_then(|x| toml::from_slice(&x).ok())
        .unwrap_or_else(Configuration::default);

    if mode == Mode::Interactive {
        println!("LCCC configurator v0.1\nby Ray Redondo\n"); // Double newline is intentional
        println!("For any of the following configure options, either type what you want the setting to be, or press enter for the default.\n"); // And again

        let install_prefix = prompt(
            "Default install prefix",
            config.directories.install_dirs.prefix.to_str(),
        );

        mode = Mode::ConfigAndBuild(
            [("prefix".into(), install_prefix)].into(),
            vec!["all".into()],
        );
    }

    if let Mode::Config(new_config) | Mode::ConfigAndBuild(new_config, _) = &mode {
        for (name, value) in new_config {
            match name.deref() {
                "prefix" => config.directories.install_dirs.prefix = value.into(),
                _ => panic!(),
            }
        }
    }

    Some(toml::to_vec(&config).unwrap()).and_then(|x| std::fs::write("lccc-config.toml", &x).ok()).unwrap_or_else(|| println!("warning: couldn't write config file"));

    config.directories = config.directories.canonicalize().unwrap();

    if let Mode::ConfigAndBuild(_, targets) = mode {
        mode = Mode::Build(targets);
    }

    if let Mode::Build(targets) = mode {
        let cargo_exe = env::var("CARGO_IN_USE").unwrap();

        let mut real_targets = Vec::new();

        for target in targets {
            if target == "all" {
                real_targets.push("all-lccc".into());
                if config.enable_languages.contains(&"rust".into()) {
                    real_targets.push("all-lcrustc".into());
                }
                for language in &config.enable_languages {
                    real_targets.push(String::from("all-frontend-") + &language);
                }
            } else if target == "install" {
                real_targets.push("install-lccc".into());
                if config.enable_languages.contains(&"rust".into()) {
                    real_targets.push("install-lcrustc".into());
                }
                for language in &config.enable_languages {
                    real_targets.push(String::from("install-frontend-") + &language);
                }
            } else {
                real_targets.push(target);
            }
        }

        for target in real_targets {
            if let Some(target) = target.strip_prefix("all-") {
                assert!(Command::new(&cargo_exe).args(["build", "-p", target]).status().unwrap().success(), "error: cargo command failed");
            } else {
                panic!("unrecognized target: {}", target);
            }
        }

        // TODO: Installing, fine-grained control
    }
}
