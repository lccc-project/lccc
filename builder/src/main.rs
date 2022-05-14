#![allow(clippy::collapsible_else_if)]

use std::collections::HashMap;
use std::env;
use std::io::{self, BufRead, Write};
use std::process::Command;

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
        io::stdout().flush();
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

    if mode == Mode::Interactive {
        println!("LCCC configurator v0.1\nby Ray Redondo\n"); // Double newline is intentional
        println!("For any of the following configure options, either type what you want the setting to be, or press enter for the default.\n"); // And again

        let install_prefix = prompt(
            "Default install prefix",
            match env::consts::OS {
                "linux" => Some("/usr/local".into()),
                _ => None,
            }
            .as_deref(),
        );

        mode = Mode::ConfigAndBuild(
            [("prefix".into(), install_prefix)].into(),
            vec!["all".into()],
        );
    }

    if let Mode::Config(_config) | Mode::ConfigAndBuild(_config, _) = &mode {
        // TODO: Apply configuration
    }

    if let Mode::ConfigAndBuild(_, targets) = mode {
        mode = Mode::Build(targets);
    }

    if let Mode::Build(targets) = mode {
        let cargo_exe = env::var("CARGO_IN_USE").unwrap();

        for target in targets {
            if target == "all" {
                Command::new(&cargo_exe)
                    .args(["build", "--all"])
                    .status()
                    .expect("errors in compilation, aborting");
            } else {
                todo!();
            }
        }

        // TODO: Installing, fine-grained control
    }
}
