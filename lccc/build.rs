#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]
use std::str::FromStr;
use std::{io::Read, path::PathBuf};

use install_dirs::dirs::InstallDirs;
use serde::{Deserialize, Deserializer};
use target_tuples::Target;

fn default_lccc_dir() -> PathBuf {
    PathBuf::from("lccc")
}

fn default_xlang_plugindir() -> PathBuf {
    PathBuf::from("xlang/plugins")
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Paths {
    #[serde(flatten)]
    pub installdirs: install_dirs::dirs::InstallDirs,
    #[serde(default = "default_lccc_dir")]
    pub lccc_dir: PathBuf,

    #[serde(default = "default_xlang_plugindir")]
    pub xlang_plugin_dir: PathBuf,
}

fn parse_target<'de, D: Deserializer<'de>>(de: D) -> Result<Target, D::Error> {
    let s = <&'de str>::deserialize(de)?;
    Target::from_str(s).map_err(<D::Error as serde::de::Error>::custom)
}

fn use_host() -> Target {
    Target::parse(&std::env::var("TARGET").unwrap())
}

#[derive(Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Targets {
    #[serde(deserialize_with = "parse_target")]
    #[serde(default = "use_host")]
    pub default_target: Target,
}

#[derive(Deserialize)]
pub struct Config {
    pub paths: Paths,
    pub targets: Targets,
}

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");
    let file = {
        let mut file = PathBuf::from(std::env!("CARGO_MANIFEST_DIR"));
        file.push("config.toml");
        file
    };

    let mut cfg: Config = if let Ok(mut file) = std::fs::File::open(file) {
        println!("cargo:rerun-if-changed=config.toml");
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        toml::from_str(&content).unwrap()
    } else {
        Config {
            paths: Paths {
                installdirs: InstallDirs::defaults(),
                lccc_dir: default_lccc_dir(),
                xlang_plugin_dir: default_xlang_plugindir(),
            },
            targets: Targets {
                default_target: Target::parse(&std::env::var("TARGET").unwrap()),
            },
        }
    };

    cfg.paths.installdirs.read_env();
    cfg.paths.installdirs = cfg.paths.installdirs.canonicalize().unwrap();
    cfg.paths.lccc_dir =
        InstallDirs::canonicalize_dir(&cfg.paths.installdirs.libdir, cfg.paths.lccc_dir);
    cfg.paths.xlang_plugin_dir =
        InstallDirs::canonicalize_dir(&cfg.paths.lccc_dir, cfg.paths.xlang_plugin_dir);

    for (name, dir) in cfg.paths.installdirs.as_env() {
        println!("cargo:rustc-env={}={}", name, dir.display());
    }

    println!("cargo:rustc-env=lccc_dir={}", cfg.paths.lccc_dir.display());
    println!(
        "cargo:rustc-env=xlang_plugin_dir={}",
        cfg.paths.xlang_plugin_dir.display()
    );
    println!(
        "cargo:rustc-env=default_target={}",
        cfg.targets.default_target
    );

    println!("cargo:rustc-env=host={}", std::env::var("TARGET").unwrap());
    Ok(())
}
