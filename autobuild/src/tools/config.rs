use std::collections::HashMap;
use std::env::Args;
use std::fs::File;
use std::io::{self, Read};

use std::path::{Path, PathBuf};

use install_dirs::dirs::InstallDirs;
use target_tuples::Target;

use crate::config::{ConfigData, ConfigInstallDirs, ConfigTargets, ConfigVarValue};
use crate::helpers::SplitOnceOwned;

fn help() {
    println!()
}

pub fn main(prg_name: &str, mut args: Args) -> io::Result<()> {
    let mut base_dir = None;
    let mut src_dir = None;
    let mut cfg_dir = None;
    let mut config_vars = HashMap::new();
    let mut extra_install_dirs = HashMap::new();

    let mut prefix_set = false;

    let mut install_dirs = InstallDirs::defaults();

    let mut build_alias = None;
    let mut host_alias = None;
    let mut target_alias = None;

    install_dirs.read_env();

    while let Some(arg) = args.next() {
        match &*arg {
            "--help" => {
                super::print_help(prg_name, "config", help);
                return Ok(());
            }
            "--version" => {
                super::print_version();
                return Ok(());
            }
            "--set" => {
                let mut val = super::require_arg("--set", &mut args)?;

                if let Some((k, v)) = val.split_once_take("=") {
                    config_vars.insert(k, ConfigVarValue::Value(v));
                } else {
                    config_vars.insert(val, ConfigVarValue::Set);
                }
            }
            "--unset" => {
                let val = super::require_arg("--unset", &mut args)?;

                config_vars.insert(val, ConfigVarValue::Unset);
            }
            "--install" => {
                let mut val = super::require_arg("--install", &mut args)?;

                let (k,v) = val.split_once_owned("=")
                    .map_err(|val| io::Error::new(io::ErrorKind::InvalidInput, format!("--install requires an argument of the form dir=path, but got `{}` instead", val)))?;

                extra_install_dirs.insert(k, PathBuf::from(v));
            }
            "--src-dir" => {
                let val = super::require_arg("--src-dir", &mut args)?;

                src_dir = Some(PathBuf::from(val));
            }

            "--config-dir" => {
                let val = super::require_arg("--config-dir", &mut args)?;

                cfg_dir = Some(PathBuf::from(val));
            }

            "--prefix" => {
                let val = super::require_arg("--prefix", &mut args)?;

                install_dirs.prefix = PathBuf::from(val);

                prefix_set = true
            }
            "--build" => {
                let val = super::require_arg("--build", &mut args)?;

                build_alias = Some(val);
            }

            "--host" => {
                let val = super::require_arg("--host", &mut args)?;

                host_alias = Some(val);
            }

            "--target" => {
                let val = super::require_arg("--target", &mut args)?;

                target_alias = Some(val);
            }

            "--" => {
                base_dir = args.next().map(PathBuf::from);
                break;
            }
            x if x.starts_with("--") => {
                let val = super::require_arg(x, &mut args)?;

                install_dirs.set_from_arg(x, val).map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Unrecognized option {}", x),
                    )
                })?;
            }

            _ => {
                base_dir = Some(PathBuf::from(arg));
                break;
            }
        }
    }

    let base_dir = base_dir.ok_or(()).or_else(|_| std::env::current_dir())?;

    let base_dir = base_dir.canonicalize()?;

    let cfg_dir = match cfg_dir {
        Some(cfg_dir) => cfg_dir.canonicalize()?,
        None => {
            let mut base_dir_config = base_dir.clone();
            base_dir_config.push(".config.toml");

            if std::fs::metadata(base_dir_config).is_ok() {
                base_dir.clone()
            } else {
                std::env::current_dir()?
            }
        }
    };

    let cfg_file = {
        let mut buf = cfg_dir.clone();
        buf.push(".config.toml");
        buf
    };

    let mut cfg_data = match File::open(&cfg_file) {
        Ok(mut file) => {
            let mut st = String::new();
            file.read_to_string(&mut st)?;
            Box::new(
                toml::from_str::<ConfigData>(&st)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?,
            )
        }
        Err(_) => {
            let src_dir = match src_dir {
                Some(src_dir) => src_dir.canonicalize()?,
                None => {
                    let mut src_dir = base_dir;

                    loop {
                        let mut autobuild_file = src_dir.clone();
                        autobuild_file.push("autobuild.toml");

                        if std::fs::metadata(autobuild_file).is_ok() {
                            break;
                        } else if &src_dir == Path::new("/") {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidInput,
                                "Could not find source manifest (autobuild.toml)",
                            ));
                        } else {
                            src_dir.pop();
                        }
                    }

                    src_dir
                }
            };

            let build = match build_alias.as_ref() {
                Some(alias) => alias.parse().map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Target `{}` is not recognized or cannot be parsed", alias),
                    )
                })?,
                None => target_tuples::from_env!("TARGET"),
            };

            let host = match host_alias.as_ref() {
                Some(alias) => alias.parse().map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Target `{}` is not recognized or cannot be parsed", alias),
                    )
                })?,
                None => build.clone(),
            };

            let target = match target_alias.as_ref() {
                Some(alias) => alias.parse().map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!("Target `{}` is not recognized or cannot be parsed", alias),
                    )
                })?,
                None => host.clone(),
            };

            let dirs = ConfigInstallDirs {
                install_dirs,
                rest: extra_install_dirs,
            };

            let targets = ConfigTargets {
                build,
                host,
                target,
            };
            Box::new(ConfigData::new(src_dir, dirs, targets))
        }
    };

    Ok(())
}
