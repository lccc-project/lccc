use std::{
    collections::HashMap,
    error::Error,
    ffi::OsStr,
    fmt::Display,
    path::{Path, PathBuf},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug)]
#[non_exhaustive]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(default = "InstallDirs::defaults"))]
pub struct InstallDirs {
    pub prefix: PathBuf,
    pub exec_prefix: PathBuf,
    pub bindir: PathBuf,
    pub sbindir: PathBuf,
    pub libdir: PathBuf,
    pub libexecdir: PathBuf,
    pub includedir: PathBuf,
    pub datarootdir: PathBuf,
    pub datadir: PathBuf,
    pub mandir: PathBuf,
    pub docdir: PathBuf,
    pub infodir: PathBuf,
    pub localedir: PathBuf,
    pub localstatedir: PathBuf,
    pub runstatedir: PathBuf,
    pub sharedstatedir: PathBuf,
    pub sysconfdir: PathBuf,
}

#[derive(Debug)]
pub struct CanonicalizationError {
    prefix: PathBuf,
}

impl Display for CanonicalizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to canonicalize Install Dirs ")?;
        f.write_fmt(format_args!(
            "(prefix {} is not an absolute path)",
            self.prefix.to_str().unwrap_or("(<non-unicode>)")
        ))
    }
}

impl Error for CanonicalizationError {}

impl InstallDirs {
    ///
    ///
    pub fn defaults() -> Self {
        Self {
            prefix: if cfg!(windows) {
                "C:\\Program Files\\"
            } else {
                "/usr/local"
            }
            .into(),
            exec_prefix: "".into(),
            bindir: "bin".into(),
            sbindir: "sbin".into(),
            libdir: "lib".into(),
            libexecdir: "libexec".into(),
            includedir: "include".into(),
            datarootdir: "share".into(),
            datadir: "".into(),
            mandir: "man".into(),
            docdir: "doc".into(),
            infodir: "info".into(),
            localedir: "locale".into(),
            localstatedir: "var".into(),
            runstatedir: "run".into(),
            sharedstatedir: "com".into(),
            sysconfdir: "var".into(),
        }
    }

    pub fn with_project_name<S: AsRef<OsStr>+?Sized>(name: &S) -> Self {
        Self {
            prefix: if cfg!(windows) {
                let mut buf = PathBuf::new();
                buf.push("C:\\Program Files");
                buf.push(name.as_ref());
                buf
            } else {
                "/usr/local".into()
            },
            exec_prefix: "".into(),
            bindir: "bin".into(),
            sbindir: "sbin".into(),
            libdir: "lib".into(),
            libexecdir: "libexec".into(),
            includedir: "include".into(),
            datarootdir: "share".into(),
            datadir: "".into(),
            mandir: "man".into(),
            docdir: {
                let mut path = PathBuf::new();
                path.push("doc");
                path.push(name.as_ref());
                path
            },
            infodir: "info".into(),
            localedir: "locale".into(),
            localstatedir: "var".into(),
            runstatedir: "run".into(),
            sharedstatedir: "com".into(),
            sysconfdir: "var".into(),
        }
    }

    pub fn with_exec_target<S: AsRef<OsStr>>(target: &S) -> Self {
        Self {
            prefix: if cfg!(windows) {
                "C:\\Program Files\\"
            } else {
                "/usr/local"
            }
            .into(),
            exec_prefix: target.as_ref().into(),
            bindir: "bin".into(),
            sbindir: "sbin".into(),
            libdir: "lib".into(),
            libexecdir: "libexec".into(),
            includedir: "include".into(),
            datarootdir: "share".into(),
            datadir: "".into(),
            mandir: "man".into(),
            docdir: "doc".into(),
            infodir: "info".into(),
            localedir: "locale".into(),
            localstatedir: "var".into(),
            runstatedir: "run".into(),
            sharedstatedir: "com".into(),
            sysconfdir: "var".into(),
        }
    }

    pub fn with_project_name_and_target<S: AsRef<OsStr>, T: AsRef<OsStr>>(
        name: &S,
        target: &T,
    ) -> Self {
        Self {
            prefix: if cfg!(windows) {
                let mut buf = PathBuf::new();
                buf.push("C:\\Program Files");
                buf.push(name.as_ref());
                buf
            } else {
                "/usr/local".into()
            },
            exec_prefix: target.as_ref().into(),
            bindir: "bin".into(),
            sbindir: "sbin".into(),
            libdir: "lib".into(),
            libexecdir: "libexec".into(),
            includedir: "include".into(),
            datarootdir: "share".into(),
            datadir: "".into(),
            mandir: "man".into(),
            docdir: {
                let mut path = PathBuf::new();
                path.push("doc");
                path.push(name.as_ref());
                path
            },
            infodir: "info".into(),
            localedir: "locale".into(),
            localstatedir: "var".into(),
            runstatedir: "run".into(),
            sharedstatedir: "com".into(),
            sysconfdir: "var".into(),
        }
    }

    pub fn canonicalize(mut self) -> Result<Self, CanonicalizationError> {
        if !self.prefix.has_root() {
            Err(CanonicalizationError {
                prefix: self.prefix,
            })
        } else {
            if !self.exec_prefix.has_root() {
                self.exec_prefix = {
                    let mut path = PathBuf::new();
                    path.push(self.prefix.clone());
                    path.push(self.exec_prefix);
                    path
                }
            }

            let exec_prefix = if (&*self.exec_prefix) == Path::new("/") {
                let mut exec_prefix = PathBuf::new();
                exec_prefix.push("/usr");
                exec_prefix
            } else {
                self.exec_prefix.clone()
            };
            let data_prefix = if (&*self.prefix) == Path::new("/") {
                let mut exec_prefix = PathBuf::new();
                exec_prefix.push("/usr");
                exec_prefix
            } else {
                self.prefix.clone()
            };
            let state_prefix = if self.prefix.starts_with("/usr") {
                let mut prefix = PathBuf::new();
                prefix.push("/");
                prefix
            } else {
                self.prefix.clone()
            };
            if !self.bindir.has_root() {
                self.bindir = {
                    let mut path = exec_prefix.clone();
                    path.push(self.bindir);
                    path
                };
            }

            if !self.sbindir.has_root() {
                self.sbindir = {
                    let mut path = exec_prefix.clone();
                    path.push(self.sbindir);
                    path
                };
            }

            if !self.libdir.has_root() {
                self.libdir = {
                    let mut path = exec_prefix.clone();
                    path.push(self.libdir);
                    path
                };
            }

            if !self.libexecdir.has_root() {
                self.libexecdir = {
                    let mut path = exec_prefix.clone();
                    path.push(self.libexecdir);
                    path
                };
            }

            if !self.includedir.has_root() {
                self.includedir = {
                    let mut path = exec_prefix.clone();
                    path.push(self.includedir);
                    path
                };
            }

            if !self.datarootdir.has_root() {
                self.datarootdir = {
                    let mut path = data_prefix.clone();
                    path.push(self.datarootdir);
                    path
                };
            }

            if !self.datadir.has_root() {
                self.datadir = {
                    let mut path = self.datarootdir.clone();
                    path.push(self.datadir);
                    path
                };
            }

            if !self.mandir.has_root() {
                self.mandir = {
                    let mut path = self.datarootdir.clone();
                    path.push(self.mandir);
                    path
                };
            }

            if !self.infodir.has_root() {
                self.infodir = {
                    let mut path = self.datarootdir.clone();
                    path.push(self.infodir);
                    path
                };
            }
            if !self.docdir.has_root() {
                self.docdir = {
                    let mut path = self.datarootdir.clone();
                    path.push(self.docdir);
                    path
                };
            }

            if !self.localedir.has_root() {
                self.localedir = {
                    let mut path = self.datarootdir.clone();
                    path.push(self.localedir);
                    path
                };
            }

            if !self.sharedstatedir.has_root() {
                self.sharedstatedir = {
                    let mut path = data_prefix.clone();
                    path.push(self.sharedstatedir);
                    path
                };
            }

            if !self.sysconfdir.has_root() {
                self.sysconfdir = if state_prefix.starts_with("/opt") {
                    let mut path = PathBuf::new();
                    path.push("/");
                    path.push(self.sysconfdir);
                    path.push(state_prefix.clone());
                    path
                } else {
                    let mut path = state_prefix.clone();
                    path.push(self.sysconfdir);
                    path
                }
            }

            if !self.localstatedir.has_root() {
                self.localstatedir = if state_prefix.starts_with("/opt") {
                    let mut path = PathBuf::new();
                    path.push("/");
                    path.push(self.localstatedir);
                    path.push(state_prefix.clone());
                    path
                } else {
                    let mut path = state_prefix.clone();
                    path.push(self.localstatedir);
                    path
                }
            }

            if !self.sharedstatedir.has_root() {
                self.sharedstatedir = {
                    let mut path = self.localstatedir.clone();
                    path.push(self.sharedstatedir);
                    path
                };
            }

            Ok(self)
        }
    }

    pub fn canonicalize_dir<S: AsRef<OsStr>+?Sized,T: Into<PathBuf>>(base: &S,dir: T)->PathBuf{
        let mut dir = dir.into();
        if !dir.has_root(){
             dir = {
                let mut path = PathBuf::from(base);
                path.push(dir);
                path
             }
        }
        dir
    }

    pub fn read_env(&mut self) {
        if let Ok(dir) = std::env::var("prefix") {
            self.prefix = dir.into()
        }

        if let Ok(dir) = std::env::var("exec_prefix") {
            self.exec_prefix = dir.into()
        }

        if let Ok(dir) = std::env::var("bindir") {
            self.bindir = dir.into()
        }

        if let Ok(dir) = std::env::var("libdir") {
            self.libdir = dir.into()
        }

        if let Ok(dir) = std::env::var("sbindir") {
            self.sbindir = dir.into()
        }
        if let Ok(dir) = std::env::var("libexecdir") {
            self.libexecdir = dir.into()
        }
        if let Ok(dir) = std::env::var("includedir") {
            self.includedir = dir.into()
        }

        if let Ok(dir) = std::env::var("datarootdir") {
            self.datarootdir = dir.into()
        }

        if let Ok(dir) = std::env::var("datadir") {
            self.datadir = dir.into()
        }

        if let Ok(dir) = std::env::var("mandir") {
            self.mandir = dir.into()
        }

        if let Ok(dir) = std::env::var("docdir") {
            self.docdir = dir.into()
        }

        if let Ok(dir) = std::env::var("infodir") {
            self.infodir = dir.into()
        }

        if let Ok(dir) = std::env::var("localedir") {
            self.localedir = dir.into()
        }

        if let Ok(dir) = std::env::var("sharedstatedir") {
            self.sharedstatedir = dir.into()
        }

        if let Ok(dir) = std::env::var("localstatedir") {
            self.localstatedir = dir.into()
        }

        if let Ok(dir) = std::env::var("runstatedir") {
            self.runstatedir = dir.into()
        }

        if let Ok(dir) = std::env::var("sysconfdir") {
            self.sysconfdir = dir.into()
        }
    }

    ///
    /// Obtains an iterator suitable for passing to [`std::process::Command::envs`].
    /// The resulting iterator contains each field and the value of that field.
    /// The order which the Items are encounted is unspecified
    ///
    /// ## Example
    ///
    /// ```
    /// use install_dirs::dirs::InstallDirs;
    /// use std::process::{Command, Stdio};
    /// let dirs = InstallDirs::defaults();
    /// let cmd = Command::new("printenv")
    ///     .stdin(Stdio::null())
    ///     .stdout(Stdio::inherit())
    ///     .stderr(Stdio::null())
    ///     .env_clear()
    ///     .envs(dirs.as_env())
    ///     .spawn()
    ///     .expect("printenv failed to start");
    /// ```
    pub fn as_env(&self) -> impl IntoIterator<Item = (&str, &Path)> {
        let mut map = HashMap::new();
        map.insert("prefix", &*self.prefix);
        map.insert("exec_prefix", &*self.exec_prefix);
        map.insert("bindir", &*self.bindir);
        map.insert("sbindir", &*self.sbindir);
        map.insert("libdir", &*self.libdir);
        map.insert("libexecdir", &*self.libexecdir);
        map.insert("datarootdir", &*self.datarootdir);
        map.insert("datadir", &*self.datadir);
        map.insert("docdir", &*self.docdir);
        map.insert("mandir", &*self.mandir);
        map.insert("infodir", &*self.infodir);
        map.insert("localedir", &*self.localedir);
        map.insert("sharedstatedir", &*self.sharedstatedir);
        map.insert("localstatedir", &*self.localstatedir);
        map.insert("runstatedir", &*self.runstatedir);
        map.insert("sysconfdir", &*self.sysconfdir);
        map
    }
}

///
/// Parses the compile-time environment into an instance of InstallDirs.
/// Note: This returns an owning structure and is not const.
/// Likely you will want to either store this, or it's canonical representation,
/// Inside a lazy_static!.
///
/// This uses the default installation configuration, see [`InstallDirs::defaults()`]
/// If a package name is specified as an expression, it uses the defaults for that package name, [`InstallDirs::with_project_name()`].
#[macro_export]
macro_rules! parse_env {
    () => {{
        let mut dirs = InstallDirs::defaults();
        if let Some(dir) = std::option_env!("prefix") {
            dirs.prefix = dir.into();
        }

        if let Some(dir) = std::option_env!("exec_prefix") {
            dirs.exec_prefix = dir.into();
        }

        if let Some(dir) = std::option_env!("bindir") {
            dirs.bindir = dir.into();
        }

        if let Some(dir) = std::option_env!("sbindir") {
            dirs.sbindir = dir.into();
        }
        if let Some(dir) = std::option_env!("libdir") {
            dirs.libdir = dir.into();
        }

        if let Some(dir) = std::option_env!("libexecdir") {
            dirs.libexecdir = dir.into();
        }

        if let Some(dir) = std::option_env!("includedir") {
            dirs.includedir = dir.into();
        }

        if let Some(dir) = std::option_env!("datarootdir") {
            dirs.datarootdir = dir.into();
        }

        if let Some(dir) = std::option_env!("datadir") {
            dirs.datadir = dir.into();
        }

        if let Some(dir) = std::option_env!("mandir") {
            dirs.mandir = dir.into();
        }

        if let Some(dir) = std::option_env!("docdir") {
            dirs.docdir = dir.into();
        }

        if let Some(dir) = std::option_env!("infodir") {
            dirs.infodir = dir.into();
        }

        if let Some(dir) = std::option_env!("localedir") {
            dirs.localedir = dir.into();
        }

        if let Some(dir) = std::option_env!("sharedstatedir") {
            dirs.sharedstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("localstatedir") {
            dirs.localstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("runstatedir") {
            dirs.runstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("sysconfdir") {
            dirs.sysconfdir = dir.into();
        }

        dirs
    }};
    ($project:expr) => {{
        let mut dirs = InstallDirs::with_project_name($project);
        if let Some(dir) = std::option_env!("prefix") {
            dirs.prefix = dir.into();
        }

        if let Some(dir) = std::option_env!("exec_prefix") {
            dirs.exec_prefix = dir.into();
        }

        if let Some(dir) = std::option_env!("bindir") {
            dirs.bindir = dir.into();
        }

        if let Some(dir) = std::option_env!("sbindir") {
            dirs.sbindir = dir.into();
        }
        if let Some(dir) = std::option_env!("libdir") {
            dirs.libdir = dir.into();
        }

        if let Some(dir) = std::option_env!("libexecdir") {
            dirs.libexecdir = dir.into();
        }

        if let Some(dir) = std::option_env!("includedir") {
            dirs.includedir = dir.into();
        }

        if let Some(dir) = std::option_env!("datarootdir") {
            dirs.datarootdir = dir.into();
        }

        if let Some(dir) = std::option_env!("datadir") {
            dirs.datadir = dir.into();
        }

        if let Some(dir) = std::option_env!("mandir") {
            dirs.mandir = dir.into();
        }

        if let Some(dir) = std::option_env!("docdir") {
            dirs.docdir = dir.into();
        }

        if let Some(dir) = std::option_env!("infodir") {
            dirs.infodir = dir.into();
        }

        if let Some(dir) = std::option_env!("localedir") {
            dirs.localedir = dir.into();
        }

        if let Some(dir) = std::option_env!("sharedstatedir") {
            dirs.sharedstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("localstatedir") {
            dirs.localstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("runstatedir") {
            dirs.runstatedir = dir.into();
        }

        if let Some(dir) = std::option_env!("sysconfdir") {
            dirs.sysconfdir = dir.into();
        }

        dirs
    }};
}

pub fn from_env() -> InstallDirs {
    let mut dirs = InstallDirs::defaults();
    dirs.read_env();
    dirs
}
