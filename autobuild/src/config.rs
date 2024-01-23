use std::{collections::HashMap, convert::TryFrom, ffi::OsString, path::PathBuf, str::FromStr};

use install_dirs::dirs::InstallDirs;
use serde_derive::{Deserialize, Serialize};

use target_tuples::{Target, UnknownError};

use crate::hash::{self, FileHash};
use crate::programs::rustc::RustcVersion;
use crate::rand::Rand;

#[derive(Clone, Debug)]
pub enum ConfigVarValue {
    Set,
    Unset,
    Value(String),
}

impl serde::Serialize for ConfigVarValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ConfigVarValue::Set => serializer.serialize_bool(true),
            ConfigVarValue::Unset => serializer.serialize_bool(false),
            ConfigVarValue::Value(v) => serializer.serialize_str(v),
        }
    }
}

impl<'de> serde::Deserialize<'de> for ConfigVarValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> serde::de::Visitor<'de> for ValueVisitor {
            type Value = ConfigVarValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string or a boolean")
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v {
                    Ok(ConfigVarValue::Set)
                } else {
                    Ok(ConfigVarValue::Unset)
                }
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ConfigVarValue::Value(v))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ConfigVarValue::Value(v.to_string()))
            }
        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConfigInstallDirs {
    #[serde(flatten)]
    pub install_dirs: InstallDirs,
    #[serde(flatten)]
    pub rest: HashMap<String, PathBuf>,
}

mod serde_target {
    use serde::{
        de::{self, Expected},
        Deserialize, Deserializer, Serializer,
    };
    use target_tuples::Target;

    pub fn serialize<S>(targ: &Target, ser: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        ser.serialize_str(targ.get_name())
    }

    pub fn deserialize<'de, D>(de: D) -> Result<Target, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExpectedTarget;

        impl de::Expected for ExpectedTarget {
            fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("Expected a target in the form <arch>-<sys> or <arch>-<vendor>-<sys> with sys being one of <os>, <env>, <objfmt> or <os> followed by either <env> or <objfmt>")
            }
        }
        let ty = <&str>::deserialize(de)?;

        ty.parse().map_err(|e| {
            <D::Error as de::Error>::invalid_value(de::Unexpected::Str(ty), &ExpectedTarget)
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Deserialize, Serialize)]
pub struct ConfigTargets {
    #[serde(with = "serde_target")]
    pub build: Target,
    #[serde(with = "serde_target")]
    pub host: Target,
    #[serde(with = "serde_target")]
    pub target: Target,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConfigFoundProgram {
    pub location: PathBuf,
    #[serde(flatten)]
    pub info: Option<ConfigProgramInfo>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ConfigProgramInfo {
    Rustc(RustcVersion),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConfigData {
    pub src_dir: PathBuf,
    pub dirs: ConfigInstallDirs,
    pub env: HashMap<String, String>,
    pub programs: HashMap<String, ConfigFoundProgram>,
    pub targets: ConfigTargets,
    pub file_cache: HashMap<String, FileHash>,
    pub config_vars: HashMap<String, ConfigVarValue>,
    pub global_key: FileHash,
}

impl ConfigData {
    pub fn new(src_dir: PathBuf, dirs: ConfigInstallDirs, targets: ConfigTargets) -> Self {
        Self {
            src_dir,
            dirs,
            env: HashMap::new(),
            programs: HashMap::new(),
            targets,
            file_cache: HashMap::new(),
            config_vars: HashMap::new(),
            global_key: FileHash::ZERO,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExtraConfigDirs {
    #[serde(flatten)]
    pub dirs: HashMap<String, ConfigDirSpec>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ConfigDirSpec {
    Plain(String),
    Separator(Vec<String>),
}

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
#[serde(default)]
pub struct ProgramSpec {
    #[serde(rename = "type")]
    pub ty: Option<ProgramType>,
    pub names: Vec<OsString>,
    pub target: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum ProgramType {
    Rustc,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BuildTargets {
    pub groups: HashMap<String, GroupSpec>,
    #[serde(flatten)]
    pub targets: HashMap<String, TargetSpec>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupSpec {}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TargetSpec {}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Manifest {
    pub dirs: ExtraConfigDirs,
    pub programs: HashMap<String, ProgramSpec>,
    pub target: BuildTargets,
}

use std::io;

#[derive(Clone, Debug)]
pub struct Config {
    data: Box<ConfigData>,
    manifests: HashMap<String, Manifest>,
    rand: Rand,
}

impl Config {
    pub fn new(mut data: Box<ConfigData>) -> Self {
        let mut rand = Rand::init();
        Self {
            data,
            manifests: HashMap::new(),
            rand,
        }
    }

    pub fn check_up_to_date(&mut self, file: String) -> io::Result<bool> {
        let mut buf = self.data.src_dir.clone();
        buf.push(&file);

        todo!()
    }
}
