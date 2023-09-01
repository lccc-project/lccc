use std::{collections::HashMap, convert::TryFrom, ffi::OsString, path::PathBuf, str::FromStr};

use install_dirs::dirs::InstallDirs;
use serde_derive::{Deserialize, Serialize};

use target_tuples::{Target, UnknownError};

use crate::dep::FileHash;
use crate::programs::rustc::RustcVersion;

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
    pub source_manifest: PathBuf,
    pub bin_dir: PathBuf,
    pub dirs: ConfigInstallDirs,
    pub env: HashMap<String, String>,
    pub programs: HashMap<String, ConfigFoundProgram>,
    pub targets: ConfigTargets,
    pub dep_files: HashMap<String, FileHash>,
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
pub enum ProgramType {}

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
