use xlang_abi::prelude::v1::*;

use target_tuples::{Architecture, Environment, ObjectFormat, Vendor, OS};

use core::hash::Hash;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Target {
    name: String,
    arch: Architecture,
    vendor: Vendor,
    os: OS,
    env: Environment,
    of: ObjectFormat,
}

impl Hash for Target {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.arch.hash(state);
        self.vendor.hash(state);
        self.os.hash(state);
        self.env.hash(state);
        self.of.hash(state);
    }
}

impl PartialEq for Target {
    fn eq(&self, other: &Self) -> bool {
        self.arch == other.arch
            && self.vendor == other.vendor
            && self.os == other.os
            && self.env == other.env
            && self.of == other.of
    }
}

impl From<target_tuples::Target> for Target {
    fn from(t: target_tuples::Target) -> Self {
        Self {
            name: t.get_name().into(),
            arch: t.arch(),
            vendor: t.vendor(),
            os: t.operating_system().unwrap_or(OS::Null),
            env: t.environment().unwrap_or(Environment::Null),
            of: t.object_format().unwrap_or(ObjectFormat::Null),
        }
    }
}

impl From<Target> for target_tuples::Target {
    fn from(t: Target) -> Self {
        Self::parse(&t.name)
    }
}

impl From<&Target> for target_tuples::Target {
    fn from(t: &Target) -> Self {
        Self::parse(&t.name)
    }
}

impl From<&mut Target> for target_tuples::Target {
    fn from(t: &mut Target) -> Self {
        Self::parse(&t.name)
    }
}

pub mod properties;
