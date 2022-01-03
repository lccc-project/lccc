//!
//! Library to describe the behaviours of targets in xlang
//! Most properties of targets are exposed via this interface, and plugins (including frontends and backends), as well as drivers, should query the properties in this library,
//! rather than maintaining their own list of properties.
//!
//! This library also provides an ABI safe version of the [`target_tuples::Target`] type.

#![deny(warnings, missing_docs, clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::module_name_repetitions)]
use xlang_abi::prelude::v1::*;

use target_tuples::{Architecture, Environment, ObjectFormat, Vendor, OS};

use core::hash::Hash;

///
/// An abi safe version of [`target_tuples::Target`], which can be converted to and from that type
#[repr(C)]
#[derive(Clone, Debug)]
pub struct Target {
    /// The exact name of the target
    pub name: String,
    /// The canonical architecture of the target
    pub arch: Architecture,
    /// The canonical vendor of the target
    pub vendor: Vendor,
    /// The os portion (if any) of the system of the target
    pub os: OS,
    /// The environment portion (if any) of the system of the target
    pub env: Environment,
    /// The object format portion (if any) of the system of the target
    pub of: ObjectFormat,
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

impl From<&target_tuples::Target> for Target {
    fn from(t: &target_tuples::Target) -> Self {
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

/// Module containing structures and functions to query the properties of xlang targets
pub mod properties;
