use target_tuples::ObjectFormat;
use xlang_abi::{const_sv, span};

use super::{OperatingSystemProperties, TargetProperties};

pub static LINUX: OperatingSystemProperties = OperatingSystemProperties {
    is_unix_like: true,
    is_windows_like: false,
    os_family: span![const_sv!("linux"), const_sv!("linux")],
};

pub static X86_64_LINUX: TargetProperties = TargetProperties {
    intbits: 32,
    longbits: 64,
    llongbits: 64,
    ptrbits: 64,
    max_align: 128,
    ptralign: 64,
    intmaxbits: 64,
    lock_free_atomic_masks: 0xff,
    arch: &super::x86::X86_64,
    os: &LINUX,
    libdirs: span![const_sv!("/lib"), const_sv!("/lib64")],
    default_libs: span![const_sv!("c")],
    startfiles: span![],
    enabled_features: span![],
    available_formats: span![ObjectFormat::Elf, ObjectFormat::Coff],
};
