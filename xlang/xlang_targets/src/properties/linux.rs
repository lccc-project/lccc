use target_tuples::ObjectFormat;
use xlang_abi::{const_sv, span, string::StringView};

use super::{OperatingSystemProperties, TargetProperties};

pub static LINUX: OperatingSystemProperties = OperatingSystemProperties {
    is_unix_like: true,
    is_windows_like: false,
    os_family: span![const_sv!("linux"), const_sv!("linux")],
    static_prefix: const_sv!("lib"),
    static_suffix: const_sv!(".a"),
    shared_prefix: const_sv!("lib"),
    shared_suffix: const_sv!(".so"),
    exec_suffix: StringView::empty(),
    obj_suffix: const_sv!(".o"),
    ld_flavour: super::LinkerFlavor::Ld,
    ar_flavour: super::ArchiverFlavor::Ar,
    base_dirs: span![const_sv!("/"), const_sv!("/usr"), const_sv!("/usr/local")],
};

pub static X86_64_LINUX_GNU: TargetProperties = TargetProperties {
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
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    enabled_features: span![],
    available_formats: span![ObjectFormat::Elf, ObjectFormat::Coff],
    interp: const_sv!("/lib64/ld-linux-x86-64.so.2"),
};
