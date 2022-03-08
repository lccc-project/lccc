use xlang_abi::{const_sv, span};

use super::{clever::CLEVER, x86::X86_64, OperatingSystemProperties, TargetProperties};

pub static BARE_ELF: OperatingSystemProperties = OperatingSystemProperties {
    is_unix_like: false,
    is_windows_like: false,
    os_family: span![],
    static_prefix: const_sv!("lib"),
    static_suffix: const_sv!(".a"),
    shared_prefix: const_sv!("lib"),
    shared_suffix: const_sv!(".so"),
    exec_suffix: const_sv!(""),
    obj_suffix: const_sv!(".o"),
    ld_flavour: super::LinkerFlavor::Ld,
    ar_flavour: super::ArchiverFlavor::Ar,
    base_dirs: span![const_sv!("/")],
    so_kind: super::SharedLibraryStyle::Linkable,
};

pub static X86_64_ELF: TargetProperties = TargetProperties {
    intbits: 32,
    longbits: 64,
    llongbits: 64,
    ptrbits: 64,
    max_align: 16,
    ptralign: 8,
    intmaxbits: 8,
    sizebits: 8,
    lock_free_atomic_mask: 0xffff,
    ldbl_align: 16,
    ldbl_format: super::LongDoubleFormat::X87,
    arch: &X86_64,
    os: &BARE_ELF,
    libdirs: span![const_sv!("lib"), const_sv!("lib64")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    enabled_features: span![],
    available_formats: span![],
    interp: const_sv!("ld-x86_64.so"),
};

pub static CLEVER_ELF: TargetProperties = TargetProperties {
    intbits: 32,
    longbits: 64,
    llongbits: 64,
    ptrbits: 64,
    max_align: 16,
    ptralign: 8,
    intmaxbits: 8,
    sizebits: 8,
    lock_free_atomic_mask: 0xff,
    ldbl_align: 8,
    ldbl_format: super::LongDoubleFormat::IEEE64,
    arch: &CLEVER,
    os: &BARE_ELF,
    libdirs: span![const_sv!("lib")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    enabled_features: span![],
    available_formats: span![],
    interp: const_sv!("ld-clever.so"),
};
