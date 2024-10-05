use xlang::prelude::v1::*;
use xlang::targets::properties::*;

use super::clever::*;
use super::x86::*;

pub static CLEVER_LILIUM_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib")],
    default_libs: span![const_sv!("usi")],
    startfiles: span![const_sv!("libusi-init.a")],
    endfiles: span![],
    available_formats: span![],
    interp: const_sv!("ld-lilium-clever.so"),
    obj_binfmt: const_sv!("elf64-clever"),
    lib_binfmt: const_sv!("elf64-clever"),
    exec_binfmt: const_sv!("elf64-clever"),
    stack_attribute_control: StackAttributeControlStyle::NoExec,
    uwtable_method: UnwindStyle::Itanium,
};

pub static X86_64_LILIUM_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib")],
    default_libs: span![const_sv!("usi")],
    startfiles: span![const_sv!("libusi-init.a")],
    endfiles: span![],
    available_formats: span![],
    interp: const_sv!("ld-lilium-x86-64.so"),
    obj_binfmt: const_sv!("elf64-x86_64"),
    lib_binfmt: const_sv!("elf64-x86_64"),
    exec_binfmt: const_sv!("elf64-x86_64"),
    stack_attribute_control: StackAttributeControlStyle::NoExec,
    uwtable_method: UnwindStyle::Itanium,
};

pub static LILIUM: OperatingSystemProperties = OperatingSystemProperties {
    is_unix_like: false,
    is_windows_like: false,
    os_family: span![const_sv!("lilium")],
    static_prefix: const_sv!("lib"),
    static_suffix: const_sv!(".a"),
    shared_prefix: const_sv!("lib"),
    shared_suffix: const_sv!(".so"),
    exec_suffix: const_sv!(""),
    obj_suffix: const_sv!(".o"),
    ld_flavour: LinkerFlavor::Ld,
    ar_flavour: ArchiverFlavor::Ar,
    base_dirs: span![const_sv!("/"), const_sv!("/usr"), const_sv!("/usr/local")],
    so_kind: SharedLibraryStyle::Linkable,
};

pub static X86_64_LILIUM: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LILIUM,
    arch: &X86_64,
    link: &X86_64_LILIUM_LINK,
    abis: span![],
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
    custom_properties: span![],
};

pub static CLEVER_LILIUM: TargetProperties = TargetProperties {
    primitives: &CLEVER_PRIMITIVES,
    os: &LILIUM,
    arch: &CLEVER,
    link: &CLEVER_LILIUM_LINK,
    abis: span![],
    enabled_features: span![],
    default_tag_name: const_sv!("C"),
    system_tag_name: const_sv!("C"),
    custom_properties: span![],
};
