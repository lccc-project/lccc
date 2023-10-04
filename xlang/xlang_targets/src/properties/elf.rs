use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};

use super::{
    clever::{CLEVER, CLEVERILP32_PRIMITIVES, CLEVER_PRIMITIVES},
    holeybytes,
    w65::{W65, W65_PRIMITIVES},
    x86::{
        I386, I486, I586, I686, I86, X32_PRIMITIVES, X86_16_FAR_DATA_NEAR_FN_PRIMITIVES,
        X86_16_FAR_PRIMITIVES, X86_16_NEAR_DATA_FAR_FN_PRIMITIVES, X86_16_NEAR_PRIMITIVES,
        X86_32_PRIMITIVES, X86_64, X86_64_PRIMITIVES,
    },
    LinkProperties, OperatingSystemProperties, TargetProperties,
};

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

pub static X86_64_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib64")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-x86_64.so"),
    obj_binfmt: const_sv!("elf64-x86_64"),
    lib_binfmt: const_sv!("elf64-x86_64"),
    exec_binfmt: const_sv!("elf64-x86_64"),
};

pub static X32_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("libx32")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-x32.so"),
    obj_binfmt: const_sv!("elf32-x86_64"),
    lib_binfmt: const_sv!("elf32-x86_64"),
    exec_binfmt: const_sv!("elf32-x86_64"),
};

pub static X86_32_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib32")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld.so"),
    obj_binfmt: const_sv!("elf32-x86"),
    lib_binfmt: const_sv!("elf32-x86"),
    exec_binfmt: const_sv!("elf32-x86"),
};

pub static X86_16_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib16")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],

    available_formats: span![],
    interp: const_sv!(""),
    obj_binfmt: const_sv!("elf32-x86_16"),
    lib_binfmt: const_sv!("elf32-x86_16"),
    exec_binfmt: const_sv!("elf32-x86_16"),
};

macro_rules! x86_abis{
    {
        $($group:ident : {
            $($group_names:literal: $group_ref:expr;)*
        })*
    } => {
        $(
            mod $group {
                use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};
                use crate::properties::{TargetProperties};
                use super::*;
                pub static ABIS: Span<Pair<StringView,&TargetProperties>> = span![
                    $(Pair(const_sv!($group_names),&$group_ref),)*
                ];
            }
        )*
    }
}

x86_abis! {
    x86_64: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
        "x32": X32_ELF;
    }
    x86_32: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
    }
    x86_16_near: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
        "near": I86_NEAR_ELF;
        "far": I86_FAR_ELF;
        "near-data": I86_NEAR_ELF;
        "far-data": I86_FAR_DATA_NEAR_FN_ELF;
        "near-fn": I86_NEAR_ELF;
        "far-fn": I86_NEAR_DATA_FAR_FN_ELF;
    }
    x86_16_far: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
        "near": I86_NEAR_ELF;
        "far": I86_FAR_ELF;
        "near-data": I86_NEAR_DATA_FAR_FN_ELF;
        "far-data": I86_FAR_ELF;
        "near-fn": I86_FAR_DATA_NEAR_FN_ELF;
        "far-fn": I86_FAR_ELF;
    }
    x86_16_near_data_far_fn: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
        "near": I86_NEAR_ELF;
        "far": I86_FAR_ELF;
        "near-data": I86_NEAR_DATA_FAR_FN_ELF;
        "far-data": I86_FAR_ELF;
        "near-fn": I86_FAR_DATA_NEAR_FN_ELF;
        "far-fn": I86_NEAR_DATA_FAR_FN_ELF;
    }
    x86_16_far_data_near_fn: {
        "64": X86_64_ELF;
        "386": I386_ELF;
        "486": I386_ELF;
        "586": I586_ELF;
        "686": I686_ELF;
        "32": I686_ELF;
        "near": I86_NEAR_ELF;
        "far": I86_FAR_ELF;
        "near-data": I86_NEAR_DATA_FAR_FN_ELF;
        "far-data": I86_FAR_DATA_NEAR_FN_ELF;
        "near-fn": I86_FAR_DATA_NEAR_FN_ELF;
        "far-fn": I86_FAR_ELF;
    }
}

pub static X86_64_ELF: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    arch: &X86_64,
    os: &BARE_ELF,
    link: &X86_64_ELF_LINK,
    abis: x86_64::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("SysV64"),
    default_tag_name: const_sv!("SysV64"),
};

pub static X32_ELF: TargetProperties = TargetProperties {
    primitives: &X32_PRIMITIVES,
    arch: &X86_64,
    os: &BARE_ELF,
    link: &X32_ELF_LINK,
    abis: x86_64::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("SysV64"),
    default_tag_name: const_sv!("SysV64"),
};

pub static I386_ELF: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    arch: &I386,
    os: &BARE_ELF,
    link: &X86_32_ELF_LINK,
    abis: x86_32::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static I486_ELF: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    arch: &I486,
    os: &BARE_ELF,
    link: &X86_32_ELF_LINK,
    abis: x86_32::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};
pub static I586_ELF: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    arch: &I586,
    os: &BARE_ELF,
    link: &X86_32_ELF_LINK,
    abis: x86_32::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};
pub static I686_ELF: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    arch: &I686,
    os: &BARE_ELF,
    link: &X86_32_ELF_LINK,
    abis: x86_32::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static I86_NEAR_ELF: TargetProperties = TargetProperties {
    primitives: &X86_16_NEAR_PRIMITIVES,
    arch: &I86,
    os: &BARE_ELF,
    link: &X86_16_ELF_LINK,
    abis: x86_16_near::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static I86_FAR_ELF: TargetProperties = TargetProperties {
    primitives: &X86_16_FAR_PRIMITIVES,
    arch: &I86,
    os: &BARE_ELF,
    link: &X86_16_ELF_LINK,
    abis: x86_16_far::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static I86_NEAR_DATA_FAR_FN_ELF: TargetProperties = TargetProperties {
    primitives: &X86_16_NEAR_DATA_FAR_FN_PRIMITIVES,
    arch: &I86,
    os: &BARE_ELF,
    link: &X86_16_ELF_LINK,
    abis: x86_16_near_data_far_fn::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static I86_FAR_DATA_NEAR_FN_ELF: TargetProperties = TargetProperties {
    primitives: &X86_16_FAR_DATA_NEAR_FN_PRIMITIVES,
    arch: &I86,
    os: &BARE_ELF,
    link: &X86_16_ELF_LINK,
    abis: x86_16_far_data_near_fn::ABIS,
    enabled_features: span![],
    system_tag_name: const_sv!("cdecl"),
    default_tag_name: const_sv!("cdecl"),
};

pub static CLEVER_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib64")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-clever.so"),
    obj_binfmt: const_sv!("elf64-clever"),
    lib_binfmt: const_sv!("elf64-clever"),
    exec_binfmt: const_sv!("elf64-clever"),
};

pub static CLEVERILP32_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("libilp32")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-cleverilp32.so"),
    obj_binfmt: const_sv!("elf32-clever"),
    lib_binfmt: const_sv!("elf32-clever"),
    exec_binfmt: const_sv!("elf32-clever"),
};

static CLEVER_ABIS: Span<Pair<StringView, &TargetProperties>> = span![
    Pair(const_sv!("64"), &CLEVER_ELF),
    Pair(const_sv!("ilp32"), &CLEVERILP32_ELF)
];

pub static CLEVER_ELF: TargetProperties = TargetProperties {
    primitives: &CLEVER_PRIMITIVES,
    arch: &CLEVER,
    os: &BARE_ELF,
    link: &CLEVER_ELF_LINK,
    abis: CLEVER_ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("C"),
    system_tag_name: const_sv!("C"),
};

pub static CLEVERILP32_ELF: TargetProperties = TargetProperties {
    primitives: &CLEVERILP32_PRIMITIVES,
    arch: &CLEVER,
    os: &BARE_ELF,
    link: &CLEVERILP32_ELF_LINK,
    abis: CLEVER_ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("C"),
    system_tag_name: const_sv!("C"),
};

pub static W65_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!(""),
    obj_binfmt: const_sv!("elf32-w65"),
    lib_binfmt: const_sv!("elf32-w65"),
    exec_binfmt: const_sv!("elf32-w65"),
};

pub static W65_ELF: TargetProperties = TargetProperties {
    primitives: &W65_PRIMITIVES,
    arch: &W65,
    os: &BARE_ELF,
    link: &W65_ELF_LINK,
    abis: span![],
    enabled_features: span![],
    default_tag_name: const_sv!("C"),
    system_tag_name: const_sv!("C"),
};

pub static HOLEYBYTES_ELF_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib")],
    default_libs: span![],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-clever.so"),
    obj_binfmt: const_sv!("elf64-holeybytes"),
    lib_binfmt: const_sv!("elf64-holeybytes"),
    exec_binfmt: const_sv!("elf64-holeybytes"),
};

pub static HOLEYBYTES_ELF: TargetProperties = TargetProperties {
    primitives: &holeybytes::PRIMITIVES,
    arch: &holeybytes::HOLEYBYTES,
    os: &BARE_ELF,
    link: &HOLEYBYTES_ELF_LINK,
    enabled_features: span![],
    abis: span![],
    default_tag_name: const_sv!("C"),
    system_tag_name: const_sv!("C"),
};
