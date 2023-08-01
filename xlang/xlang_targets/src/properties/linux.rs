use xlang_abi::{const_sv, span, string::StringView};

use super::{
    x86::{
        I386, I486, I586, I686, X32_PRIMITIVES, X86_32_PRIMITIVES, X86_64, X86_64_PRIMITIVES,
        X86_64_V2, X86_64_V3, X86_64_V4,
    },
    LinkProperties, OperatingSystemProperties, TargetProperties,
};

pub static LINUX: OperatingSystemProperties = OperatingSystemProperties {
    is_unix_like: true,
    is_windows_like: false,
    os_family: span![const_sv!("linux"), const_sv!("unix")],
    static_prefix: const_sv!("lib"),
    static_suffix: const_sv!(".a"),
    shared_prefix: const_sv!("lib"),
    shared_suffix: const_sv!(".so"),
    exec_suffix: StringView::empty(),
    obj_suffix: const_sv!(".o"),
    ld_flavour: super::LinkerFlavor::Ld,
    ar_flavour: super::ArchiverFlavor::Ar,
    base_dirs: span![const_sv!("/"), const_sv!("/usr"), const_sv!("/usr/local")],
    so_kind: super::SharedLibraryStyle::Linkable,
};

pub static X86_64_LINUX_GNU_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib64")],
    default_libs: span![const_sv!("c")],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-linux-x86-64.so.2"),
    obj_binfmt: const_sv!("elf64-x86_64"),
    lib_binfmt: const_sv!("elf64-x86_64"),
    exec_binfmt: const_sv!("elf64-x86_64"),
};

pub static X86_64_LINUX_MUSL_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib64")],
    default_libs: span![const_sv!("c")],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-musl-x86-64.so"),
    obj_binfmt: const_sv!("elf64-x86_64"),
    lib_binfmt: const_sv!("elf64-x86_64"),
    exec_binfmt: const_sv!("elf64-x86_64"),
};

pub static X32_LINUX_GNU_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("libx32")],
    default_libs: span![const_sv!("c")],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-linux-x32.so.2"),
    obj_binfmt: const_sv!("elf32-x86"),
    lib_binfmt: const_sv!("elf32-x86"),
    exec_binfmt: const_sv!("elf32-x86"),
};

pub static X86_32_LINUX_GNU_LINK: LinkProperties = LinkProperties {
    libdirs: span![const_sv!("lib"), const_sv!("lib32")],
    default_libs: span![const_sv!("c")],
    startfiles: span![const_sv!("crt1.o"), const_sv!("crti.o")],
    endfiles: span![const_sv!("crtn.o")],
    available_formats: span![],
    interp: const_sv!("ld-linux.so.2"),
    obj_binfmt: const_sv!("elf32-x86_64"),
    lib_binfmt: const_sv!("elf32-x86_64"),
    exec_binfmt: const_sv!("elf32-x86_64"),
};

macro_rules! x86_abis{
    {
        $($os_abi:ident: {$($group:ident : {
            $($group_names:literal: $group_ref:expr;)*
        })*})*
    } => {
        $(mod $os_abi{$(
            pub mod $group {
                use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};
                use crate::properties::{TargetProperties};
                use super::super::*;
                pub static ABIS: Span<Pair<StringView,&TargetProperties>> = span![
                    $(Pair(const_sv!($group_names),&$group_ref),)*
                ];
            }
        )*})*
    }
}

x86_abis! {
    gnu: {
        x86_64: {
            "64": X86_64_LINUX_GNU;
            "32": I686_LINUX_GNU;
            "x32": X32_LINUX_GNU;
        }
        x86_32: {
            "64": X86_64_LINUX_GNU;
            "32": I686_LINUX_GNU;
        }
    }
    musl: {
        x86_64: {
            "64": X86_64_LINUX_MUSL;
        }
    }
}

pub static X86_64_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64,
    link: &X86_64_LINUX_GNU_LINK,
    abis: gnu::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};

pub static X86_64_V2_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64_V2,
    link: &X86_64_LINUX_GNU_LINK,
    abis: gnu::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};

pub static X86_64_V3_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64_V3,
    link: &X86_64_LINUX_GNU_LINK,
    abis: gnu::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};

pub static X86_64_V4_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64_V4,
    link: &X86_64_LINUX_GNU_LINK,
    abis: gnu::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};

pub static X32_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X32_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64,
    link: &X32_LINUX_GNU_LINK,
    abis: gnu::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};

pub static I686_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    os: &LINUX,
    arch: &I686,
    link: &X86_32_LINUX_GNU_LINK,
    abis: gnu::x86_32::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("cdecl"),
    system_tag_name: const_sv!("cdecl"),
};

pub static I586_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    os: &LINUX,
    arch: &I586,
    link: &X86_32_LINUX_GNU_LINK,
    abis: gnu::x86_32::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("cdecl"),
    system_tag_name: const_sv!("cdecl"),
};
pub static I486_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    os: &LINUX,
    arch: &I486,
    link: &X86_32_LINUX_GNU_LINK,
    abis: gnu::x86_32::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("cdecl"),
    system_tag_name: const_sv!("cdecl"),
};
pub static I386_LINUX_GNU: TargetProperties = TargetProperties {
    primitives: &X86_32_PRIMITIVES,
    os: &LINUX,
    arch: &I386,
    link: &X86_32_LINUX_GNU_LINK,
    abis: gnu::x86_32::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("cdecl"),
    system_tag_name: const_sv!("cdecl"),
};

pub static X86_64_LINUX_MUSL: TargetProperties = TargetProperties {
    primitives: &X86_64_PRIMITIVES,
    os: &LINUX,
    arch: &X86_64,
    link: &X86_64_LINUX_MUSL_LINK,
    abis: musl::x86_64::ABIS,
    enabled_features: span![],
    default_tag_name: const_sv!("SysV64"),
    system_tag_name: const_sv!("SysV64"),
};
