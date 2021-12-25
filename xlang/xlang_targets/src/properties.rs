use xlang_abi::{pair::Pair, span::Span, string::StringView};

use crate::Target;

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct MachineProperties {
    pub default_features: Span<'static, StringView<'static>>,
}

#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LinkerFlavor {
    MsLink,
    Ld,
    MacosLd64,
}

#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ArchiverFlavor {
    Ar,
    MsLib,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ArchProperties {
    pub lock_free_atomic_masks: u32,
    pub builtin_names: Span<'static, StringView<'static>>,
    pub target_features: Span<'static, StringView<'static>>,
    pub machines: Span<'static, Pair<StringView<'static>, &'static MachineProperties>>,
    pub default_machine: &'static MachineProperties,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct OperatingSystemProperties {
    pub is_unix_like: bool,
    pub is_windows_like: bool,
    pub os_family: Span<'static, StringView<'static>>,
    pub static_prefix: StringView<'static>,
    pub static_suffix: StringView<'static>,
    pub shared_prefix: StringView<'static>,
    pub shared_suffix: StringView<'static>,
    pub exec_suffix: StringView<'static>, // Assume that executable outputs don't have a prefix/suffix
    pub obj_suffix: StringView<'static>,
    pub ld_flavour: LinkerFlavor,
    pub ar_flavour: ArchiverFlavor,
    pub base_dirs: Span<'static, StringView<'static>>,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct TargetProperties {
    pub intbits: u16,
    pub longbits: u16,
    pub llongbits: u16,
    pub ptrbits: u16,
    pub max_align: u16,
    pub ptralign: u16,
    pub intmaxbits: u16,
    pub lock_free_atomic_masks: u16,
    pub arch: &'static ArchProperties,
    pub os: &'static OperatingSystemProperties,
    pub libdirs: Span<'static, StringView<'static>>,
    pub default_libs: Span<'static, StringView<'static>>,
    pub startfiles: Span<'static, StringView<'static>>,
    pub endfiles: Span<'static, StringView<'static>>,
    pub enabled_features: Span<'static, Pair<StringView<'static>, bool>>,
    pub available_formats: Span<'static, target_tuples::ObjectFormat>,
    pub interp: StringView<'static>,
}

mod linux;
mod x86;

#[doc(hidden)]
#[deprecated(note = "internal interface that doesn't get linked in via xlang_interface")]
#[must_use]
pub fn __get_properties(targ: Target) -> &'static TargetProperties {
    target_tuples::match_targets! {
        match (targ.into()){
            x86_64-*-linux-gnu => &linux::X86_64_LINUX_GNU,
            * => todo!()
        }
    }
}

extern "C" {
    pub fn xlang_get_target_properties(targ: Target) -> &'static TargetProperties;
}

#[must_use]
pub fn get_properties(targ: Target) -> &'static TargetProperties {
    unsafe { xlang_get_target_properties(targ) }
}
