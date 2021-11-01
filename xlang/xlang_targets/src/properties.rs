use xlang_abi::{pair::Pair, span::Span, string::StringView};


#[repr(C)]

pub struct MachineProperties {
    pub default_features: Span<'static, StringView<'static>>
}


#[repr(C)]
pub struct ArchProperties{
    pub lock_free_atomic_masks: u32,
    pub builtin_names: Span<'static,StringView<'static>>,
    pub target_features: Span<'static,StringView<'static>>,
    pub machines: Span<'static, Pair<StringView<'static>,&'static MachineProperties>>,
    pub default_machine: &'static MachineProperties,
}