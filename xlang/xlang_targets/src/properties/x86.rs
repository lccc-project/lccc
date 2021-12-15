use xlang_abi::{const_sv, pair::Pair, span::Span, string::StringView};

use crate::properties::MachineProperties;

use super::ArchProperties;

mod machines {
    use xlang_abi::{const_sv, span::Span};

    use crate::properties::MachineProperties;

    pub static MX86_64: MachineProperties = MachineProperties {
        default_features: Span::new(&[
            const_sv!("cmov"),
            const_sv!("cmpxchg8b"),
            const_sv!("x87"),
            const_sv!("mmx"),
            const_sv!("fxsr"),
            const_sv!("sce"),
            const_sv!("sse"),
            const_sv!("sse2"),
        ]),
    };
}

pub static X86_MACHINES: Span<'static, Pair<StringView<'static>, &'static MachineProperties>> =
    Span::new(&[Pair(const_sv!("x86_64"), &machines::MX86_64)]);

pub static X86_64: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFF,
    builtin_names: Span::new(&[]),
    target_features: Span::new(&[
        const_sv!("x87"),
        const_sv!("mmx"),
        const_sv!("sse"),
        const_sv!("sse2"),
        const_sv!("sse3"),
        const_sv!("ssse3"),
        const_sv!("sse4"),
        const_sv!("sse4a"),
        const_sv!("sse4.1"),
        const_sv!("sse4.2"),
    ]),
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64,
};
