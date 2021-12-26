use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};

use crate::properties::MachineProperties;

use super::ArchProperties;

macro_rules! x86_machines{
    {
        $(($mach:ident, $name:literal $(| $alias:literal)* , [$($feature:literal),* $(,)?])),* $(,)?
    } => {
        mod machines{
            $(pub static $mach: super::MachineProperties = super::MachineProperties{
                default_features: xlang_abi::span![$(xlang_abi::const_sv!($feature)),*]
            };)*
        }

        pub static X86_MACHINES: Span<'static,Pair<StringView<'static>,&'static MachineProperties>> = span![
            $(Pair(const_sv!($name),&machines:: $mach) $(, Pair(const_sv!($alias),&machines:: $mach))*),*
        ];
    }
}

macro_rules! x86_builtins{
    [
        $($name:ident),* $(,)?
    ] => {
        pub const X86_BUILTINS: Span<'static,StringView<'static>> = span![
            $(const_sv!(::std::stringify!($name))),*
        ];
    }
}

x86_builtins![];

x86_machines! {
    (MX86_64, "x86_64", ["x87","fxsr","mmx","sce","sse","sse2"]),
    (MX86_64_V2, "x86_64-v2" | "x86_64v2", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3"]),
    (MX86_64_V3, "x86_64-v3" | "x86_64v3", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3","avx","avx2","bmi","bmi2","f16c","lzcnt","movbe","xsave"]),
    (MX86_64_V4, "x86_64-v4" | "x86_64v4", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3","avx",
        "avx2","bmi","bmi2","f16c","lzcnt","movbe","xsave","avx512f","avx512bw","avx512cd","avx512dq","avx512vl"]),
    (MI386, "i386", ["x87"]),
    (MI486, "i486", ["x87"]),
    (MI586, "i586" | "pentium", ["x87"]),
    (MLAKEMONT, "lakemont", ["x87"]),
    (MPENTIUM_MMX, "pentium-mmx", ["x87", "mmx"]),
    (MI686, "i686" | "pentium-pro", ["x87"]),
    (MPENTIUM2, "pentium2", ["x87", "mmx"]),
    (MPENTIUM3, "pentium3" | "pentium3m", ["x87", "mmx", "sse"]),
    (MPENTIUM_M, "pentium-m", ["x87","mmx","sse","sse2"]),
    (MPENTIUM4, "pentium4" | "pentium4m", ["x87","mmx","sse","sse2"]),
    (MPRESCOTT, "prescott", ["x87","mmx","sse","sse2","sse3"]),
    (MNOCONA, "nocona", ["x87", "mmx","sse","sse2", "sse3"]),
    (MCORE2, "core2", ["x87", "mmx", "sse", "sse2","sse3","ssse3"]),
    (MNELHALEM, "nelhalem", ["x87","mmx","sse", "sse2", "sse3", "ssse3","sse4.1","sse4.2","popcbt"])
}

pub const X86_FEATURES: Span<'static, StringView<'static>> = span![
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
    const_sv!("avx"),
    const_sv!("avx2"),
    const_sv!("avx512f"),
    const_sv!("avx512pf"),
    const_sv!("avx512er"),
    const_sv!("avx512cd"),
    const_sv!("avx512vl"),
    const_sv!("avx512bw"),
    const_sv!("avx512dq"),
    const_sv!("avx512ifma"),
    const_sv!("avx512vbmi"),
    const_sv!("aes"),
    const_sv!("sha"),
    const_sv!("pclmul"),
    const_sv!("clflushopt"),
    const_sv!("clwb"),
    const_sv!("fsgsbase"),
    const_sv!("ptwrite"),
    const_sv!("rdrand"),
    const_sv!("f16c"),
    const_sv!("fma"),
    const_sv!("pconfig"),
    const_sv!("wbnoinvd"),
    const_sv!("fma4"),
    const_sv!("prfchw"),
    const_sv!("rdpid"),
    const_sv!("prefetchwcl"),
    const_sv!("rdseed"),
    const_sv!("sgx"),
    const_sv!("xop"),
    const_sv!("lwp"),
    const_sv!("3dnow"),
    const_sv!("3dnowa"),
    const_sv!("popcnt"),
    const_sv!("abm"),
    const_sv!("adx"),
    const_sv!("bmi"),
    const_sv!("bmi2"),
    const_sv!("lzcnt"),
    const_sv!("xsave"),
    const_sv!("xsaveopt"),
    const_sv!("xsavec"),
    const_sv!("xsaves"),
    const_sv!("rtm"),
    const_sv!("hle"),
    const_sv!("tbm"),
    const_sv!("mwaitx"),
    const_sv!("clzero"),
    const_sv!("pku"),
    const_sv!("avx512vbmi2"),
    const_sv!("avx512bf16"),
    const_sv!("avx512fp16"),
    const_sv!("gfni"),
    const_sv!("vaes"),
    const_sv!("waitpkg"),
    const_sv!("vpclmulqdq"),
    const_sv!("avx512bitalg"),
    const_sv!("movdiri"),
    const_sv!("movdir64b"),
    const_sv!("enqcmd"),
    const_sv!("uintr"),
    const_sv!("tsxldtrk"),
    const_sv!("avx512vpopcntdq"),
    const_sv!("avx512vp2intersect"),
    const_sv!("avx5124fmaps"),
    const_sv!("avx512vnni"),
    const_sv!("avxvnni"),
    const_sv!("avx5124vnniw"),
    const_sv!("cldemote"),
    const_sv!("serialize"),
    const_sv!("amx-tile"),
    const_sv!("amx-int8"),
    const_sv!("amx-bf16"),
    const_sv!("hreset"),
    const_sv!("kl"),
    const_sv!("widekl"),
    const_sv!("sahf"),
    const_sv!("cx16"),
    const_sv!("movbe"),
    const_sv!("crc32"),
    const_sv!("mwait"),
];

pub static X86_64: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64,
};

pub static X86_64_V2: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V2,
};

pub static X86_64_V3: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V3,
};

pub static X86_64_V4: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V4,
};

pub static I386: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI386,
};

pub static I486: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI486,
};
pub static I586: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI586,
};

pub static I686: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI686,
};
