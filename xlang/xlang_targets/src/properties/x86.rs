use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};

use crate::properties::MachineProperties;

use super::{
    ArchProperties, AsmProperties, AsmScalar,
    AsmScalarKind::{Float, Integer, Vector},
};

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

x86_builtins![
    _mm_add_pi16,
    _mm_add_pi32,
    _mm_add_pi8,
    _mm_adds_pi16,
    _mm_adds_pi8,
    _mm_adds_pu16,
    _mm_adds_pu8,
    _mm_and_si64,
    _mm_andnot_si64,
    _mm_cmpeq_pi16,
    _mm_cmpeq_pi32,
    _mm_cmpeq_pi8,
    _mm_cmpgt_pi16,
    _mm_cmpgt_pi32,
    _mm_cmpgt_pi8,
    _mm_cvtm64_si64,
    _mm_cvtsi32_si64,
    _m_empty,
    _mm_empty,
    _m_from_int,
    _m_from_int64,
    _mm_madd_pi16,
    _mm_mulhi_pi16,
    _mm_mullo_pi16,
    _mm_or_si64,
    _mm_packs_pi16,
    _mm_packs_pi32,
    _mm_packs_pu16,
    _m_packssdw,
    _m_packsswb,
    _m_packuswb,
    _m_paddb,
    _m_paddd,
    _m_paddsb,
    _m_paddsw,
    _m_paddusb,
    _m_paddusw,
    _m_paddw,
    _m_pand,
    _m_pandn,
    _m_pcmpeqb,
    _m_pcmpeqd,
    _m_pcmpeqw,
    _m_pcmpgtb,
    _m_pcmpgtd,
    _m_pcmpgtw,
    _m_paddwd,
    _m_pmulhw,
    _m_pullw,
];

x86_machines! {
    (MX86_64, "x86_64", ["x87","fxsr","mmx","sce","sse","sse2"]),
    (MX86_64_V2, "x86_64-v2" | "x86_64v2", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3"]),
    (MX86_64_V3, "x86_64-v3" | "x86_64v3", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3","avx","avx2","bmi","bmi2","f16c","lzcnt","movbe","xsave"]),
    (MX86_64_V4, "x86_64-v4" | "x86_64v4", ["x87","fxsr","mmx","sce","sse","sse2","cx16","sahf","popcnt","sse3","sse4.1","sse4.2","sse3","avx",
        "avx2","bmi","bmi2","f16c","lzcnt","movbe","xsave","avx512f","avx512bw","avx512cd","avx512dq","avx512vl"]),
    (MI86, "i86"|"i8086", []),
    (MI186, "i186", []),
    (MI286, "i286", ["x87"]),
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

macro_rules! x86_constraints{
    [
        $($base_arch:ident: [$($kind:ident @ $($sizes:literal)|* => $name:ident),* $(,)?])*
    ] => {
        mod constraints{
            use super::*;

            $(pub mod $base_arch{
                use super::*;
                pub const ASM_CONSTRAINTS: Span<'static, Pair<StringView<'static>, AsmScalar>> = span![
                    $($(Pair(const_sv!(::std::stringify!($name)),AsmScalar($kind,$sizes)),)*)*
                ];
            })*
        }
    }
}

macro_rules! x86_register_groups {
    [
        $($base_arch:ident: [$($name:ident => $($regname:ident)|*),* $(,)?])*
    ] => {
        mod reg_groups{
            use super::*;
            $(pub mod $base_arch{
                use super::*;
                pub const REGISTER_GROUPS: Span<'static, Pair<StringView<'static>,Span<'static,StringView<'static>>>> = span![
                    $(Pair(const_sv!(::std::stringify!($name)),span![$(const_sv!(::std::stringify!($regname))),*])),*
                ];
            })*
        }
    }
}

macro_rules! x86_overlaps {
    [ $($name:ident => $($overlap_names:ident)|*),* $(,)?] => {
        pub const X86_ASM_REGISTER_OVERLAPS: Span<'static, Pair<StringView<'static>,StringView<'static>>> = span![
            $($(Pair(const_sv!(::std::stringify!($name)),const_sv!(::std::stringify!($overlap_names)))),*),*
        ];
    }
}

x86_constraints![
    x86_16: [
        Integer @ 16 => reg,
        Integer @ 16 => reg_abcd,
        Integer @ 8 => reg_byte,
        Vector @ 128 => xmm_reg,
        Integer @ 8 | 16 | 32 | 64 | 128 => xmm_reg,
        Float @ 32 | 64 | 128 => xmm_reg,
        Vector @ 256 => ymm_reg,
        Vector @ 512 => zmm_reg,
        Float @ 32 | 64 | 80 => x87_reg,
        Integer @ 32 | 64 => mmx_reg,
        Vector @ 32 | 64 => mmx_reg,
        Integer @ 64 => kreg
    ]
    x86_32: [
        Integer @ 16 | 32 => reg,
        Integer @ 16 | 32 => reg_abcd,
        Integer @ 8 => reg_byte,
        Vector @ 128 => xmm_reg,
        Integer @ 8 | 16 | 32 | 64 | 128 => xmm_reg,
        Float @ 32 | 64 | 128 => xmm_reg,
        Vector @ 256 => ymm_reg,
        Vector @ 512 => zmm_reg,
        Float @ 32 | 64 | 80 => x87_reg,
        Integer @ 32 | 64 => mmx_reg,
        Vector @ 32 | 64 => mmx_reg,
        Integer @ 64 => kreg
    ]
    x86_64: [
        Integer @ 16 | 32 | 64 => reg,
        Integer @ 16 | 32 | 64 => reg_abcd,
        Integer @ 8 => reg_byte,
        Vector @ 128 => xmm_reg,
        Integer @ 8 | 16 | 32 | 64 | 128 => xmm_reg,
        Float @ 32 | 64 | 128 => xmm_reg,
        Vector @ 256 => ymm_reg,
        Vector @ 512 => zmm_reg,
        Vector @ 65536 => tmm_reg,
        Float @ 32 | 64 | 80 => x87_reg,
        Integer @ 32 | 64 => mmx_reg,
        Vector @ 32 | 64 => mmx_reg,
        Integer @ 64 => kreg,
    ]
];

x86_register_groups![
    x86_16: [
        reg => ax | cx | dx | si | di,
        reg_abcd => ax | cx | dx,
        reg_byte => al | cl | dl | ah | ch | dh,
        xmm_reg => xmm0 | xmm1 | xmm2 | xmm3 | xmm4 | xmm5 | xmm6 | xmm7,
        ymm_reg => ymm0 | ymm1 | ymm2 | ymm3 | ymm4 | ymm5 | ymm6 | ymm7,
        zmm_reg => zmm0 | zmm1 | zmm2 | zmm3 | zmm4 | zmm5 | zmm6 | zmm7,
        x87_reg => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
        mmx_reg => mm0 | mm1 | mm2 | mm3 | mm4 | mm5 | mm6 | mm7,
        kreg => k1 | k2 | k3 | k4 | k5 | k6 | k7,
        kreg0 => k0 | k1 | k2 | k3 | k4 | k5 | k6 | k7,
    ]
    x86_32: [
        reg => ax | cx | dx | bx | di
            | eax | ecx | edx | ebx | edi,
        reg_abcd => ax | cx | dx | bx
            | eax | ecx | edx | ebx,
        reg_byte => al | cl | dl | bl | ah | ch | dh | bh,
        xmm_reg => xmm0 | xmm1 | xmm2 | xmm3 | xmm4 | xmm5 | xmm6 | xmm7,
        ymm_reg => ymm0 | ymm1 | ymm2 | ymm3 | ymm4 | ymm5 | ymm6 | ymm7,
        zmm_reg => zmm0 | zmm1 | zmm2 | zmm3 | zmm4 | zmm5 | zmm6 | zmm7,
        x87_reg => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
        mmx_reg => mm0 | mm1 | mm2 | mm3 | mm4 | mm5 | mm6 | mm7,
        kreg => k1 | k2 | k3 | k4 | k5 | k6 | k7,
        kreg0 => k0 | k1 | k2 | k3 | k4 | k5 | k6 | k7,
    ]
    x86_64: [
        reg => ax | cx | dx | si | di
            | eax | ecx | edx | esi | edi
            | rax | rcx | rdx | rsi | rdi
            | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
            | r8d | r9d | r10d | r11d | r12d | r13d | r14d | r15d
            | r8w | r9w | r10w | r11w | r12w | r13w | r14w | r15w,
        reg_abcd => ax | cx | dx
            | eax | ecx | edx
            | rax | rcx | rdx,
        reg_byte => al | cl | dl | sil | dil | r8b | r9b | r10b | r11b | r12b | r13b | r14b | r15b,
        xmm_reg => xmm0 | xmm1 | xmm2 | xmm3 | xmm4 | xmm5 | xmm6 | xmm7
            | xmm8 | xmm9 | xmm10 | xmm11 | xmm12 | xmm13 | xmm14 | xmm15
            | xmm16 | xmm17 | xmm18 | xmm19 | xmm20 | xmm21 | xmm22 | xmm23
            | xmm24 | xmm25 | xmm26 | xmm27 | xmm28 | xmm29 | xmm30 | xmm31,
        ymm_reg => ymm0 | ymm1 | ymm2 | ymm3 | ymm4 | ymm5 | ymm6 | ymm7
            | ymm8 | ymm9 | ymm10 | ymm11 | ymm12 | ymm13 | ymm14 | ymm15
            | ymm16 | ymm17 | ymm18 | ymm19 | ymm20 | ymm21 | ymm22 | ymm23
            | ymm24 | ymm25 | ymm26 | ymm27 | ymm28 | ymm29 | ymm30 | ymm31,
        zmm_reg => zmm0 | zmm1 | zmm2 | zmm3 | zmm4 | zmm5 | zmm6 | zmm7
            | zmm8 | zmm9 | zmm10 | zmm11 | zmm12 | zmm13 | zmm14 | zmm15
            | zmm16 | zmm17 | zmm18 | zmm19 | zmm20 | zmm21 | zmm22 | zmm23
            | zmm24 | zmm25 | zmm26 | zmm27 | zmm28 | zmm29 | zmm30 | zmm31,
        tmm_reg => tmm0 | tmm1 | tmm2 | tmm3 | tmm4 | tmm5 | tmm6 | tmm7,
        x87_reg => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
        mmx_reg => mm0 | mm1 | mm2 | mm3 | mm4 | mm5 | mm6 | mm7,
        kreg => k1 | k2 | k3 | k4 | k5 | k6 | k7,
        kreg0 => k0 | k1 | k2 | k3 | k4 | k5 | k6 | k7,
    ]
];

x86_overlaps![
    rax => eax | ax | ah | al,
    eax => ax | ah | al,
     ax => ah | al,
    rcx => ecx | cx | ch | cl,
    ecx => cx | ch | cl,
     cx => ch | cl,
    rdx => edx | dx | dh | dl,
    edx => dx | dh | dl,
     dx => dh | dl,
    rbx => ebx | bx | bh | bl,
    ebx => bx | bh | bl,
     bx => bh | bl,
    rsi => esi | si | sil,
    esi => si | sil,
     si => sil,
    rdi => edi | di | dil,
    edi => di | dil,
     di => dil,

    r8  => r8d | r8w | r8b,
    r8d => r8w | r8b,
    r8w => r8b,
    r9  => r9d | r9w | r9b,
    r9d => r9w | r9b,
    r9w => r9b,
    r10  => r10d | r10w | r10b,
    r10d => r10w | r10b,
    r10w => r10b,
    r11  => r11d | r11w | r11b,
    r11d => r11w | r11b,
    r11w => r11b,
    r12  => r12d | r12w | r12b,
    r12d => r12w | r12b,
    r12w => r12b,
    r13  => r13d | r13w | r13b,
    r13d => r13w | r13b,
    r13w => r13b,
    r14  => r14d | r14w | r14b,
    r14d => r14w | r14b,
    r14w => r14b,
    r15  => r15d | r15w | r15b,
    r15d => r15w | r15b,
    r15w => r15b,

    zmm0 => ymm0 | xmm0,
    ymm0 => xmm0,
    zmm1 => ymm1 | xmm1,
    ymm1 => xmm1,
    zmm2 => ymm2 | xmm2,
    ymm2 => xmm2,
    zmm3 => ymm3 | xmm3,
    ymm3 => xmm3,
    zmm4 => ymm4 | xmm4,
    ymm4 => xmm4,
    zmm5 => ymm5 | xmm5,
    ymm5 => xmm5,
    zmm6 => ymm6 | xmm6,
    ymm6 => xmm6,
    zmm7 => ymm7 | xmm7,
    ymm7 => xmm7,
    zmm8 => ymm8 | xmm8,
    ymm8 => xmm8,
    zmm9 => ymm9 | xmm9,
    ymm9 => xmm9,
    zmm10 => ymm10 | xmm10,
    ymm10 => xmm10,
    zmm11 => ymm11 | xmm11,
    ymm11 => xmm11,
    zmm12 => ymm12 | xmm12,
    ymm12 => xmm12,
    zmm13 => ymm13 | xmm13,
    ymm13 => xmm13,
    zmm14 => ymm14 | xmm14,
    ymm14 => xmm14,
    zmm15 => ymm15 | xmm15,
    ymm15 => xmm15,
    zmm16 => ymm16 | xmm16,
    ymm16 => xmm16,
    zmm17 => ymm17 | xmm17,
    ymm17 => xmm17,
    zmm18 => ymm18 | xmm18,
    ymm18 => xmm18,
    zmm19 => ymm19 | xmm19,
    ymm19 => xmm19,
    zmm20 => ymm20 | xmm20,
    ymm20 => xmm20,
    zmm21 => ymm21 | xmm21,
    ymm21 => xmm21,
    zmm22 => ymm22 | xmm22,
    ymm22 => xmm22,
    zmm23 => ymm23 | xmm23,
    ymm23 => xmm23,
    zmm24 => ymm24 | xmm24,
    ymm24 => xmm24,
    zmm25 => ymm25 | xmm25,
    ymm25 => xmm25,
    zmm26 => ymm26 | xmm26,
    ymm26 => xmm26,
    zmm27 => ymm27 | xmm27,
    ymm27 => xmm27,
    zmm28 => ymm28 | xmm28,
    ymm28 => xmm28,
    zmm29 => ymm29 | xmm29,
    ymm29 => xmm29,
    zmm30 => ymm30 | xmm30,
    ymm30 => xmm30,
    zmm31 => ymm31 | xmm31,
    ymm31 => xmm31,

    mm0 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm1 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm2 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm3 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm4 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm5 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm6 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
    mm7 => st0 | st1 | st2 | st3 | st4 | st5 | st6 | st7,
];

pub static X86_16_ASM_PROPERTIES: AsmProperties = AsmProperties {
    syntax_names: span![const_sv!("intel"), const_sv!("at&t")],
    constraints: constraints::x86_16::ASM_CONSTRAINTS,
    register_groups: reg_groups::x86_16::REGISTER_GROUPS,
    overlaps: X86_ASM_REGISTER_OVERLAPS,
};

pub static X86_32_ASM_PROPERTIES: AsmProperties = AsmProperties {
    syntax_names: span![const_sv!("intel"), const_sv!("at&t")],
    constraints: constraints::x86_32::ASM_CONSTRAINTS,
    register_groups: reg_groups::x86_32::REGISTER_GROUPS,
    overlaps: X86_ASM_REGISTER_OVERLAPS,
};

pub static X86_64_ASM_PROPERTIES: AsmProperties = AsmProperties {
    syntax_names: span![const_sv!("intel"), const_sv!("at&t")],
    constraints: constraints::x86_64::ASM_CONSTRAINTS,
    register_groups: reg_groups::x86_64::REGISTER_GROUPS,
    overlaps: X86_ASM_REGISTER_OVERLAPS,
};

pub static X86_64: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64,
    arch_names: span![const_sv!("x86_64"), const_sv!("x86-64")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_64_ASM_PROPERTIES,
};

pub static X86_64_V2: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V2,
    arch_names: span![const_sv!("x86_64"), const_sv!("x86-64")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_64_ASM_PROPERTIES,
};

pub static X86_64_V3: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V3,
    arch_names: span![const_sv!("x86_64"), const_sv!("x86-64")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_64_ASM_PROPERTIES,
};

pub static X86_64_V4: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xFFFF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MX86_64_V4,
    arch_names: span![const_sv!("x86_64"), const_sv!("x86-64")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_64_ASM_PROPERTIES,
};

pub static I386: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI386,
    arch_names: span![const_sv!("i386"), const_sv!("x86")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_32_ASM_PROPERTIES,
};

pub static I486: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI486,
    arch_names: span![const_sv!("i486"), const_sv!("x86")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_32_ASM_PROPERTIES,
};
pub static I586: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI586,
    arch_names: span![const_sv!("i586"), const_sv!("x86")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_32_ASM_PROPERTIES,
};

pub static I686: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xF,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI686,
    arch_names: span![const_sv!("i686"), const_sv!("x86")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_32_ASM_PROPERTIES,
};

pub static I86: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0x3,
    builtin_names: X86_BUILTINS,
    target_features: X86_FEATURES,
    machines: X86_MACHINES,
    default_machine: &machines::MI86,
    arch_names: span![const_sv!("i86"), const_sv!("i8086")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &X86_16_ASM_PROPERTIES,
};
