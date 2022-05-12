use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};

use super::{ArchProperties, AsmProperties, MachineProperties};

macro_rules! clever_machines{
    {
        $(($mach:ident, $name:literal $(| $alias:literal)* , [$($feature:literal),* $(,)?])),* $(,)?
    } => {
        mod machines{
            $(pub static $mach: super::MachineProperties = super::MachineProperties{
                default_features: xlang_abi::span![$(xlang_abi::const_sv!($feature)),*]
            };)*
        }

        pub static CLEVER_MACHINES: Span<'static,Pair<StringView<'static>,&'static MachineProperties>> = span![
            $(Pair(const_sv!($name),&machines:: $mach) $(, Pair(const_sv!($alias),&machines:: $mach))*),*
        ];
    }
}

pub const CLEVER_FEATURES: Span<StringView> = span![
    const_sv!("float"),
    const_sv!("vector"),
    const_sv!("float-ext")
];

clever_machines! {
    (MCLEVER1_0, "clever1.0" | "clever", ["float"]),
}

macro_rules! clever_builtins{
    [
        $($name:ident),* $(,)?
    ] => {
        pub const CLEVER_BUILTINS: Span<'static,StringView<'static>> = span![
            $(const_sv!(::std::stringify!($name))),*
        ];
    }
}

clever_builtins![
    __fpx_expf16,
    __fpx_lnf16,
    __fpx_lgf16,
    __fpx_sinf16,
    __fpx_cosf16,
    __fpx_tanf16,
    __fpx_asinf16,
    __fpx_acosf16,
    __fpx_atanf16,
    __fpx_exp2f16,
    __fpx_log10f16,
    __fpx_lnp1f16,
    __fpx_expm1f16,
    __fpx_sqrtf16,
    __fpx_expf32,
    __fpx_lnf32,
    __fpx_lgf32,
    __fpx_sinf32,
    __fpx_cosf32,
    __fpx_tanf32,
    __fpx_asinf32,
    __fpx_acosf32,
    __fpx_atanf32,
    __fpx_exp2f32,
    __fpx_log10f32,
    __fpx_lnp1f32,
    __fpx_expm1f32,
    __fpx_sqrtf32,
    __fpx_expf64,
    __fpx_lnf64,
    __fpx_lgf64,
    __fpx_sinf64,
    __fpx_cosf64,
    __fpx_tanf64,
    __fpx_asinf64,
    __fpx_acosf64,
    __fpx_atanf64,
    __fpx_exp2f64,
    __fpx_log10f64,
    __fpx_lnp1f64,
    __fpx_expm1f64,
    __fpx_sqrtf64,
    __fp_rdfpcw,
    __fp_wrfpcw,
    __fp_cvtf16_fx32,
    __fp_cvtf16_fx16,
    __fp_cvtf16_fx64,
    __fp_cvtf32_fx32,
    __fp_cvtf32_fx64,
    __fp_cvtf64_fx64,
    __fp_fma16,
    __fp_fma32,
    __fp_fma64,
    __fp_raiseexcept,
    __fp_triggerexcept,
    __rdflags,
    __wrflags8,
    __wrflags16,
    __wrflags32,
    __wrflags64,
    __rdcpuid,
    __rdcpuex2,
    __rdcpuex3,
    __rdcpuex4,
    __rdcpuex5,
    __rdcpuex6,
    __rdmscpuex,
    __xms_pcfl,
    __xms_flall,
    __xms_dflush,
    __xms_iflush,
    __xms_in8,
    __xms_in16,
    __xms_in32,
    __xms_in64,
    __xms_out8,
    __xms_out16,
    __xms_out32,
    __xms_out64,
    __store_gpr,
    __load_gpr,
    __store_ar,
    __load_ar,
    __store_regf,
    __rand_rpoll,
];

macro_rules! clever_constraints{
    [
        $($kind:ident @ $($sizes:literal)|* => $name:ident),* $(,)?
    ] => {
        pub const CLEVER_ASM_CONSTRAINTS: Span<'static, Pair<StringView<'static>, AsmScalar>> = span![
            $($(Pair(const_sv!(::std::stringify!($name)),AsmScalar($kind,$sizes)),)*)*
        ];
    }
}

macro_rules! clever_register_groups {
    [
        $($name:ident => $($regname:ident)|*),* $(,)?
    ] => {
        pub const CLEVER_ASM_REGISTER_GROUPS: Span<'static, Pair<StringView<'static>,Span<'static,StringView<'static>>>> = span![
            $(Pair(const_sv!(::std::stringify!($name)),span![$(const_sv!(::std::stringify!($regname))),*])),*
        ];
    }
}

macro_rules! clever_overlaps {
    [ $($name:ident => $($overlap_names:ident)|*),* $(,)?] => {
        pub const CLEVER_ASM_REGISTER_OVERLAPS: Span<'static, Pair<StringView<'static>,StringView<'static>>> = span![
            $($(Pair(const_sv!(::std::stringify!($name)),const_sv!(::std::stringify!($overlap_names)))),*),*
        ];
    }
}

macro_rules! clever_classes {
    [ $($constraint:ident => $($class:ident)|*),* $(,)?] => {
        pub const CLEVER_ASM_CLASSES: Span<'static, Pair<StringView<'static>,StringView<'static>>> = span![
            $($(Pair(const_sv!(::std::stringify!($constraint)),const_sv!(::std::stringify!($class)))),*),*
        ];
    }
}

use super::{
    AsmScalar,
    AsmScalarKind::{Float, Integer, Vector},
};

clever_constraints![
    Integer @ 8 | 16 | 32 | 64 => general,
    Float @ 8 | 16 | 32 | 64 => general,
    Float @ 16 | 32 | 64 => float,
    Integer @ 8 | 16 | 32 | 64 => vectorlo,
    Integer @ 8 | 16 | 32 | 64 => vectorhi,
    Integer @ 8 | 16 | 32 | 64 => vectorhalf,
    Integer @ 8 | 16 | 32 | 64 | 128 => vector,
    Float @ 8 | 16 | 32 | 64 => vectorlo,
    Float @ 8 | 16 | 32 | 64 => vectorhi,
    Float @ 8 | 16 | 32 | 64 => vectorhalf,
    Float @ 8 | 16 | 32 | 64 | 128 => vector,
    Vector @ 8 | 16 | 32 | 64 => vectorlo,
    Vector @ 8 | 16 | 32 | 64 => vectorhi,
    Vector @ 8 | 16 | 32 | 64 => vectorhalf,
    Vector @ 8 | 16 | 32 | 64 | 128 => vector,
    Integer @ 1 => flag,
];

clever_register_groups![
    general => r0 | r1 | r2 | r3 | r4 | r5 | r8 | r9 | r10 | r11 | r12 | r13 | r15,
    float => f0 | f1 | f2 | f3 | f4 | f5 | f6 | f7,
    vectorlo => v0l | v1l | v2l | v3l | v4l | v5l | v6l | v7l | v8l | v9l | v10l | v11l | v12l | v13l | v14l | v15l
        | v16l | v17l | v18l | v19l | v20l | v21l | v22l | v23l | v24l | v25l | v26l | v27l | v28l | v29l | v30l | v31l,
    vectorhi => v0h | v1h | v2h | v3h | v4h | v5h | v6h | v7h | v8h | v9h | v10h | v11h | v12h | v13h | v14h | v15h
        | v16h | v17h | v18h | v19h | v20h | v21h | v22h | v23h | v24h | v25h | v26h | v27h | v28h | v29h | v30h | v31h,
    vectorhalf => v0l | v1l | v2l | v3l | v4l | v5l | v6l | v7l | v8l | v9l | v10l | v11l | v12l | v13l | v14l | v15l
        | v16l | v17l | v18l | v19l | v20l | v21l | v22l | v23l | v24l | v25l | v26l | v27l | v28l | v29l | v30l | v31l
        | v0h | v1h | v2h | v3h | v4h | v5h | v6h | v7h | v8h | v9h | v10h | v11h | v12h | v13h | v14h | v15h
        | v16h | v17h | v18h | v19h | v20h | v21h | v22h | v23h | v24h | v25h | v26h | v27h | v28h | v29h | v30h | v31h,
    vector => v0 | v1 | v2 | v3 | v4 | v5 | v6 | v7 | v8 | v9 | v10 | v11 | v12 | v13 | v14 | v15
        | v16 | v17 | v18 | v19 | v20 | v21 | v22 | v23 | v24 | v25 | v26 | v27 | v28 | v29 | v30 | v31,
    flag => z | v | c | n | p,
];

clever_overlaps![
    v0 => v0l | v0h,
    v1 => v1l | v1h,
    v2 => v2l | v2h,
    v3 => v3l | v3h,
    v4 => v4l | v4h,
    v5 => v5l | v5h,
    v6 => v6l | v6h,
    v7 => v7l | v7h,
    v8 => v8l | v8h,
    v9 => v9l | v9h,
    v10 => v10l | v10h,
    v11 => v11l | v11h,
    v12 => v12l | v12h,
    v13 => v13l | v13h,
    v14 => v14l | v14h,
    v15 => v15l | v15h,
    v16 => v16l | v16h,
    v17 => v17l | v17h,
    v18 => v18l | v18h,
    v19 => v19l | v19h,
    v20 => v20l | v20h,
    v21 => v21l | v21h,
    v22 => v22l | v22h,
    v23 => v23l | v23h,
    v24 => v24l | v24h,
    v25 => v25l | v25h,
    v26 => v26l | v26h,
    v27 => v27l | v27h,
    v28 => v28l | v28h,
    v29 => v29l | v29h,
    v30 => v30l | v30h,
    v31 => v31l | v31h,
];

clever_classes![
    vector => v | h | l,
    vectorhi => v,
    vectorlo => v,
];

pub static CLEVER_ASM: AsmProperties = AsmProperties {
    syntax_names: span![const_sv!("standard"), const_sv!("official")],
    constraints: CLEVER_ASM_CONSTRAINTS,
    register_groups: CLEVER_ASM_REGISTER_GROUPS,
    overlaps: CLEVER_ASM_REGISTER_OVERLAPS,
    classes: CLEVER_ASM_CLASSES,
};

pub static CLEVER: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xff,
    builtin_names: CLEVER_BUILTINS,
    target_features: CLEVER_FEATURES,
    machines: CLEVER_MACHINES,
    default_machine: &machines::MCLEVER1_0,
    arch_names: span![const_sv!("clever")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &CLEVER_ASM,
};
