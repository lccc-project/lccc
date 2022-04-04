use xlang_abi::{const_sv, pair::Pair, span, span::Span, string::StringView};

use super::{ArchProperties, MachineProperties};

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

pub static CLEVER: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xff,
    builtin_names: CLEVER_BUILTINS,
    target_features: CLEVER_FEATURES,
    machines: CLEVER_MACHINES,
    default_machine: &machines::MCLEVER1_0,
    arch_names: span![const_sv!("clever")],
    byte_order: super::ByteOrder::LittleEndian,
};
