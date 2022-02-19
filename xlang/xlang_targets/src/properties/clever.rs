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

clever_builtins![];

pub static CLEVER: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xff,
    builtin_names: CLEVER_BUILTINS,
    target_features: CLEVER_FEATURES,
    machines: CLEVER_MACHINES,
    default_machine: &machines::MCLEVER1_0,
};
