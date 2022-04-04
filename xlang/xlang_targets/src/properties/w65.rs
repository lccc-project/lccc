use xlang_abi::{
    const_sv,
    pair::Pair,
    string::StringView,
    {span, span::Span},
};

use super::{ArchProperties, MachineProperties};

macro_rules! w65_machines{
    {
        $(($mach:ident, $name:literal $(| $alias:literal)* , [$($feature:literal),* $(,)?])),* $(,)?
    } => {
        mod machines{
            $(pub static $mach: super::MachineProperties = super::MachineProperties{
                default_features: xlang_abi::span![$(xlang_abi::const_sv!($feature)),*]
            };)*
        }

        pub static W65_MACHINES: Span<'static,Pair<StringView<'static>,&'static MachineProperties>> = span![
            $(Pair(const_sv!($name),&machines:: $mach) $(, Pair(const_sv!($alias),&machines:: $mach))*),*
        ];
    }
}

w65_machines! {
    (MW65, "w65" | "65816" | "65c816" | "wdc65c816", [])
}

macro_rules! w65_arithmetic_intrinsic_name {
    ($op:tt,$ty:tt,$bits:tt) => {
        ::xlang_abi::const_sv!(::core::concat!(
            "__",
            ::core::stringify!($op),
            "_",
            ::core::stringify!($ty),
            ::core::stringify!($bits)
        ))
    };
}

macro_rules! w65_arithmetic_intrinsics {
    (
        [$($op:ident),*]: $tys:tt @ $bits:tt
    ) => (w65_arithmetic_intrinsics! {
        $(
            $op: $tys
        )* @ $bits
    });

    (
        $(
            $op:ident : [$($ty:ident),*]
        )* @ $bits:tt
    ) => (w65_arithmetic_intrinsics! {
        $(
            [$($op: $ty)*] @ $bits
        )*
    });


    (
        $(
            $op_ty:tt @ [$($bit:literal),*]
        )*
    ) => (w65_arithmetic_intrinsics! {
        $(
            $(
                $op_ty @ $bit
            )*
        )*
    });

    (
        $(
            [$($op:tt : $ty:tt)*] @ $bit:tt
        )*
    ) => (
        [
            $($(
                w65_arithmetic_intrinsic_name!($op,$ty,$bit),
            )*)*
        ]
    );
}

static W65_INTRINSICS: Span<StringView> = Span::new(
    &w65_arithmetic_intrinsics!([add,sub,mul,div,cmp]: [uint,int,fixed,float] @ [8,16,32,64,128]),
);

pub static W65: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0x3,
    builtin_names: W65_INTRINSICS,
    target_features: span![const_sv!("softfp"), const_sv!("copfp")],
    machines: W65_MACHINES,
    default_machine: &machines::MW65,
    arch_names: span![
        const_sv!("w65"),
        const_sv!("65816"),
        const_sv!("65c816"),
        const_sv!("65c816"),
        const_sv!("wdc65c816")
    ],
    byte_order: super::ByteOrder::LittleEndian,
};
