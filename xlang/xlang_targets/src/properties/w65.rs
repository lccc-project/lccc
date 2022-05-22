use xlang_abi::{
    const_sv,
    pair::Pair,
    string::StringView,
    {span, span::Span},
};

use super::{
    ArchProperties, AsmProperties, AsmScalar,
    AsmScalarKind::{ClobberOnly, Float, Integer},
    MachineProperties,
};

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

macro_rules! w65_constraints{
    [
        $($kind:ident @ $($sizes:literal)|* => $name:ident),* $(,)?
    ] => {
        pub const W65_ASM_CONSTRAINTS: Span<'static, Pair<StringView<'static>, AsmScalar>> = span![
            $($(Pair(const_sv!(::std::stringify!($name)),AsmScalar($kind,$sizes)),)*)*
        ];
    }
}

macro_rules! w65_register_groups {
    [
        $($name:ident => $($regname:ident)|*),* $(,)?
    ] => {
        pub const W65_ASM_REGISTER_GROUPS: Span<'static, Pair<StringView<'static>,Span<'static,StringView<'static>>>> = span![
            $(Pair(const_sv!(::std::stringify!($name)),span![$(const_sv!(::std::stringify!($regname))),*])),*
        ];
    }
}

macro_rules! w65_classes {
    [ $($constraint:ident => $($class:ident)|*),* $(,)?] => {
        pub const W65_ASM_CLASSES: Span<'static, Pair<StringView<'static>,StringView<'static>>> = span![
            $($(Pair(const_sv!(::std::stringify!($constraint)),const_sv!(::std::stringify!($class)))),*),*
        ];
    }
}

macro_rules! w65_overlaps {
    [ $($name:ident => $($overlap_names:ident)|*),* $(,)?] => {
        pub const W65_ASM_REGISTER_OVERLAPS: Span<'static, Pair<StringView<'static>,StringView<'static>>> = span![
            $($(Pair(const_sv!(::std::stringify!($name)),const_sv!(::std::stringify!($overlap_names)))),*),*
        ];
    }
}

w65_constraints![
    Integer @ 8 | 16 => acc,
    Integer @ 8 | 16 => idx,
    Integer @ 8 | 16 | 32 => vreg,
    Float  @ 8 | 16 | 32 => vreg,
    Integer @ 64 => vreg64,
    Float @ 64 => vreg64,
    ClobberOnly @ 0 => status,
];

w65_register_groups![
    acc => A,
    idx => X | Y,
    vreg => __r0 | __r1 |  __r2 | __r3 | __r4 | __r5 |  __r6 | __r7,
    vreg64 => __r0q | __r2q | __r4q | __r6q,
    status => S | S16 | m | x,
];

w65_overlaps![
    __r0q => __r0 | __r1,
    __r2q => __r2 | __r3,
    __r4q => __r4 | __r5,
    __r6q => __r6 | __r7,
    S16 => S | m | x | cc,
    S => cc,
];

w65_classes![
    vreg64 => q | d | w | b,
    vreg => d | w | b,
];

pub static W65_ASSEMBLY: AsmProperties = AsmProperties {
    syntax_names: span![
        const_sv!("snesdev"),
        const_sv!("wladx"),
        const_sv!("ca65"),
        const_sv!("asar")
    ],
    constraints: W65_ASM_CONSTRAINTS,
    register_groups: W65_ASM_REGISTER_GROUPS,
    overlaps: W65_ASM_REGISTER_OVERLAPS,
    classes: W65_ASM_CLASSES,
};

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
    asm_propreties: &W65_ASSEMBLY,
};
