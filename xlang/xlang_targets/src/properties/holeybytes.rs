use {
    super::{
        asm::{AsmScalar, AsmScalarKind},
        ArchProperties, AsmProperties, MachineProperties, PrimitiveProperties,
    },
    xlang_abi::{const_sv, pair::Pair, span},
};

macro_rules! constraints {
    {
        $($kind:ident @ $($sizes:literal)|*),* $(,)?
    } => {
        span![
            $($(Pair(const_sv!("register"),AsmScalar(AsmScalarKind::$kind,$sizes)),)*)*
        ]
    }
}

macro_rules! sv_span {
    [$($r:ident),* $(,)?] => {
        span![$(const_sv!(::std::stringify!($r))),*]
    };
}

pub static PRIMITIVES: PrimitiveProperties = PrimitiveProperties {
    intbits: 32,
    longbits: 64,
    llongbits: 64,
    ptrbits: 64,
    max_align: 16,
    ptralign: 8,
    fnptrbits: 64,
    nearptrbits: 64,
    farptrbits: 64,
    intmaxbits: 128,
    sizebits: 64,
    lock_free_atomic_mask: 0xff,
    max_atomic_align: 16,
    ldbl_align: 16,
    ldbl_format: super::FloatFormat::Ieee754(128),
};

pub static MACHINE_GENERIC: MachineProperties = MachineProperties {
    default_features: span![],
};

pub static ASM: AsmProperties = AsmProperties {
    syntax_names: span![const_sv!("standard")],
    constraints: constraints! {
        Integer @ 8 | 16 | 32 | 64,
        Float   @ 8 | 16 | 32 | 64,
        Vector  @ 8 | 16 | 32 | 64,
    },
    register_groups: span![Pair(
        const_sv!("register"),
        sv_span![
            r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18,
            r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35,
            r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52,
            r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63, r64, r65, r66, r67, r68, r69,
            r70, r71, r72, r73, r74, r75, r76, r77, r78, r79, r80, r81, r82, r83, r84, r85, r86,
            r87, r88, r89, r90, r91, r92, r93, r94, r95, r96, r97, r98, r99, r100, r101, r102,
            r103, r104, r105, r106, r107, r108, r109, r110, r111, r112, r113, r114, r115, r116,
            r117, r118, r119, r120, r121, r122, r123, r124, r125, r126, r127, r128, r129, r130,
            r131, r132, r133, r134, r135, r136, r137, r138, r139, r140, r141, r142, r143, r144,
            r145, r146, r147, r148, r149, r150, r151, r152, r153, r154, r155, r156, r157, r158,
            r159, r160, r161, r162, r163, r164, r165, r166, r167, r168, r169, r170, r171, r172,
            r173, r174, r175, r176, r177, r178, r179, r180, r181, r182, r183, r184, r185, r186,
            r187, r188, r189, r190, r191, r192, r193, r194, r195, r196, r197, r198, r199, r200,
            r201, r202, r203, r204, r205, r206, r207, r208, r209, r210, r211, r212, r213, r214,
            r215, r216, r217, r218, r219, r220, r221, r222, r223, r224, r225, r226, r227, r228,
            r229, r230, r231, r232, r233, r234, r235, r236, r237, r238, r239, r240, r241, r242,
            r243, r244, r245, r246, r247,
        ]
    )],
    overlaps: span![],
    classes: span![],
};

pub static HOLEYBYTES: ArchProperties = ArchProperties {
    lock_free_atomic_masks: 0xff,
    builtins: span![],
    target_features: span![],
    machines: span![Pair(const_sv!("generic"), &MACHINE_GENERIC)],
    default_machine: &MACHINE_GENERIC,
    arch_names: span![const_sv!("holeybytes")],
    byte_order: super::ByteOrder::LittleEndian,
    asm_propreties: &ASM,
    tag_names: span![const_sv!("C")],
    width: 64,
    abi_properties: None,
};
