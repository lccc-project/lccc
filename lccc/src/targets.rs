use xlang::abi::string::StringView;
pub use xlang::targets::properties::*;

mod builtin {
    pub mod clever;
    pub mod elf;
    pub mod holeybytes;
    pub mod lilium;
    pub mod linux;
    pub mod w65;
    pub mod x86;
}

#[must_use]
pub fn get_properties(targ: StringView) -> Option<&'static TargetProperties<'static>> {
    target_tuples::match_targets! {
        match (target_tuples::Target::parse(&targ)){
            x86_64-*-linux-gnu => Some(&builtin::linux::X86_64_LINUX_GNU),
            x86_64v2-*-linux-gnu => Some(&builtin::linux::X86_64_V2_LINUX_GNU),
            x86_64v3-*-linux-gnu => Some(&builtin::linux::X86_64_V3_LINUX_GNU),
            x86_64v4-*-linux-gnu => Some(&builtin::linux::X86_64_V4_LINUX_GNU),
            x86_64-*-linux-musl => Some(&builtin::linux::X86_64_LINUX_MUSL),
            x86_64-*-elf => Some(&builtin::elf::X86_64_ELF),
            x86_64-*-linux-gnux32 => Some(&builtin::linux::X32_LINUX_GNU),
            clever-*-elf => Some(&builtin::elf::CLEVER_ELF),
            i386-*-linux-gnu => Some(&builtin::linux::I386_LINUX_GNU),
            i486-*-linux-gnu => Some(&builtin::linux::I486_LINUX_GNU),
            i586-*-linux-gnu => Some(&builtin::linux::I586_LINUX_GNU),
            i686-*-linux-gnu => Some(&builtin::linux::I686_LINUX_GNU),
            i386-*-elf => Some(&builtin::elf::I386_ELF),
            i486-*-elf => Some(&builtin::elf::I486_ELF),
            i586-*-elf => Some(&builtin::elf::I586_ELF),
            i686-*-elf => Some(&builtin::elf::I686_ELF),
            w65-*-elf => Some(&builtin::elf::W65_ELF),
            w65-*-snes-elf => Some(&builtin::elf::W65_ELF),
            i86-*-near => Some(&builtin::elf::I86_NEAR_ELF),
            holeybytes-*-elf => Some(&builtin::elf::HOLEYBYTES_ELF),
            x86_64-*-lilium-std => Some(&builtin::lilium::X86_64_LILIUM),
            clever-*-lilium-std => Some(&builtin::lilium::CLEVER_LILIUM),
            i686-*-lilium-std => Some(&builtin::lilium::I686_LILIUM),
            * => None
        }
    }
}
