#![allow(
    clippy::upper_case_acronyms,
    clippy::manual_non_exhaustive,
    clippy::match_like_matches_macro
)] // Kill clippy for MSRV
use alloc::{borrow::ToOwned, string::String};
use core::fmt::Formatter;
use core::{fmt::Display, str::FromStr};

///
/// The result of FromStr::from_str, when parsing a field (other than vendor),
///  with a value that is not known to the library
#[derive(Debug, Clone, Copy)]
pub struct UnknownError;

impl Display for UnknownError {
    fn fmt(&self, fmt: &mut Formatter) -> core::fmt::Result {
        fmt.write_str("Unknown or invalid target or component")
    }
}

///
/// The Architecture field of a target tuple
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u32)]
pub enum Architecture {
    Unknown = 0,
    I86 = 1,
    I8086 = 2,
    I086 = 3,
    I186 = 4,
    I286 = 5,
    I386 = 6,
    I486 = 7,
    I586 = 8,
    I686 = 9,
    X86_64 = 10,
    Arm = 11,
    ArmBe = 12,
    Aarch64 = 13,
    Aarch64Be = 14,
    Aarch64_32 = 15,
    Mips = 16,
    MipsLE = 17,
    Mips64 = 18,
    Mips64LE = 19,
    PowerPC32 = 20,
    PowerPC64 = 21,
    PowerPC64le = 22,
    RiscV32 = 23,
    RiscV64 = 24,
    Sparc = 25,
    SparcV9 = 26,
    SparcEL = 27,
    Wasm32 = 28,
    Wasm64 = 29,
    Wc65c816 = 30,
    M6502 = 31,
    M65C02 = 32,
    SPC700 = 33,

    /// Used for ABI Purposes with lccc
    Null = (-1i32) as u32,
    #[doc(hidden)]
    __Nonexhaustive = (-2i32) as u32,
}

impl FromStr for Architecture {
    type Err = UnknownError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "i86" => Self::I86,
            "i8086" => Self::I8086,
            "i086" => Self::I086,
            "i186" => Self::I186,
            "i286" => Self::I286,
            "i386" => Self::I386,
            "i486" => Self::I486,
            "i586" => Self::I586,
            "i686" => Self::I686,
            "amd64" | "x86_64" | "x86_64h" => Self::X86_64,
            "armeb" => Self::ArmBe,
            "arm" => Self::Arm,
            "aarch64" | "arm64" | "arm64e" => Self::Aarch64,
            "aarch64_be" | "arm64_be" => Self::Aarch64Be,
            "aarch64_32" | "arm64_32" => Self::Aarch64_32,
            "powerpc" | "powerpcspe" | "ppc" | "ppc32" => Self::PowerPC32,
            "powerpc64" | "ppu" | "ppc64" => Self::PowerPC64,
            "powerpc64le" | "ppc64le" => Self::PowerPC64le,
            "mips" | "mipseb" | "mipsallegrex" | "mipsisa32r6" | "mipsr6" => Self::Mips,
            "mipsel" | "mipsallegrexel" | "mipsisa32r6el" | "mipsr6el" => Self::MipsLE,
            "mips64" | "mips64eb" | "mipsn32" | "mipsisa64r6" | "mips64r6" | "mipsn32r6" => {
                Self::Mips64
            }
            "mips64el" | "mipsn32el" | "mipsisa64r6el" | "mips64r6el" | "mipsn32r6el" => {
                Self::Mips64LE
            }
            "sparc" => Self::Sparc,
            "sparcel" => Self::SparcEL,
            "sparcv9" | "sparc64" => Self::SparcV9,
            "riscv32" => Self::RiscV32,
            "riscv64" => Self::RiscV64,
            "wc65c816" | "65816" | "w65c816" | "65c816" | "w65" => Self::Wc65c816,
            "6502" | "6502x" | "6502X" => Self::M6502,
            "65c02" | "65C02" => Self::M65C02,
            "wasm32" => Self::Wasm32,
            "wasm64" => Self::Wasm64,

            "spc700" | "spc" => Self::SPC700,

            _ => return Err(UnknownError),
        })
    }
}

impl Display for Architecture {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.canonical_name().fmt(f)
    }
}

impl Architecture {
    /// Parses the Architecture in a "lossy" manner
    /// This is equivalent to [`Self::from_str`], but returns [`Architecture::Unknown`], instead of an error,
    ///  on an unknown architecture.
    /// This is useful (in conjunction with an actual target name)
    pub fn parse(st: &str) -> Self {
        Self::from_str(st).unwrap_or(Architecture::Unknown)
    }

    ///
    /// Returns the canonical name of the target
    /// The canonical name, when passed into `[`Self::parse`] will yield an equivalent value,
    /// Formatting an Architecture yields this string
    pub fn canonical_name(&self) -> &'static str {
        match self {
            Architecture::Unknown => "unknown",
            Architecture::I86 => "i86",
            Architecture::I086 => "i086",
            Architecture::I8086 => "i8086",
            Architecture::I186 => "i186",
            Architecture::I286 => "i286",
            Architecture::I386 => "i386",
            Architecture::I486 => "i486",
            Architecture::I586 => "i586",
            Architecture::I686 => "i686",
            Architecture::X86_64 => "x86_64",
            Architecture::Arm => "arm",
            Architecture::ArmBe => "armeb",
            Architecture::Aarch64 => "aarch64",
            Architecture::Aarch64Be => "aarch64_be",
            Architecture::Aarch64_32 => "aarch64_32",
            Architecture::Mips => "mips",
            Architecture::Mips64 => "mips64",
            Architecture::PowerPC32 => "powerpc",
            Architecture::PowerPC64 => "powerpc64",
            Architecture::PowerPC64le => "powerpc64le",
            Architecture::RiscV32 => "riscv32",
            Architecture::RiscV64 => "riscv64",
            Architecture::Sparc => "sparc",
            Architecture::SparcV9 => "sparcv9",
            Architecture::SparcEL => "sparcel",
            Architecture::Wasm32 => "wasm32",
            Architecture::Wasm64 => "wasm64",
            Architecture::Wc65c816 => "w65",
            Architecture::MipsLE => "mipsel",
            Architecture::Mips64LE => "mips64el",
            Architecture::M6502 => "6502",
            Architecture::M65C02 => "6502",
            Architecture::SPC700 => "spc700",
            Architecture::Null => "null",
            Architecture::__Nonexhaustive => unreachable!(),
        }
    }

    pub fn is_x86(&self) -> bool {
        match self {
            Architecture::I86
            | Architecture::I8086
            | Architecture::I186
            | Architecture::I286
            | Architecture::I386
            | Architecture::I486
            | Architecture::I586
            | Architecture::I686 => true,
            _ => false,
        }
    }
}

///
/// The Vendor field of a target tuple
///
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u32)]
pub enum Vendor {
    Unknown = 0,
    Apple = 1,
    PC = 2,
    SCEI = 3,
    Freescale = 4,
    IBM = 5,
    ImaginationTechnologies = 6,
    MipsTechnologies = 7,
    NVIDIA = 8,
    CSR = 9,
    Myriad = 10,
    AMD = 11,
    Mesa = 12,
    SUSE = 13,
    OpenEmbedded = 14,
    WDC = 15,

    /// Used for ABI Purposes with lccc
    Null = (-1i32) as u32,
    #[doc(hidden)]
    __Nonexhaustive = (-2i32) as u32,
}

impl FromStr for Vendor {
    type Err = core::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "apple" => Self::Apple,
            "pc" => Self::PC,
            // "nes" => Self::NES,
            // "snes" | "snesdev" => Self::SNES,
            "scei" => Self::SCEI,
            "fsl" => Self::Freescale,
            "img" => Self::ImaginationTechnologies,
            "ibm" => Self::IBM,
            "mti" => Self::MipsTechnologies,
            "nvidia" => Self::NVIDIA,
            "csr" => Self::CSR,
            "myriad" => Self::Myriad,
            "amd" => Self::AMD,
            "mesa" => Self::Mesa,
            "suse" => Self::SUSE,
            "oe" => Self::OpenEmbedded,
            "wdc" => Self::WDC,
            _ => Self::Unknown,
        })
    }
}

impl Display for Vendor {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.canonical_name().fmt(f)
    }
}

impl Vendor {
    /// Parses the Vendor in a "lossy" manner
    /// This is equivalent to [`Self::from_str`].
    /// Note that an unknown vendor is not considered an error.
    pub fn parse(s: &str) -> Self {
        Self::from_str(s).unwrap()
    }

    ///
    /// Returns the canonical name of the vendor
    /// The canonical name, when passed into `[`Self::parse`] will yield an equivalent value,
    /// Formatting a Vendor yields this string
    pub fn canonical_name(&self) -> &'static str {
        match self {
            Vendor::Apple => "apple",
            Vendor::PC => "pc",
            Vendor::Unknown => "unknown",
            Vendor::SCEI => "scei",
            Vendor::Freescale => "fsl",
            Vendor::IBM => "ibm",
            Vendor::ImaginationTechnologies => "img",
            Vendor::MipsTechnologies => "mti",
            Vendor::NVIDIA => "nvidia",
            Vendor::CSR => "csr",
            Vendor::Myriad => "myriad",
            Vendor::AMD => "amd",
            Vendor::Mesa => "mesa",
            Vendor::SUSE => "suse",
            Vendor::OpenEmbedded => "oe",
            Vendor::WDC => "wdc",
            Vendor::Null => "null",
            Vendor::__Nonexhaustive => unreachable!(),
        }
    }
}

///
/// The Operating System Field of a target tuple
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u32)]
pub enum OS {
    Unknown = 0,

    Ananas = 1,
    CloudABI = 2,
    Darwin = 3,
    DragonFly = 4,
    FreeBSD = 5,
    Fuchsia = 6,
    IOS = 7,
    KFreeBSD = 8,
    Linux = 9,
    Lv2 = 10,
    MacOSX = 11,
    NetBSD = 12,
    OpenBSD = 13,
    Solaris = 14,
    Win32 = 15,
    ZOS = 16,
    Haiku = 17,
    Minix = 18,
    RTEMS = 19,
    NaCl = 20,
    AIX = 21,
    CUDA = 22,
    NVCL = 23,
    AMDHSA = 24,
    PS4 = 25,
    ELFIAMCU = 26,
    TvOS = 27,
    WatchOS = 28,
    Mesa3D = 29,
    Contiki = 30,
    AMDPAL = 31,
    HermitCore = 32,
    Hurd = 33,
    WASI = 34,
    Emscripten = 35,
    PhantomOS = 36,
    SNES = 37, // Not an OS, but the currently config.sub places it in the os field
    NES = 38,  // likewise

    Null = (-1i32) as u32,
    #[doc(hidden)]
    __Nonexhaustive = (-2i32) as u32,
}

impl FromStr for OS {
    type Err = UnknownError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            x if x.starts_with("ananas") => Self::Ananas,
            x if x.starts_with("cloudabi") => Self::CloudABI,
            x if x.starts_with("darwin") => Self::Darwin,
            x if x.starts_with("dragonfly") => Self::DragonFly,
            x if x.starts_with("freebsd") => Self::FreeBSD,
            x if x.starts_with("fuchsia") => Self::Fuchsia,
            x if x.starts_with("ios") => Self::IOS,
            x if x.starts_with("kfreebsd") => Self::KFreeBSD,
            x if x.starts_with("linux") => Self::Linux,
            x if x.starts_with("lv2") => Self::Lv2,
            x if x.starts_with("macos") => Self::MacOSX,
            x if x.starts_with("netbsd") => Self::NetBSD,
            x if x.starts_with("openbsd") => Self::OpenBSD,
            x if x.starts_with("solaris") => Self::Solaris,
            x if x.starts_with("win32") | x.starts_with("windows") => Self::Win32,
            x if x.starts_with("zos") => Self::ZOS,
            x if x.starts_with("haiku") => Self::Haiku,
            x if x.starts_with("minix") => Self::Minix,
            x if x.starts_with("rtems") => Self::RTEMS,
            x if x.starts_with("nacl") => Self::NaCl,
            x if x.starts_with("aix") => Self::AIX,
            x if x.starts_with("cuda") => Self::CUDA,
            x if x.starts_with("nvcl") => Self::NVCL,
            x if x.starts_with("amdhsa") => Self::AMDHSA,
            x if x.starts_with("ps4") => Self::PS4,
            x if x.starts_with("elfiamcu") => Self::ELFIAMCU,
            x if x.starts_with("tvos") => Self::TvOS,
            x if x.starts_with("watchos") => Self::WatchOS,
            x if x.starts_with("mesa3d") => Self::Mesa3D,
            x if x.starts_with("contiki") => Self::Contiki,
            x if x.starts_with("amdpal") => Self::AMDPAL,
            x if x.starts_with("hermit") => Self::HermitCore,
            x if x.starts_with("hurd") => Self::Hurd,
            x if x.starts_with("wasi") => Self::WASI,
            x if x.starts_with("emscripten") => Self::Emscripten,
            x if x.starts_with("phantom") => Self::PhantomOS,
            x if x.starts_with("snes") => Self::SNES,
            x if x.starts_with("nes") => Self::NES,

            _ => return Err(UnknownError),
        })
    }
}

impl Display for OS {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.canonical_name().fmt(f)
    }
}

impl OS {
    /// Parses the OS in a "lossy" manner
    /// This is equivalent to [`Self::from_str`], except that [`OS::Unknown`] is returned, instead of an error, on an unknown OS Field
    pub fn parse(s: &str) -> Self {
        Self::from_str(s).unwrap_or(Self::Unknown)
    }

    ///
    /// Returns the canonical name of the operating system
    /// The canonical name, when passed into `[`Self::parse`] will yield an equivalent value,
    /// Formatting an OS yields this string
    pub fn canonical_name(&self) -> &'static str {
        match self {
            OS::Unknown => "unknown",
            OS::Ananas => "ananas",
            OS::CloudABI => "cloudabi",
            OS::Darwin => "darwin",
            OS::DragonFly => "dragonfly",
            OS::FreeBSD => "freebsd",
            OS::Fuchsia => "fuchsia",
            OS::IOS => "ios",
            OS::KFreeBSD => "kfreebsd",
            OS::Linux => "linux",
            OS::Lv2 => "lv2",
            OS::MacOSX => "macos",
            OS::NetBSD => "netbsd",
            OS::OpenBSD => "openbsd",
            OS::Solaris => "solaris",
            OS::Win32 => "win32",
            OS::ZOS => "zos",
            OS::Haiku => "haiku",
            OS::Minix => "minix",
            OS::RTEMS => "rtems",
            OS::NaCl => "nacl",
            OS::AIX => "aix",
            OS::CUDA => "cuda",
            OS::NVCL => "nvcl",
            OS::AMDHSA => "amdhsa",
            OS::PS4 => "ps4",
            OS::ELFIAMCU => "elfiamcu",
            OS::TvOS => "tvos",
            OS::WatchOS => "watchos",
            OS::Mesa3D => "mesa3d",
            OS::Contiki => "contiki",
            OS::AMDPAL => "amdpal",
            OS::HermitCore => "hermit",
            OS::Hurd => "hurd",
            OS::WASI => "wasi",
            OS::Emscripten => "emscripten",
            OS::PhantomOS => "phantom",
            OS::SNES => "snes",
            OS::NES => "nes",
            OS::Null => "null",
            OS::__Nonexhaustive => unreachable!(),
        }
    }
}

///
/// The Environment field of target tuples
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u32)]
pub enum Environment {
    Unknown = 0,
    GNU = 1,
    GNUABIN32 = 2,
    GNUABI64 = 3,
    GNUEABI = 4,
    GNUEABIHF = 5,
    GNUX32 = 6,
    CODE16 = 7,
    EABI = 8,
    EABIHF = 9,
    Android = 10,
    Musl = 11,
    MuslEABI = 12,
    MuslEABIHF = 13,

    MSVC = 15,
    Itanium = 16,
    Cygnus = 17,
    CoreCLR = 18,
    Simulator = 19,
    MacABI = 20,

    PhantomStandard = 21,
    PhantomKernel = 22,

    Null = (-1i32) as u32,
    #[doc(hidden)]
    __Nonexhaustive = (-2i32) as u32,
}

impl FromStr for Environment {
    type Err = UnknownError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            x if x.starts_with("eabihf") => Self::EABIHF,
            x if x.starts_with("eabi") => Self::EABI,
            x if x.starts_with("gnuabin32") => Self::GNUABIN32,
            x if x.starts_with("gnuabi64") => Self::GNUABI64,
            x if x.starts_with("gnueabihf") => Self::GNUEABIHF,
            x if x.starts_with("gnueabi") => Self::GNUEABI,
            x if x.starts_with("gnux32") => Self::GNUX32,
            x if x.starts_with("gnu") => Self::GNU,
            x if x.starts_with("code16") => Self::CODE16,
            x if x.starts_with("android") => Self::Android,
            x if x.starts_with("musleabihf") => Self::MuslEABIHF,
            x if x.starts_with("musleabi") => Self::MuslEABI,
            x if x.starts_with("musl") => Self::Musl,
            x if x.starts_with("msvc") => Self::MSVC,
            x if x.starts_with("itanium") => Self::Itanium,
            x if x.starts_with("cygnus") => Self::Cygnus,
            x if x.starts_with("coreclr") => Self::CoreCLR,
            x if x.starts_with("simulator") => Self::Simulator,
            x if x.starts_with("macabi") => Self::MacABI,
            x if x.starts_with("pcore") || x.starts_with("user") => Self::PhantomStandard,
            x if x.starts_with("pkrnl") || x.starts_with("kernel") => Self::PhantomKernel,
            _ => return Err(UnknownError),
        })
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.canonical_name().fmt(f)
    }
}

impl Environment {
    /// Parses the Environment name in a "lossy" manner
    /// This is equivalent to [`Self::from_str`], except that [`Environment::Unknown`] is returned, instead of an error, on an unknown OS Field
    pub fn parse(s: &str) -> Self {
        Self::from_str(s).unwrap_or(Self::Unknown)
    }

    ///
    /// Returns the canonical name of the environment
    /// The canonical name, when passed into [`Self::parse`] will yield an equivalent value,
    /// Formatting an Environment yields this string
    pub fn canonical_name(&self) -> &'static str {
        match self {
            Environment::Unknown => "unknown",
            Environment::GNU => "gnu",
            Environment::GNUABIN32 => "gnuabin32",
            Environment::GNUABI64 => "gnuabi64",
            Environment::GNUEABI => "gnueabi",
            Environment::GNUEABIHF => "gnueabihf",
            Environment::GNUX32 => "gnux32",
            Environment::CODE16 => "code16",
            Environment::EABI => "eabi",
            Environment::EABIHF => "eabihf",
            Environment::Android => "android",
            Environment::Musl => "musl",
            Environment::MuslEABI => "musleabi",
            Environment::MuslEABIHF => "musleabihf",
            Environment::MSVC => "msvc",
            Environment::Itanium => "itanium",
            Environment::Cygnus => "cygnus",
            Environment::CoreCLR => "coreclr",
            Environment::Simulator => "simulator",
            Environment::MacABI => "macabi",
            Environment::PhantomStandard => "user",
            Environment::PhantomKernel => "kernel",
            Environment::Null => "",
            Environment::__Nonexhaustive => unreachable!(),
        }
    }
}

///
/// The object format used by a target
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u32)]
pub enum ObjectFormat {
    Unknown = 0,
    XCoff = 1,
    Coff = 2,
    Elf = 3,
    Goff = 4,
    MachO = 5,
    Wasm = 6,

    Xo65 = 7,
    O65 = 8,
    WlaObj = 9,

    Null = (-1i32) as u32,

    #[doc(hidden)]
    __Nonexhaustive = (-2i32) as u32,
}

impl FromStr for ObjectFormat {
    type Err = UnknownError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            x if x.ends_with("xcoff") => Self::XCoff,
            x if x.ends_with("coff") => Self::Coff,
            x if x.ends_with("elf") => Self::Elf,
            x if x.ends_with("goff") => Self::Goff,
            x if x.ends_with("macho") => Self::MachO,
            x if x.ends_with("wasm") => Self::Wasm,
            x if x.ends_with("xo65") => Self::Xo65,
            x if x.ends_with("o65") => Self::O65,
            x if x.ends_with("wlaobj") => Self::WlaObj,
            x if x.ends_with("wla") => Self::WlaObj,
            _ => return Err(UnknownError),
        })
    }
}

impl Display for ObjectFormat {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.canonical_name().fmt(f)
    }
}

impl ObjectFormat {
    /// Parses the ObjectFormat name in a "lossy" manner, from the end of the Environment field
    /// This is equivalent to [`Self::from_str`], except that [`ObjectFormat::Unknown`] is returned, instead of an error, on an unknown OS Field
    pub fn parse(s: &str) -> Self {
        Self::from_str(s).unwrap_or(Self::Unknown)
    }

    ///
    /// Returns the canonical name of the object format
    /// The canonical name, when passed into [`Self::parse`] will yield an equivalent value,
    /// Formatting an ObjectFormat yields this string
    pub fn canonical_name(&self) -> &'static str {
        match self {
            ObjectFormat::Unknown => "unknown",
            ObjectFormat::XCoff => "xcoff",
            ObjectFormat::Coff => "coff",
            ObjectFormat::Elf => "elf",
            ObjectFormat::Goff => "goff",
            ObjectFormat::MachO => "macho",
            ObjectFormat::Wasm => "wasm",
            ObjectFormat::Xo65 => "xo65",
            ObjectFormat::O65 => "o65",
            ObjectFormat::WlaObj => "wlaobj",
            ObjectFormat::Null => "",
            ObjectFormat::__Nonexhaustive => unreachable!(),
        }
    }
}

///
/// The representation of a target tuple.
///
/// A Target Tuple is of the form arch-vendor-system, where system can be either os-env
///  or simply either os or env (the latter is used in the case of a freestanding target).
///
/// There are two types of target tuple: canonical and exact.
/// This type can be used to represent both.
///
/// The [`core::fmt::Display`] implementation will display the canonical tuple;
///  the function [`Self::get_name`] extracts the exact form that was parsed.
/// In any case, if any field, other than vendor, is unknown, or the form is not the one above,
///  the [`core::str::FromStr`] implementation will yield an UnknownError.
///
#[derive(Clone, Debug)]
pub struct Target {
    full: String,
    arch: Architecture,
    vendor: Option<Vendor>,
    // Invariant:
    // At least one of these fields is Some
    os: Option<OS>,
    env: Option<Environment>,
    objfmt: Option<ObjectFormat>,
}

impl FromStr for Target {
    type Err = UnknownError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split('-');
        let arch = split.next().ok_or(UnknownError).and_then(|s| s.parse())?;
        let f2 = split.next().ok_or(UnknownError)?;
        let f3 = split.next();
        let f4 = split.next();
        let vendor;
        let os;
        let env;
        let objfmt;
        if let (Some(s), None) = (f3, f4) {
            if let "unknown" = f2 {
                vendor = Some(Vendor::Unknown);
                if let Ok(o) = s.parse() {
                    os = Some(o);
                    env = None;
                    objfmt = None;
                } else if let Ok(e) = s.parse() {
                    os = None;
                    env = Some(e);
                    objfmt = s.parse().ok();
                } else if let Ok(of) = s.parse() {
                    os = None;
                    env = None;
                    objfmt = Some(of);
                } else {
                    return Err(UnknownError);
                }
            } else if let Vendor::Unknown = f2.parse().unwrap() {
                vendor = None;
                os = Some(f2.parse()?);
                env = s.parse().ok();
                objfmt = s.parse().ok();
                env.map(|_| ())
                    .or_else(|| objfmt.map(|_| ()))
                    .ok_or(UnknownError)?;
            } else {
                vendor = Some(f2.parse().unwrap());
                if let Ok(o) = s.parse() {
                    os = Some(o);
                    env = None;
                    objfmt = None;
                } else if let Ok(e) = s.parse() {
                    os = None;
                    env = Some(e);
                    objfmt = s.parse().ok();
                } else if let Ok(of) = s.parse() {
                    os = None;
                    env = None;
                    objfmt = Some(of);
                } else {
                    return Err(UnknownError);
                }
            }
        } else if let Some(s) = f4 {
            vendor = Some(f2.parse().unwrap());
            os = Some(f3.unwrap().parse()?);
            env = s.parse().ok();
            objfmt = s.parse().ok();
            env.map(|_| ())
                .or_else(|| objfmt.map(|_| ()))
                .ok_or(UnknownError)?;
        } else if let Ok(o) = f2.parse() {
            vendor = None;
            os = Some(o);
            env = None;
            objfmt = None;
        } else if let Ok(e) = f2.parse() {
            vendor = None;
            os = None;
            env = Some(e);
            objfmt = f2.parse().ok();
        } else if let Ok(of) = f2.parse() {
            vendor = None;
            os = None;
            env = None;
            objfmt = Some(of);
        } else {
            return Err(UnknownError);
        }

        Ok(Self {
            full: s.to_owned(),
            arch,
            vendor,
            os,
            env,
            objfmt,
        })
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.arch.fmt(f)?;
        f.write_str("-")?;
        if let Some(vendor) = &self.vendor {
            vendor.fmt(f)?;
        } else {
            self.vendor().fmt(f)?;
        }
        if let Some(os) = &self.os {
            f.write_str("-")?;
            os.fmt(f)?;
        }
        let mut last_field_sep = true;
        if let Some(env) = &self.env {
            last_field_sep = false;
            f.write_str("-")?;
            env.fmt(f)?;
        }
        if let Some(objfmt) = &self.objfmt {
            if last_field_sep {
                f.write_str("-")?;
            }
            objfmt.fmt(f)?;
        }
        Ok(())
    }
}

impl core::hash::Hash for Target {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.arch.hash(state);
        self.vendor.hash(state);
        self.os.hash(state);
        self.env.hash(state);
        self.objfmt.hash(state);
    }
}

impl PartialEq for Target {
    fn eq(&self, other: &Self) -> bool {
        self.arch == other.arch
            && self.vendor == other.vendor
            && self.os == other.os
            && self.env == other.env
            && self.objfmt == other.objfmt
    }
}

impl Eq for Target {}

impl Target {
    ///
    /// Gets the exact name of the target tuple.
    pub fn get_name(&self) -> &str {
        &*self.full
    }

    ///
    /// Returns the architecture name
    pub fn arch_name(&self) -> &str {
        self.full.split('-').next().unwrap()
    }

    pub fn vendor_name(&self) -> &str {
        if self.vendor.is_some() {
            self.full.split('-').nth(2).unwrap()
        } else {
            self.vendor().canonical_name()
        }
    }

    /// Parses a target tuple of the form arch-vendor-system (where system is either os-env, os, or env).
    /// If a field is not known, it is left as unknown, and the original value will be available
    ///  through the exact name.
    ///
    /// Panics if s is not of the above form
    pub fn parse(s: &str) -> Self {
        let mut split = s.split('-');
        let arch = Architecture::parse(split.next().unwrap());
        let f2 = split.next().ok_or(UnknownError).unwrap();
        let f3 = split.next();
        let f4 = split.next();
        let os;
        let env;
        let objfmt;
        let vendor;
        if let (Some(s), None) = (f3, f4) {
            if let "unknown" = f2 {
                vendor = Some(Vendor::Unknown);
                if let Ok(o) = s.parse() {
                    os = Some(o);
                    env = None;
                    objfmt = None;
                } else if let Ok(e) = s.parse() {
                    os = None;
                    env = Some(e);
                    objfmt = s.parse().ok();
                } else if let Ok(of) = s.parse() {
                    os = None;
                    env = None;
                    objfmt = Some(of);
                } else {
                    os = Some(OS::Unknown);
                    env = Some(Environment::Unknown);
                    objfmt = None;
                }
            } else if let Vendor::Unknown = f2.parse().unwrap() {
                vendor = None;
                os = Some(f2.parse().unwrap());
                env = s.parse().ok();
                objfmt = s.parse().ok();
                env.map(|_| ()).or_else(|| objfmt.map(|_| ())).unwrap();
            } else {
                vendor = Some(f2.parse().unwrap());
                if let Ok(o) = s.parse() {
                    os = Some(o);
                    env = None;
                    objfmt = None;
                } else if let Ok(e) = s.parse() {
                    os = None;
                    env = Some(e);
                    objfmt = s.parse().ok();
                } else if let Ok(of) = s.parse() {
                    os = None;
                    env = None;
                    objfmt = Some(of);
                } else {
                    os = Some(OS::Unknown);
                    env = Some(Environment::Unknown);
                    objfmt = None;
                }
            }
        } else if let Some(s) = f4 {
            vendor = Some(f2.parse().unwrap());
            os = Some(OS::parse(f3.unwrap()));
            env = s.parse().ok();
            objfmt = s.parse().ok();
            env.map(|_| ()).or_else(|| objfmt.map(|_| ())).unwrap();
        } else if let Ok(o) = f2.parse() {
            vendor = None;
            os = Some(o);
            env = None;
            objfmt = None;
        } else if let Ok(e) = f2.parse() {
            vendor = None;
            os = None;
            env = Some(e);
            objfmt = f2.parse().ok();
        } else if let Ok(of) = f2.parse() {
            vendor = None;
            os = None;
            env = None;
            objfmt = Some(of);
        } else {
            vendor = Some(Vendor::Unknown);
            os = Some(OS::Unknown);
            env = Some(Environment::Unknown);
            objfmt = None;
        }

        Self {
            full: s.to_owned(),
            arch,
            vendor,
            os,
            env,
            objfmt,
        }
    }

    ///
    /// Gets the value of the `os` field
    pub fn operating_system(&self) -> Option<OS> {
        self.os
    }

    ///
    /// Gets the value of the `env` field, or unknown if the environment was omitted
    pub fn environment(&self) -> Option<Environment> {
        self.env
    }

    ///
    /// Constructs a target tuple in canonical form from the specified components.
    pub fn from_components(
        arch: Architecture,
        vendor: Vendor,
        os: Option<OS>,
        env: Option<Environment>,
        objfmt: Option<ObjectFormat>,
    ) -> Self {
        let mut ret = Self {
            full: String::new(),
            arch,
            vendor: Some(vendor),
            os,
            env,
            objfmt,
        };
        ret.full = alloc::format!("{}", &ret);
        ret
    }

    ///
    /// Gets the object format, either from the end of the `env` field, or the default for the target
    pub fn target_object_format(&self) -> ObjectFormat {
        if let Some(of) = self.objfmt {
            of
        } else {
            match (&self.arch, &self.os) {
                (Architecture::Unknown, Some(OS::MacOSX)) => ObjectFormat::MachO,
                (Architecture::Aarch64, Some(OS::MacOSX)) => ObjectFormat::MachO,
                (Architecture::Aarch64_32, Some(OS::MacOSX)) => ObjectFormat::MachO,
                (Architecture::Arm, Some(OS::MacOSX)) => ObjectFormat::MachO,
                (arch, Some(OS::MacOSX)) if arch.is_x86() => ObjectFormat::MachO,
                (Architecture::X86_64, Some(OS::MacOSX)) => ObjectFormat::MachO,
                (Architecture::Unknown, Some(OS::Win32)) => ObjectFormat::Coff,
                (Architecture::Aarch64, Some(OS::Win32)) => ObjectFormat::Coff,
                (Architecture::Aarch64_32, Some(OS::Win32)) => ObjectFormat::Coff,
                (Architecture::Arm, Some(OS::Win32)) => ObjectFormat::Coff,
                (arch, Some(OS::Win32)) if arch.is_x86() => ObjectFormat::Coff,
                (Architecture::X86_64, Some(OS::Win32)) => ObjectFormat::Coff,
                (Architecture::PowerPC32, Some(OS::AIX)) => ObjectFormat::XCoff,
                (Architecture::PowerPC64, Some(OS::AIX)) => ObjectFormat::XCoff,
                (Architecture::SPC700, _) => ObjectFormat::WlaObj,
                _ => ObjectFormat::Elf,
            }
        }
    }

    /// Gets the object format component from the end of the env component, or None if none is present
    pub fn object_format(&self) -> Option<ObjectFormat> {
        self.objfmt
    }

    ///
    /// Gets the value of the Architecture field
    pub fn arch(&self) -> Architecture {
        self.arch
    }

    ///
    /// Gets the value of the vendor field.
    pub fn vendor(&self) -> Vendor {
        if let Some(vendor) = &self.vendor {
            *vendor
        } else if self.arch.is_x86() || Architecture::X86_64 == self.arch {
            match self.os {
                Some(OS::MacOSX) | Some(OS::IOS) | Some(OS::TvOS) | Some(OS::WatchOS) => {
                    Vendor::Apple
                }
                Some(OS::CUDA) => Vendor::NVIDIA,
                _ => Vendor::PC,
            }
        } else if let Architecture::Wc65c816 = self.arch {
            Vendor::WDC
        } else {
            Vendor::Unknown
        }
    }
}

///
/// Parses a target tuple from an environment variable.
#[macro_export]
macro_rules! from_env {
    ($var:literal) => {{
        use core::str::FromStr as _;
        let _target: $crate::Target = ::core::env!($var).parse().unwrap();
        _target
    }};
    ($var:literal?) => {{
        use core::str::FromStr as _;
        let _target: ::core::option::Option<$crate::Target> =
            ::core::option_env!($var).map(|s| s.parse().unwrap());
        _target
    }};
}
