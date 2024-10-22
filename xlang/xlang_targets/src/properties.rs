use std::str::FromStr;

use xlang_abi::{pair::Pair, span::Span, string::StringView};

use self::builtins::BuiltinSignature;

///
/// Structure containing properties about individual machines (selected by `-march` or `-C target-cpu`) used by the architectures
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct MachineProperties<'a> {
    /// Features enabled by default on the Machine
    pub default_features: Span<'a, StringView<'a>>,
}

///
/// The Flavor of the linker used by default on the target
/// This chooses the default name of the linker as well as the CLI to use (even if the linker path is overridden)
#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LinkerFlavor {
    /// Microsoft link.exe
    MsLink,
    /// Unix ld
    Ld,
    /// Macos ld64
    MacosLd64,
}

/// The flavor of the archiver used by default on the target
/// By default, when linking to static or static-with-manifest (including static-with-rmanifest), lccc uses an internal archiver.
/// When this is overriden, these options choose the default program name, and the CLI used for the archiver
#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ArchiverFlavor {
    /// UNIX ar
    Ar,
    /// Microsoft lib.exe
    MsLib,
}

/// The Format for floating-point types
#[repr(i16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FloatFormat {
    /// An IEEE754 floating-point value (width)
    Ieee754(u16),
    /// A 10-byte (x87) double-extended precision value
    X87DoubleExtended,
    /// Ibm128/double-double precisiion used by PowerPC
    Ieee64x2,
}

impl FloatFormat {
    /// The size (in bytes) of the value, before applying alignment
    pub fn size(&self) -> u16 {
        match self {
            Self::Ieee754(val) => (*val).saturating_add(7) >> 3,
            Self::X87DoubleExtended => 10,
            Self::Ieee64x2 => 16,
        }
    }
}

///
/// The types of shared libraries available
#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SharedLibraryStyle {
    /// Indicates no shared libraries are available by default on the platform
    None,
    /// Indicates that shared libraries cannot be directly linked and need an import library (like windows dlls)
    Import,
    /// Indicates that shared libraries can be directly linked (like ELF shared objects)
    Linkable,
}

/// The Order of bytes in a scalar value
#[repr(i32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ByteOrder {
    /// Little Endian (LSB first) byte order
    LittleEndian,
    /// Big Endian (MSB first) byte order
    BigEndian,
    /// Mixed Endian byte order
    MiddleEndian,
}

/// Properties about inline assembly on a target
pub mod asm;

/// Properties about the assembly offered by the architecture
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct AsmProperties<'a> {
    /// Names of syntax options available
    pub syntax_names: Span<'a, StringView<'a>>,
    /// Names of architecture-specific input, output, and clobbers constraints that are registers (or pseudo-registers)
    pub constraints: Span<'a, Pair<StringView<'a>, asm::AsmScalar>>,

    /// Groups of registers by name
    pub register_groups: Span<'a, Pair<StringView<'a>, Span<'a, StringView<'a>>>>,

    /// List of register names that overlap
    pub overlaps: Span<'a, Pair<StringView<'a>, StringView<'a>>>,

    /// List of class-types by constraint type
    pub classes: Span<'a, Pair<StringView<'a>, StringView<'a>>>,
}

/// Properties about builtin functions on a target
pub mod builtins;

/// Properties about types that may require specific target features available to compile to
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct AbiProperties<'a> {
    /// Which feature vector types to each size
    pub vector_width_features: Span<'a, Pair<u16, StringView<'a>>>,
    /// Which feature is required to enable all unmentioned vector types
    pub vector_default_feature: StringView<'a>,
    /// Which feature each floating-point format requires,
    pub float_format_features: Span<'a, Pair<FloatFormat, StringView<'a>>>,
    /// Which feature is required to enable all unmentioned floating-point types
    pub float_default_features: StringView<'a>,
}

///
/// Properties about the architecture, shared between targets that use this architecture
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArchProperties<'a> {
    /// A bitmask of sizes (in bytes) of the atomic types that the architecture always supports lock free access on
    /// The first bit is size 1, the second is size 2, third is size 4, etc. Bits between powers of two should typically be set if the next power of two is set
    /// (This is used by `std::atomic` in C++)
    pub lock_free_atomic_masks: u16,
    /// Lists the name of all architecture builtins available on the architecture.
    /// These are made available from the namespace `__lccc::builtins::<arch_canonical_name>` builtin namespace. C and C++ compilers also inject them into the global namespace
    pub builtins: Span<'a, Pair<StringView<'a>, BuiltinSignature<'a>>>,
    /// A list of all target features available on the architecture, that can be selected at both a file and function-level
    pub target_features: Span<'a, StringView<'a>>,
    /// A list of pairs of all machine names and machine properties that can be used on this architecture
    pub machines: Span<'a, Pair<StringView<'a>, &'a MachineProperties<'a>>>,
    /// The default set of machine properties to use when compiling for this architecture
    pub default_machine: &'a MachineProperties<'a>,
    /// The list of names aliasing the architecture
    pub arch_names: Span<'a, StringView<'a>>,

    /// Byte Order
    pub byte_order: ByteOrder,

    /// Inline Assembly Properties
    pub asm_propreties: &'a AsmProperties<'a>,

    /// Architecture call ABI names
    pub tag_names: Span<'a, StringView<'a>>,

    /// Width of the architecture: Used by backends to differentiate between supported targets
    pub width: u16,

    /// Abi Limits of the target
    pub abi_properties: Option<&'a AbiProperties<'a>>,
}

///
/// Properties about the operating system (the sys component), shared between targets that use the same system
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct OperatingSystemProperties<'a> {
    /// Set if the operating system is unix like (IE. linux or bsd)
    pub is_unix_like: bool,
    /// Set if the operating system is windows or similar to windows (IE. win32)
    pub is_windows_like: bool,
    /// List of all os family names on the target
    pub os_family: Span<'a, StringView<'a>>,
    /// The prefix to prepend to static library file names, both when searching and when creating them (IE. "lib")
    pub static_prefix: StringView<'a>,
    /// The suffix (including the ".") to append to static library file names, both when searching and when creating them (IE. ".a")
    pub static_suffix: StringView<'a>,
    /// The prefix to prepend to shared library file names, both when searching and when creating them (IE. "lib")
    pub shared_prefix: StringView<'a>,
    /// The suffix (including the ".") to append to shared library file names, both when searching and when creating them (IE. ".so")
    pub shared_suffix: StringView<'a>,
    /// The suffix (including the ".") to append to executable file names, if any, when creating them (IE. ".exe")
    pub exec_suffix: StringView<'a>, // Assume that executable outputs don't have a prefix
    /// The suffix (including the ".") to append to object file names when creating them (IE. ".o")
    /// This suffix is also used to recognize xir bitcode files when linking with LTO enabled
    pub obj_suffix: StringView<'a>,
    /// The flavour of the linker to use when linking for this target
    pub ld_flavour: LinkerFlavor,
    /// The flavour of the archiver to use when linking static libraries for this target
    pub ar_flavour: ArchiverFlavor,
    /// The base directories (in the sysroot) to search in.
    /// This does not include the Microsoft Visual Studio Prefixes on windows.
    /// These directories should start with the unix directory separator (`/`), reguardless of the host target
    pub base_dirs: Span<'a, StringView<'a>>,

    /// The type of shared libraries on the system, if any
    pub so_kind: SharedLibraryStyle,
}

/// How to control stack options
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum StackAttributeControlStyle {
    /// Use `.note.GNU-stack`
    GnuStack,
    /// No option required for non-executable stack
    NoExec,
    /// Unknown or cannot specify non-executable stack
    CveFactory,
}

/// How unwinding tables are generated
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum UnwindStyle {
    /// Itanium (DWARF) Unwinding Tables
    Itanium,
    /// Windows SEH
    Seh,
    /// Setjmp/Longjmp landing Pads
    SjLj,
    /// Deterministic
    Deterministic,
    /// No unwinding on the target
    None,
}

/// Properties for the link step
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct LinkProperties<'a> {
    /// The library directories to be searched (within the base directories) for the `-l` flag, start files, end files, and the interpreter
    pub libdirs: Span<'a, StringView<'a>>,
    /// The default library files to be added to the link line on this target, unless the `-nostdlib` or `-nodefaultlibs` switch is enabled
    pub default_libs: Span<'a, StringView<'a>>,
    /// The object files to be included before other link inputs when linking executables on this target
    pub startfiles: Span<'a, StringView<'a>>,
    /// The object files to be included after other link inputs when linking executables on this target
    pub endfiles: Span<'a, StringView<'a>>,
    /// Additional object formats that are supported on this target (the default one is extracted from the target tuple)
    pub available_formats: Span<'a, StringView<'a>>,
    /// The name of the ELF interpreter when producing ELF files.
    pub interp: StringView<'a>,
    /// The name of the binfmt or bfd vector to use for objects
    pub obj_binfmt: StringView<'a>,
    /// The name of the binfmt or bfd vector to use for shared objects
    pub lib_binfmt: StringView<'a>,
    /// The name of the binfmt or bfd vector to use for executables
    pub exec_binfmt: StringView<'a>,
    /// The method for disabling executable stack, if needed.
    pub stack_attribute_control: StackAttributeControlStyle,
    /// Unwind Table Generation Method
    pub uwtable_method: UnwindStyle,
}

/// Properties about primitive types on the target
/// lccc makes a few assumptions about targets, for example, that it uses 8-bit bytes, and that short is 16-bit.
/// It also assumes the size and formats of `float`, `double`, `_Float16`, and `__float128`/`_Float128` (but not `long double`)
///
/// It further assumes that all pointer types, except fn pointers are compatible
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct PrimitiveProperties {
    /// The size (in bits) used by the C `int` type
    pub intbits: u16,
    /// The size (in bits) used by the C `long` type
    pub longbits: u16,
    /// The size (in bits) used by the C `long long` type
    pub llongbits: u16,
    /// The size (in bits) used by the C `void*` type. This is also the size (in bits) used by the `intptr_t` and `uintptr_t` typedefs
    pub ptrbits: u16,
    /// The maximum fundamental alignment requirement (in bytes). All primitive types up to this size use their size as their alignment (other than pointers)
    pub max_align: u16,
    /// The maximum alignment requirement of pointer types
    pub ptralign: u16,

    /// The size (in bits) used by the C `void(*)(void)` type
    pub fnptrbits: u16,
    /// The size (in bits) used by near pointers
    pub nearptrbits: u16,
    /// The size (in bits) used by near pointers
    pub farptrbits: u16,

    /// The size (in bits) used by the `intmax_t` and `uintmax_t` typedefs
    pub intmaxbits: u16,
    /// The size (in bits) used by the `size_t` and `ptrdiff_t` typedefs
    pub sizebits: u16,
    /// The mask (ORd with the Architecture mask) of bits that the target supports lock-free access on
    /// Some operating systems emulate lock-free atomic support in the supervisor, without using a global mutex in userspace (for example, arm-linux targets),
    /// this mask allows targets that support this to enable atomic sizes that are not architecturally supported
    /// It is the responsibility of the codegen to ensure that atomic accesses are properly compiled on targets when the mask is set.
    ///
    /// lccc does not guarantee that even lock free atomics will not make calls to software atomic operations (for example, w65 will call `__atomic_compare_exchange` even for size 1 and 2 accesses),
    /// but it guarantees that those calls made (if any) will be async-signal-safe and interrupt safe/atomic.
    /// It is likewise the responsibility of the codegen to ensure that any calls made for lock-free atomic sizes uphold these guarantees
    pub lock_free_atomic_mask: u16,

    /// The maximum alignment requirement of atomic operations. Atomic operations require the lesser of the size of the type, rounded to a power of two, and this number
    pub max_atomic_align: u16,

    /// The alignment requirement of the C `long double` type on this target.
    /// While `long double` does not use `max_align`, `max_align` should be at least this value to match the definition of the C/C++ typedef `align_max_t`.
    pub ldbl_align: u16,
    /// The format (and size) of the C `long double` type on this target
    pub ldbl_format: FloatFormat,
}

/// Properties about a particular target, which includes an architecture and an operating system
#[repr(C)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TargetProperties<'a> {
    /// The properties about the architecture's primtives
    pub primitives: &'a PrimitiveProperties,
    /// The shared properties of the architecture used by this target
    pub arch: &'a ArchProperties<'a>,
    /// The shared properties of the system used by this target
    pub os: &'a OperatingSystemProperties<'a>,
    /// Link step properties
    pub link: &'a LinkProperties<'a>,
    /// Target features (defined by the architecture) which are further controlled (explicitly enabled or explicitly disabled) on this target
    pub enabled_features: Span<'a, Pair<StringView<'a>, bool>>,
    /// Additional abis available
    pub abis: Span<'a, Pair<StringView<'a>, &'a TargetProperties<'a>>>,
    /// Default C call ABI
    pub default_tag_name: StringView<'a>,

    /// Default call abi for system apis. Usually not different from `default_tag_name`
    pub system_tag_name: StringView<'a>,

    /// Additional (namespaced) properties about the target.
    /// The first string is a key, which shall be a string in the form `<namespace>:<path>` where `<namespace>` and each `/` separated segment of `<path>`, shall match `[-A-Za-z._][-A-Za-z._]*`.
    /// The second string is the value, which may be any arbitrary UTF-8 text.
    /// Keys may be duplicated - the meaning of duplicate entries is defined by the property
    pub custom_properties: Span<'a, Pair<StringView<'a>, StringView<'a>>>,
}

impl<'a> TargetProperties<'a> {
    /// Convience Function for querying individual extended properties by name.
    /// Note that this performs no caching as the `custom_properties` list is intended to be short, and thus this performs a linear search on all accesses.
    pub fn extended_property<T: FromStr>(&self, property: &str) -> Result<Option<T>, T::Err> {
        self.custom_properties
            .iter()
            .find_map(
                |Pair(key, val)| {
                    if key == property {
                        Some(val)
                    } else {
                        None
                    }
                },
            )
            .map(|v| v.parse::<T>())
            .transpose()
    }
}
