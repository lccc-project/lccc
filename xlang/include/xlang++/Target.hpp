#ifndef LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11
#define LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11

#include <xlang++/Layout.h>

namespace lccc{
    enum class Architecture : std::uint32_t
    {
        X86_64 = 0,
        IX86 = 1,
        W65816 = 2,
        ARM = 3,
        AARCH64 = 4,
        SUPERH = 5,
        MIPS = 6,
        POWERPC = 7,
        POWERPC64 = 8,
        AVR = 9,
        M68000 = 10,

        SPARC = 14,
        RISCV32 = 15,
        RISCV64 = 16,
        WASM32 = 17,
        WASM64 = 18,

        WDC65C816 = 19,
        M6502     = 20,
        M65C02    = 21,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class Vendor : std::uint32_t
    {
        PC = 0,
        APPLE = 1,
        SNES = 2,
        WDC = 3,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class OperatingSystem : std::uint32_t
    {
        NONE = 0,
        LINUX = 1,
        WINDOWS = 2,
        MINGW32 = 3,
        MACOS = 4,
        IOS = 5,
        PHANTOM = 6,

        NULLOS = static_cast<std::uint32_t>(-2),
        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class Environment : std::uint32_t
    {
        NONE = 0,
        GNU = 1,
        EABI = 2,
        MSVC = 4,
        MUSL = 5,
        LC = 6,
        PHANTOM_STD = 7,
        PHANTOM_KERNEL = 8,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class ObjectFormat : std::uint32_t {
        AOUT = 0,
        COFF = 1,
        ELF  = 2,
        PE   = 3,
        XCOFF= 4,
        XO65 = 5,

        UNKNOWN = static_cast<std::uint32_t>(-1) 
    };

    struct XLANG_API Target
    {
    private:
        lccc::string_view name;
        Architecture arch;
        Vendor vendor;
        OperatingSystem os;
        Environment env;

    public:
        explicit Target(lccc::string_view) noexcept;

        lccc::string_view getName() const noexcept;

        lccc::string_view getArchName() const noexcept;
        lccc::string_view getCanonicalArch() const noexcept;
        Architecture getArch() const noexcept{
            return arch;
        }

        lccc::string_view getVendorName() const noexcept;
        lccc::string_view getCanonicalVendor() const noexcept;
        Vendor getVendor()const noexcept{
            return vendor;
        }

        lccc::string_view getOperatingSystemName() const noexcept;
        lccc::string_view getCanonicalOperatingSystem() const noexcept;
        OperatingSystem getOperatingSystem()const noexcept{
            return os;
        }

        lccc::string_view getEnvironmentName() const noexcept;
        lccc::string_view getCanonicalEnvironment() const noexcept;
        Environment getEnvironment()const noexcept{
            return env;
        }

    };
}

namespace lccc::xlang{

    struct MachineProperties{
        lccc::span<lccc::string_view> default_features;

    };
    struct ArchProperties{
        // The nth bit corresponds to the 2^n (bytes) size
        std::uint16_t lock_free_atomic_masks;

        lccc::span<const lccc::string_view> builtin_names;
        lccc::span<const lccc::string_view> target_features;
        lccc::span<const lccc::pair<lccc::string_view,const MachineProperties*>> machines;
        const MachineProperties* default_machine;
    };

    struct TargetProperties{
        std::uint16_t intbits;
        std::uint16_t longbits;
        std::uint16_t llongbits;
        std::uint16_t ptrbits;
        std::uint16_t max_align;
        std::uint16_t ptralign;

        std::uint16_t lock_free_atomic_masks;
        bool is_rust_supported;
        const ArchProperties* arch;
        lccc::span<const lccc::string_view> libdirs;
        lccc::span<const lccc::string_view> default_libs;
        lccc::span<const lccc::string_view> startfiles;
        lccc::span<const lccc::pair<lccc::string_view,bool>> enabled_features;
        lccc::span<const lccc::ObjectFormat> available_formats;
    };

    const TargetProperties* get_properties(lccc::Target target);
}

#endif