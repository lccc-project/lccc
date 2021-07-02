#ifndef LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11
#define LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11

#include <xlang++/Layout.h>
#include <functional> // grab definition of std::hash and also partial specs for enums.

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

        M6502     = 20,
        M65C02    = 21,
        CLEVER    = 22,

        UNKNOWN = static_cast<std::uint32_t>(-1)
    };

    enum class Vendor : std::uint32_t
    {
        PC = 0,
        APPLE = 1,
        WDC = 3,

        NULLVND = static_cast<std::uint32_t>(-2),
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
        SNES = 7,

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
        NONE = 0,
        AOUT = 1,
        COFF = 2,
        ELF  = 3,
        PE   = 4,
        XCOFF= 5,
        XO65 = 6,

        UNKNOWN = static_cast<std::uint32_t>(-1) 
    };

    struct XLANG_API Target
    {
    private:
        lccc::string name;
        Architecture arch;
        Vendor vendor;
        OperatingSystem os;
        Environment env;
        ObjectFormat of;
    public:
        explicit Target(lccc::string_view) noexcept;

        // Canonical Target Constructor
        Target(Architecture arch,Vendor vendor,OperatingSystem os = OperatingSystem::NONE,
            Environment env=Environment::NONE,ObjectFormat of=ObjectFormat::NONE)
            : arch{arch}, vendor{vendor}, os{os}, env{env}, of{of}{
                name = this->toCanonicalName();
            }

        Target(Architecture arch,Vendor vendor,Environment env,ObjectFormat of=ObjectFormat::NONE) : Target{arch,vendor,OperatingSystem::NONE,env,of}{}
        Target(Architecture arch,Vendor vendor,ObjectFormat of) : Target{arch,vendor,OperatingSystem::NONE,Environment::NONE,of}{}
        

        lccc::string_view getName() const noexcept;

        lccc::string toCanonicalName() const noexcept;

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

        lccc::string_view getObjectFormatName() const noexcept;
        lccc::string_view getCanonicalObjectFormat() const noexcept;
        ObjectFormat getObjectFormat() const noexcept{
            return of;
        }

        bool operator==(const Target& other) const noexcept{
            return this->arch==other.arch&&this->vendor==other.vendor&&this->os==other.os&&this->env==other.env&&this->of==other.of;
        }

        bool operator!=(const Target& other) const noexcept{
            return !(*this==other);
        }
    };
}

namespace std{
    template<> struct hash<lccc::Target>{
    public:
        constexpr hash() noexcept=default;

        std::size_t operator()(const lccc::Target& targ) const noexcept{
            constexpr std::size_t prime = 65537;
            std::size_t h = prime;
            h += std::hash<lccc::Architecture>{}(targ.getArch());
            h *= prime;
            h += std::hash<lccc::Vendor>{}(targ.getVendor());
            h *= prime;
            h += std::hash<lccc::OperatingSystem>{}(targ.getOperatingSystem());
            h *= prime;
            h += std::hash<lccc::Environment>{}(targ.getEnvironment());
            h *= prime;
            h += std::hash<lccc::ObjectFormat>{}(targ.getObjectFormat());
            return h;
        }
    };
}

namespace lccc::xlang{

    struct MachineProperties{
        lccc::span<const lccc::string_view> default_features;

    };
    struct ArchProperties{
        // The nth bit corresponds to the 2^n (bytes) size
        std::uint16_t lock_free_atomic_masks;

        lccc::span<const lccc::string_view> builtin_names;
        lccc::span<const lccc::string_view> target_features;
        lccc::span<const lccc::pair<lccc::string_view,const MachineProperties*>> machines;
        const MachineProperties* default_machine;
    };

    struct OperatingSystemProperties{
        bool is_unix_like;
        bool is_windows_like;
        lccc::span<const lccc::string_view> os_family;
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
        const OperatingSystemProperties* os;
        lccc::span<const lccc::string_view> libdirs;
        lccc::span<const lccc::string_view> default_libs;
        lccc::span<const lccc::string_view> startfiles;
        lccc::span<const lccc::pair<lccc::string_view,bool>> enabled_features;
        lccc::span<const lccc::ObjectFormat> available_formats;
    };

    const TargetProperties* get_properties(const lccc::Target& target);
}

#endif