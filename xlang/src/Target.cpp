#include <xlang++/Target.hpp>

namespace lccc{
    lccc::string_view Target::getName()const noexcept{
        return this->name;
    }

    Target::Target(lccc::string_view name) noexcept : name{name} {
        std::string_view st_name{name};
        
    }

    lccc::string_view Target::getCanonicalArch() const noexcept{
        switch(this->arch){
        case Architecture::X86_64:
            return "x86_64"_sv;
        case Architecture::IX86:
            return "i386"_sv;
        case Architecture::W65816:
            return "w65"_sv;
        case Architecture::AARCH64:
            return "aarch64"_sv;
        case Architecture::ARM:
            return "arm"_sv;
        case Architecture::SUPERH:
            return "superh"_sv;
        case Architecture::AVR:
            return "avr"_sv;
        case Architecture::MIPS:
            return "mips"_sv;
        case Architecture::POWERPC:
            return "powerpc"_sv;
        case Architecture::POWERPC64:
            return "powerpc64"_sv;
        case Architecture::RISCV32:
            return "riscv32"_sv;
        case Architecture::RISCV64:
            return "riscv64"_sv;
        case Architecture::WASM32:
            return "wasm32"_sv;
        case Architecture::WASM64:
            return "wasm64"_sv;
        case Architecture::M6502:
            return "m6502"_sv;
        case Architecture::M65C02:
            return "m65c02"_sv;
        case Architecture::M68000:
            return "m68k"_sv;
        case Architecture::CLEVER:
            return "clever"_sv;
        case Architecture::SPARC:
            return "sparc"_sv;
        case Architecture::UNKNOWN:
            return "unknown"_sv;
        }
        std::abort(); // TODO get better exception handling
    }

    lccc::string_view Target::getCanonicalVendor() const noexcept{
        switch(this->getVendor()){
        case Vendor::PC:
            return "pc"_sv;
        case Vendor::NULLVND: // Should never happen, but just in case, omitted=unknown
        case Vendor::UNKNOWN:
            return "unknown"_sv;
        case Vendor::APPLE:
            return "apple"_sv;
        case Vendor::WDC:
            return "wdc"_sv;
        }
        std::abort(); // TODO get better exception handling
    }

    lccc::string_view Target::getCanonicalOperatingSystem() const noexcept{
        switch(this->os){
        case OperatingSystem::NONE:
            return "none"_sv;
        case OperatingSystem::LINUX:
            return "linux"_sv;
        case OperatingSystem::WINDOWS:
            return "windows"_sv;
        case OperatingSystem::MINGW32:
            return "mingw32"_sv;
        case OperatingSystem::MACOS:
            return "macos"_sv;
        case OperatingSystem::IOS:
            return "ios"_sv;
        case OperatingSystem::PHANTOM:
            return "phantom"_sv;
        case OperatingSystem::SNES:
            return "snes"_sv;
        case OperatingSystem::UNKNOWN:
            return "unknown"_sv;
        }
        std::abort(); // TODO get better exception handling
    }

    lccc::string_view Target::getCanonicalEnvironment() const noexcept{
        switch(this->env){
        case Environment::NONE:
            return ""_sv;
        case Environment::GNU:
            return "gnu"_sv;
        case Environment::EABI:
            return "eabi"_sv;
        case Environment::MSVC:
            return "msvc"_sv;
        case Environment::LC:
            return "lc"_sv;
        case Environment::PHANTOM_KERNEL:
            return "kernel"_sv;
        case Environment::PHANTOM_STD:
            return "user"_sv;
        case Environment::UNKNOWN:
            return "unknown"_sv;
        case Environment::MUSL:
            return "musl"_sv;
        }
        std::abort();
    }

    lccc::string_view Target::getCanonicalObjectFormat() const noexcept{
        switch(this->of){
        case ObjectFormat::NONE: // Note: Impossible
            return "none"_sv;
        case ObjectFormat::AOUT:
            return "aout"_sv;
        case ObjectFormat::COFF:
            return "coff"_sv;
        case ObjectFormat::XCOFF:
            return "xcoff"_sv;
        case ObjectFormat::PE:
            return "pe"_sv;
        case ObjectFormat::ELF:
            return "elf"_sv;
        case ObjectFormat::XO65:
            return "xo65"_sv;
        case ObjectFormat::UNKNOWN:
            return "unknown"_sv;
        }
        std::abort();
    }


}