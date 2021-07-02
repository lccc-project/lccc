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
        std::abort();
    }

}