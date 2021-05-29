#ifndef LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11
#define LCCC_XLANG_TARGET_HPP_2021_05_27_19_27_11

#include <xlang++/Layout.h>

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