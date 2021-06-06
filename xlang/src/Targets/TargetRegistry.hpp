#ifndef LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23
#define LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23

#include <xlang++/Target.hpp>

#include <unordered_map>

namespace lccc::xlang{

    struct hash_fields{
        std::size_t operator()(const lccc::Target& target){
            return std::hash<lccc::Architecture>(target.getArcg*())
        }
    };

    std::
}

#endif /* LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23 */