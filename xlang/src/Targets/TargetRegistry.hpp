#ifndef LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23
#define LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23

#include <xlang++/Target.hpp>

#include <unordered_map>

#include <initializer_list>

namespace lccc::xlang{

    std::unordered_map<lccc::Target,const lccc::xlang::TargetProperties*>& get_target_registry();

    struct RegisterTargets{
    public:
        RegisterTargets(const lccc::xlang::TargetProperties* props,std::initializer_list<lccc::Target> targets){
            std::unordered_map<lccc::Target,const lccc::xlang::TargetProperties*>& map = get_target_registry();
            for(const auto& a:targets)
                map.insert({a,props});
        }

    };

}

#endif /* LCCC_XLANG_TARGETS_TARGETREGISTERY_HPP_2021_06_04_09_23_23 */