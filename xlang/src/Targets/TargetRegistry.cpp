#include "TargetRegistry.hpp"
namespace lccc::xlang{


    std::unordered_map<lccc::Target,const lccc::xlang::TargetProperties*>& get_target_registry(){
        static std::unordered_map<lccc::Target,const lccc::xlang::TargetProperties*> target_registry;
        return target_registry;
    }

    const TargetProperties* get_properties(const lccc::Target& target){
        auto& registry = get_target_registry();
        if(registry.count(target))
            return registry[target];
        else
            return nullptr;
    }

}