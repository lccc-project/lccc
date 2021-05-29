
#include <xlang++/Target.hpp>

namespace lccc::xlang{
    extern const lccc::pair<lccc::string_view,const MachineProperties*> x86_machines[];
    extern const lccc::string_view x86_target_features[];
    extern const lccc::xlang::ArchProperties x86_properties;
    extern const lccc::xlang::ArchProperties x86_64_properties;
}