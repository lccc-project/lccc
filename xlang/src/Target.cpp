#include <xlang++/Target.hpp>

namespace lccc{
    lccc::string_view Target::getName()const noexcept{
        return this->name;
    }

    Target::Target(lccc::string_view name) noexcept : name{name} {
        std::string_view st_name{name};
        
    }

}