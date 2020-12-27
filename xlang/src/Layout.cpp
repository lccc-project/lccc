//
// Created by chorm on 2020-12-24.
//

#include <xlang++/Layout.h>

template struct lccc::basic_string_view<char>;

using namespace lccc;
using namespace std::string_view_literals;

lccc::string_view Target::getName()const noexcept{
    return this->name;
}

Target::Target(lccc::string_view name) noexcept : name{name} {
    std::string_view st_name{name};

}

