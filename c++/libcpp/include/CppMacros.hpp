//
// Created by chorm on 2020-04-25.
//

#ifndef LCCC_CPPMACROS_HPP
#define LCCC_CPPMACROS_HPP

#include <type_traits>
#include <string>
#include <vector>


namespace lccc::cpp{
    class Macro{
    public:
        virtual ~Macro()=default;
        virtual std::string replace(std::vector<std::string> params)const=0;
    };
}
#endif //LCCC_CPPMACROS_HPP
