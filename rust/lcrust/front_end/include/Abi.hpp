#ifndef LCRUST_FRONTEND_ABI_HPP_2021_03_08_14_16_02
#define LCRUST_FRONTEND_ABI_HPP_2021_03_08_14_16_02

#include <cstdint>

namespace lccc::lcrust{
    namespace abi{
        namespace v0{
            template<typename T> struct Slice{
                T* begin;
                std::uintptr_t size;
            };
            
        }
    }
}

#endif