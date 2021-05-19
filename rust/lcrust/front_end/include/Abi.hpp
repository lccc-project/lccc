#ifndef LCRUST_FRONTEND_ABI_HPP_2021_03_08_14_16_02
#define LCRUST_FRONTEND_ABI_HPP_2021_03_08_14_16_02

#include <cstdint>

#include <xlang++/Properties.h>
#include <xlang++/Layout.h>

namespace lccc::lcrust{
    namespace abi{
        namespace v0{
            template<typename T> struct Slice{
                T* begin;
                std::uintptr_t size;

                constexpr explicit Slice(T(&arr)[N]) noexcept : 
                    begin{limit(static_cast<T*>(arr),N)},size{static_cast<std::uintptr_t>(N)}{}
            };


            struct TypeId{
            private:
                static_assert(sizeof(std::uintptr_t)==4||sizeof(std::uintptr_t)==8,"lccc does not support targets with pointer sizes other than 4 or 8");
                constexpr static std::uintptr_t get_hash_init(){
                    if constexpr(sizeof(std::uintptr_t)==4)
                        return 2166136261;
                    else if constexpr(sizeof(std::uintptr_t)==8)
                        return 14695981039346656037;
                    else 
                        return -1; // This line is unreachable by the above static_assert.
                }

                constexpr static std::uintptr_t get_hash_prime(){
                    if constexpr(sizeof(std::uintptr_t)==4)
                        return 16777619;
                    else if constexpr(sizeof(std::uintptr_t)==8)
                        return 1099511628211;
                    else
                        return -1; // Line is also unreachable
                }

                constexpr static std::uintptr_t type_name_hash(const char* name){
                    std::uintptr_t hash = get_init_val();
                    while(*name){
                        hash ^= static_cast<std::uintptr_t>(*name++);
                        hash *= get_hash_prime;
                    }
                    return hash;
                }
            public:
                const char* name;
                std::uintptr_t hash;

                constexpr explicit TypeId(const char* name) : name{name}, hash{type_name_hash(name)}{}
                

                constexpr bool operator==(const TypeId& other) const noexcept{
                    if(this->name==other.name){
                        return true;
                    }else if(this->hash!=other.hash){
                        return false;
                    }else{
                        const char* c1 = this->name;
                        const char* c2 = other.name;
                        while(*c1){
                            if(*c1!=*c2)
                                return false;
                            c1++;
                            c2++;  
                        }
                        return true;
                    }
                }

                constexpr bool operator!=(const TypeId& other) const noexcept{
                    return !((*this)==other);
                }
            };
            
        }
    }
}

#endif