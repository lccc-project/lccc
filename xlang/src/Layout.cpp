/**
 * xlang++/Layout.cpp
 * This file is part of libxlang, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  libxlang is additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

#include <xlang++/layout/Hash.hpp>
#include <xlang++/layout/StringView.hpp>
#include <xlang++/layout/Allocator.hpp>
#include <xlang++/layout/UnorderedMap.hpp>


#include <random>

template struct lccc::basic_string_view<char>;
template struct lccc::basic_string_view<char16_t>;
template struct lccc::basic_string_view<char32_t>;

template struct lccc::hash<char>;
template struct lccc::hash<unsigned char>;
template struct lccc::hash<signed char>;
template struct lccc::hash<short>;
template struct lccc::hash<unsigned short>;
template struct lccc::hash<int>;
template struct lccc::hash<unsigned>;
template struct lccc::hash<long>;
template struct lccc::hash<unsigned long>;
template struct lccc::hash<long long>;
template struct lccc::hash<unsigned long long>;
template struct lccc::hash<void*>;

using namespace lccc;
using namespace std::string_view_literals;


namespace lccc{
    extern"C"{
        void* xlang_allocate(std::size_t sz)noexcept{
            return ::operator new(sz,std::nothrow);
        }

        void* xlang_allocate_aligned(std::size_t sz,std::size_t a)noexcept{
            return ::operator new(a,std::align_val_t{a},std::nothrow);
        }

        void xlang_deallocate(void* v)noexcept{
            ::operator delete(v);
        }

        void xlang_deallocate_aligned(void* v,std::size_t a)noexcept{
            ::operator delete(v,std::align_val_t{a});
        }

        const char xlang_hash_seed = 0xff;

        std::size_t xlang_hash_scalar(const void* v,std::size_t sz){
            constexpr std::size_t hash_init = sizeof(std::size_t)==4?2166136261:sizeof(std::size_t)==8?14695981039346656037ull:0;
            static const std::size_t seed{(hash_init^std::random_device{}())*xlang_hash_prime};
            std::size_t hash{seed};
            for(const unsigned char* c = static_cast<const unsigned char*>(v);c!=static_cast<const unsigned char*>(v)+sz;c++){
                hash ^= *c;
                hash *= xlang_hash_prime;
            }
            return hash;
        }
    }
}


namespace lccc{
    std::size_t _next_hashtable_capacity(std::size_t s){
        return s+6;
    }
}
