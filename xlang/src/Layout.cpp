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

#include <xlang++/Layout.h>

template struct lccc::basic_string_view<char>;

using namespace lccc;
using namespace std::string_view_literals;

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

}

