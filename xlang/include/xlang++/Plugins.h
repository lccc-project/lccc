/*
    The file is part of the lccc project. 
    Copyright (C) 2020-2021, Lightning Creations

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef LCCC_PLUGINS_H
#define LCCC_PLUGINS_H

#include <xlang++/Layout.h>
#include <xlang++/Visit.hpp>
#include <vector>
#include <string>

namespace lccc
{
    struct PluginImpl;
    [[noreturn]] XLANG_API void throw_symbol_not_found(lccc::string_view);
    struct XLANG_API Plugin
    {
    private:
        lccc::unique_ptr<struct PluginImpl> impl;
        lccc::unique_ptr<xlang::FileVisitor> visitor;
        void *find_sym(lccc::string_view name);
        
    public:
        Plugin(lccc::string_view name, lccc::span<lccc::string_view> args);
        ~Plugin();
        xlang::FileVisitor *load(xlang::FileVisitor *parent);
        template <typename T>
        T &find_symbol(lccc::string_view name)
        {
            if (void *v = find_sym(name); v)
                return *static_cast<T *>(v);
            else
                throw_symbol_not_found(name);
        }
    };

} // namespace lccc

#endif //LCCC_PLUGINS_H
