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

#include <Config.hpp>
#include <xlang++/Plugins.h>
#include <dlfcn.h>
#include <system_error>
#include <stdexcept>
#include <string>
#include <unistd.h>

using namespace std::string_literals;

namespace lccc
{
    [[noreturn]] XLANG_API void throw_symbol_not_found(lccc::string_view sv)
    {
        throw std::runtime_error{std::string{sv}};
    }
    struct PluginImpl
    {
        void *hdl;
        lccc::string_view name;
    };

} // namespace lccc
