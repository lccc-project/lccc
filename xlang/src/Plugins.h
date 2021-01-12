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

namespace lccc{
    struct Plugin{
    private:
        std::string args_stored;
        std::vector<lccc::string_view> args;
        lccc::unique_ptr<xlang::FileVisitor> visitor;
        std::string name;
        lccc::unique_ptr<xlang::FileVisitor>(*entry_point)(xlang::FileVisitor*,lccc::span<lccc::string_view>);
        void* handle;
    public:
        Plugin(std::string name,std::string args);
        ~Plugin();
        xlang::FileVisitor* load(xlang::FileVisitor* parent);
    };



}

#endif //LCCC_PLUGINS_H
