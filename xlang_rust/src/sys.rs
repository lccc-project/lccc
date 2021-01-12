/**
 * xlang-rust/sys.rs
 * This file is part of xlang-rust, a part of the lccc project
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

use std::mem::MaybeUninit;

#[repr(transparent)]
pub struct VTable([Option<unsafe extern"C" fn(*const core::ffi::c_void)->()>;1]);


#[repr(C)]
pub struct Visitor{
    vtbl: *const VTable,
    _vparent: *mut Visitor
}

#[repr(C)]
pub struct VisitorVTable{
    pub __complete_off: isize,
    pub __type_info: *const core::ffi::c_void, // For now
    pub __complete_dtor: Option<unsafe extern"C" fn(*mut std::ffi::c_void)->()>,
    pub __deleting_dtor: Option<unsafe extern"C" fn(*mut std::ffi::c_void)->()>,
    pub visit_end: Option<unsafe extern"C" fn(*mut std::ffi::c_void)->()>,
    pub visit_diagnostic: Option<unsafe extern"C" fn(*mut std::ffi::c_void)->()>,
    pub visit_line: Option<unsafe extern"C" fn(*mut std::ffi::c_void,u64)->()>,
    pub visit_source_file: Option<unsafe extern"C" fn(*mut std::ffi::c_void,crate::layout::StringView)->()>
}




#[allow(non_snake_case)]
extern"C"{

    pub fn _ZN4lccc5xlang7VisitorC1P4lccc5xlang7Visitor(ptr: &mut MaybeUninit<Visitor>,_vparent: *mut Visitor,_VTT: *const VTable);

    pub fn _ZN4lccc5xlang7VisitorC4P4lccc5xlang7Visitor(_vparent: *mut Visitor,_VTT: *const VTable);

    pub fn _ZN4lccc5xlang7VisitorD0v(ptr: *mut Visitor);

    pub fn _ZN4lccc5xlang7VisitorD1v(ptr: *mut Visitor);

    pub fn _ZN4lccc5xlang7Visitor10get_parentP4lccc5xlang7Visitorv(this: &mut Visitor) -> *mut Visitor;

    pub fn _ZN4lccc5xlang7Visitor8visitEndvv(this: &mut Visitor);

    pub fn _ZN4lccc5xlang7Visitor15visitDiagnosticvN4lccc17basic_string_viewIcE(this: &mut Visitor,diag: crate::layout::StringView);



}

