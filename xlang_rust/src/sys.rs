use std::mem::MaybeUninit;

#[repr(transparent)]
pub struct VTable{
    vfn_ptrs: Option<unsafe extern"C" fn(*const core::ffi::c_void)->()>
}


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


}

