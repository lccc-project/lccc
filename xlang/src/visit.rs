use xlang_abi::traits::*;

pub trait Visitor {
    extern "C" fn visit_end(&mut self);
}

pub struct VisitorVTable {
    pub size: usize,
    pub align: usize,
    pub destructor: Option<unsafe extern "C" fn(*mut ())>,
    pub reserved: Option<unsafe extern "C" fn(*mut ())>,
    pub visit_end: unsafe extern "C" fn(*mut ()),
}

unsafe impl AbiSafeVTable<dyn Visitor> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Send> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Sync> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Send + Sync> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Send + Unpin> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Sync + Unpin> for VisitorVTable {}
unsafe impl AbiSafeVTable<dyn Visitor + Send + Sync + Unpin> for VisitorVTable {}

unsafe impl AbiSafeTrait for dyn Visitor {
    type VTable = VisitorVTable;
}

unsafe impl AbiSafeTrait for dyn Visitor + Send {
    type VTable = VisitorVTable;
}

unsafe impl AbiSafeTrait for dyn Visitor + Sync {
    type VTable = VisitorVTable;
}

unsafe impl AbiSafeTrait for dyn Visitor + Send + Sync {
    type VTable = VisitorVTable;
}

unsafe extern "C" fn destructor<T>(x: *mut ()) {
    core::ptr::drop_in_place(x as *mut T)
}

unsafe extern "C" fn visit_end<T: Visitor>(x: *mut ()) {
    <T as Visitor>::visit_end(&mut *(x as *mut T))
}

unsafe impl<T: Visitor> AbiSafeUnsize<T> for dyn Visitor {
    fn construct_vtable_for() -> &'static Self::VTable {
        &VisitorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(destructor::<T>),
            reserved: None,
            visit_end: visit_end::<T>,
        }
    }
}

unsafe impl<T: Visitor + Send> AbiSafeUnsize<T> for dyn Visitor + Send {
    fn construct_vtable_for() -> &'static Self::VTable {
        &VisitorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(destructor::<T>),
            reserved: None,
            visit_end: visit_end::<T>,
        }
    }
}

unsafe impl<T: Visitor + Send> AbiSafeUnsize<T> for dyn Visitor + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &VisitorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(destructor::<T>),
            reserved: None,
            visit_end: visit_end::<T>,
        }
    }
}

unsafe impl<T: Visitor + Send> AbiSafeUnsize<T> for dyn Visitor + Send + Sync {
    fn construct_vtable_for() -> &'static Self::VTable {
        &VisitorVTable {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
            destructor: Some(destructor::<T>),
            reserved: None,
            visit_end: visit_end::<T>,
        }
    }
}

impl<'lt> Visitor for DynPtrSafe<'lt, dyn Visitor> {
    extern "C" fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_mut_ptr()) }
    }
}

impl<'lt> Visitor for DynPtrSafe<'lt, dyn Visitor + Send> {
    extern "C" fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_mut_ptr()) }
    }
}

impl<'lt> Visitor for DynPtrSafe<'lt, dyn Visitor + Sync> {
    extern "C" fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_mut_ptr()) }
    }
}

impl<'lt> Visitor for DynPtrSafe<'lt, dyn Visitor + Send + Sync> {
    extern "C" fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_mut_ptr()) }
    }
}
