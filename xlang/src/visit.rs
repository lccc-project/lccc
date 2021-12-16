use xlang_abi::traits::{AbiSafeTrait, AbiSafeUnsize, AbiSafeVTable, DynPtrSafe};

pub trait Visitor {
    fn visit_end(&mut self);
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
    core::ptr::drop_in_place(x.cast::<T>());
}

unsafe extern "C" fn visit_end<T: Visitor>(x: *mut ()) {
    <T as Visitor>::visit_end(&mut *x.cast::<T>());
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

impl<'lt> Visitor for dyn DynPtrSafe<dyn Visitor> + 'lt {
    fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_raw_mut()) }
    }
}

impl<'lt> Visitor for dyn DynPtrSafe<dyn Visitor + Send> + 'lt {
    fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_raw_mut()) }
    }
}

impl<'lt> Visitor for dyn DynPtrSafe<dyn Visitor + Sync> + 'lt {
    fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_raw_mut()) }
    }
}

impl<'lt> Visitor for dyn DynPtrSafe<dyn Visitor + Send + Sync> + 'lt {
    fn visit_end(&mut self) {
        let vtbl = self.vtable();
        unsafe { (vtbl.visit_end)(self.as_raw_mut()) }
    }
}
