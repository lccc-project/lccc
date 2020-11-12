

#[non_exhaustive]
#[derive(Clone,Copy,Ord, PartialOrd, Eq, PartialEq,Hash)]
pub enum Ordering{
    Relaxed,
    Release,
    Acquire,
    AcqRel,
    SeqCst
}

#[inline]
#[__lccc::xlang_opt_hint(contains_sequence)]
pub fn compiler_fence(ord: Ordering){
    match ord{
        Ordering::Release => {__lccc::xir!("sequence atomic release")}
        Ordering::Acquire => {__lccc::xir!("sequence atomic acquire")}
        Ordering::AcqRel => {__lccc::xir!("sequence atomic acqrel")}
        Ordering::SeqCst => {__lccc::xir!("sequence atomic seq_cst")}
        _ => panic!("Invalid ordering {}")
    }
}

#[inline]
#[__lccc::xlang_opt_hint(contains_fence)]
pub fn fence(ord: Ordering){
    match ord{
        Ordering::Release => {__lccc::xir!("fence atomic release")}
        Ordering::Acquire => {__lccc::xir!("fence atomic acquire")}
        Ordering::AcqRel => {__lccc::xir!("fence atomic acqrel")}
        Ordering::SeqCst => {__lccc::xir!("fence atomic seq_cst")}
        _ => panic!("Invalid ordering {}")
    }
}

#[cfg(target_has_atomic = "8")]
#[repr(C,align(1))]
pub struct AtomicBool{
    inner: UnsafeCell<u8>
}

#[cfg(target_has_atomic = "8")]
impl Default for AtomicBool{
    fn default(){
        AtomicBool::new(false)
    }
}

#[cfg(target_has_atomic = "8")]
unsafe impl Sync for AtomicBool{}

#[cfg(target_has_atomic = "8")]
impl AtomicBool{
    pub const fn new(x: bool) -> Self{
        Self{inner: UnsafeCell::new(x as u8)}
    }

    pub fn get_mut(&mut self) -> &mut bool{
        unsafe{transmute(self.inner.get_mut())}
    }

    #[unstable(feature="atomic_from_mut",issue="76314")]
    pub fn from_mut(x: &mut bool) -> &Self{
        unsafe{&*(x as *mut bool as *mut Self as *const Self)}
    }

    pub fn into_inner(self) -> bool{
        unsafe{*self.inner.get()}
    }

    pub fn load(&self,ord: Ordering) -> bool{
        unsafe{transmute(ops::atomic_load(self.inner.get(),ord))}
    }
}

mod ops{
    pub fn atomic_load<T>(mem: *const T,ord: Ordering){
        if ::__lccc::atomic_not_lock_free!(T) {
            panic!("Cannot perform non-lock free atomic load")
        }

        match ord{
            Acquire | AcqRel => __lccc::xir!(r"(
                local {}
                as_rvalue
                indirect
                as_rvalue atomic acquire
            )":mem:[T]),
            SeqCst => __lccc::xir!(r"(
                local {}
                as_rvalue
                indirect
                as_rvalue atomic seqcst
            )":mem:[T]),
            _ => panic!("Bad ordering")
        }
    }


}