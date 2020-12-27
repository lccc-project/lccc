
use alloc::alloc::{Allocator,Global};
use core::ptr::*;
use core::mem::MaybeUninit;

#[repr(C)]
struct RcControlBlock<T: ?Sized>{
    strong_count: Cell<usize>,
    weak_count: Cell<usize>,
    value: T
}

pub struct Rc<T: ?Sized,#[unstable(feature="allocator_api",issue="32838")] A: Allocator = Global>(NonNull<RcControlBlock<T>>,A);

pub struct Weak<T: ?Sized,#[unstable(feature="allocator_api",issue="32838")] A: Allocator = Global>(NonNull<RcControlBlock<T>>,A);

impl<T: ?Sized,A> !Send for Rc<T,A>{}
impl<T: ?Sized,A> !Sync for Rc<T,A>{}

impl<T> Rc<T>{
    pub fn new(value: T) -> Self{
        new_in(value,Global)
    }

    #[unstable(feature="arc_new_cyclic",issue="75861")]
    pub fn new_cyclic(data_fn: impl FnOnce(&Weak<T>)->T)->Self{
        new_cyclic_in(data_fn,Global)
    }

    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_uninit() -> Rc<MaybeUninit<T>>{
        new_uninit_in(Global)
    }

    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_zeroed() -> Rc<MaybeUninit<T>>{
        new_zeroed_in(Global)
    }

    pub fn pin(x: T) -> Rc<T>{
        pin_in(x,Global)
    }
}

impl<T,A: Allocator> Rc<T,A>{
    #[unstable(feature="allocator_api",issue="32838")]
    pub fn new_in(x: T,alloc: A) -> Self{
        
    }
}
