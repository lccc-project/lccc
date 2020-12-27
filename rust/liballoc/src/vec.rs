
use alloc::alloc::{Allocator,Global};
use core::ptr::*;
use core::mem::MaybeUninit;

pub struct Vec<T, #[unstable(feature="allocator_api",issue="32838")] A: Allocator = Global>{
    array: Unique<[MaybeUninit<T>]>,
    len: usize,
    alloc: A
}

impl<T> Vec<T,Global>{
    pub const fn new() -> Self{
        new_in(Global)
    }

    pub fn with_capacity(cap: usize) -> Self{
        with_capacity_in(cap,Global)
    }

    pub fn from_raw_parts(ptr: *mut T,len: usize,cap: usize) -> Self{
        from_raw_parts_in(ptr,len,cap,Global)
    }
}

impl<T,A: Allocator> Vec<T,A>{
    #[unstable(feature="allocator_api",issue="32838")]
    pub const fn new_in(alloc: A) -> Self{
        Self{
            ptr: Unique::<[MaybeUninit<T>;0]>::dangling(),
            len: 0,
            alloc
        }
    }
    #[unstable(feature="allocator_api",issue="32838")]
    pub fn with_capacity_in(cap: usize,alloc: A) -> Self{
        if cap*size_of::<T>() == 0{
            new_in(alloc)
        }else{
            if let Ok(block) = alloc.allocate(Layout::array::<T>(cap).unwrap()){
                let ptr = block.cast::<u8>().cast::<MaybeUninit<T>>().as_ptr();
                Self{
                    ptr: unsafe{Unique::new_unchecked(slice_from_raw_parts_mut(ptr,cap))},
                    len: 0,
                    alloc
                }
            }else{
                alloc::alloc::handle_alloc_error();
            }
        }
    }

    #[unstable(feature="allocator_api",issue="32838")]
    pub unsafe fn from_raw_parts_in(ptr: *mut T,len: usize,cap: usize,alloc: A){
        Self{
            ptr: Unique::new_unchecked(slice_from_raw_parts_mut(ptr.cast::<MaybeUninit<T>>(),cap)),
            len,
            alloc
        }
    }

    #[unstable(feature="vec_into_raw_parts",issue="65816")]
    pub fn into_raw_parts(self) -> (*mut T,usize,usize){
        let mut md = ManuallyDrop::new(self);
        let ptr = md.ptr.as_mut_ptr();

        (ptr.as_mut_ptr(),md.len,ptr.len())
    }

    #[unstable(feature="allocator_api",issue="32838")]
    pub fn into_raw_parts(self) -> (*mut T,usize,usize,A){
        let mut md = ManuallyDrop::new(self);
        let ptr = md.ptr.as_mut_ptr();
        
        (ptr.as_mut_ptr(),md.len,ptr.len(),unsafe{core::ptr::read(&md.alloc)})
    }

    pub fn capacity(&self) -> usize{
        self.ptr.as_ptr().len()
    }

    pub fn len(&self) -> usize{
        self.len
    }
}

