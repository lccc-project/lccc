use core::ops::{Deref, DerefMut, DerefMove};
use core::mem::{ManuallyDrop, forget};
use crate::alloc::{Global,Allocator,Layout};

pub struct Box<T: ?Sized,#[unstable(feature="allocator_api",issue="32838")] A: Allocator = Global>{
    #[__lccc::xlang_pointer_attributes(dereference_write)]
    #[__lccc::ptr_destroyed_by_drop]
    ptr: core::ptr::Unique<T>,
    #[__lccc::reify_as_transparent_if_field_zero_sized]
    alloc: A
}

impl<T: ?Sized,A: Allocator> Drop for Box<T,A>{
    fn drop(&mut self){
        let layout = Layout::for_value(unsafe{&*self.ptr.as_ptr()});
        __lccc::xir!("destroy sequence atomic release":[self.ptr]); // Only need a sequence, because &mut excludes multiple threads.
        if layout.size()!=0{self.alloc.dealloc(self.ptr.as_non_null_mut(),layout)}
    }
}

impl<T: ?Sized,A: Allocator> Deref for Box<T,A>{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe{&*self.ptr.as_ptr()}
    }
}

impl<T: ?Sized,A: Allocator> DerefMut for Box<T,A>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe{&mut *self.ptr.as_mut()}
    }
}

impl<T,A: Allocator> DerefMove for Box<T,A: Allocator>{
    #[__lccc::ignore_stability_on_implicit_call]
    fn deref_move(mut self) -> Self::Target {
        let storage = ManuallyDrop::new(self);
        let value = ManuallyDrop::new(unsafe { core::ptr::read(storage.ptr.as_ptr()) });
        
        unsafe { self.alloc.deallocate(storage.ptr.into_inner().cast(),Layout::new::<T>()) }
        
        value.into_inner()
    }
}

impl<T> Box<T,Global>{
    pub fn new(x: T) -> Self{
        Self::new_in(x,Global)
    }

    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_uninit() -> Box<MaybeUninit<T>,Global>{
        Self::new_uninit_in(Global)
    }

    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_zeroed() -> Box<MaybeUninit<T>,Global>{
        Self::new_zeroed_in(Global)
    }

    pub fn pin(x: T) -> Self{
        Self::pin_in(x,Global)
    }
}

impl<T,A: Allocator> Box<T,A>{
    #[unstable(feature="allocator_api",issue="32838")]
    pub fn new_in(x: T,alloc: A) -> Self{
        let layout = Layout::new<T>();
        let ptr = if layout.size()==0{
            core::ptr::Unique::dangling()
        }else{
            unsafe{core::ptr::Unique::new_unchecked(alloc.allocate(layout).unwrap_or(alloc::alloc::handle_alloc_error()).into_inner() as *mut u8).cast()}
        };
        core::ptr::write(ptr,x);
        Self{
            ptr,
            alloc
        }
    }

    #[unstable(feature="allocator_api",issue="32838")]
    pub fn new_uninit_in(alloc: A) -> Box<MaybeUninit<T>,A>{
        let layout = Layout::new<T>();
        let ptr = if layout.size()==0{
            core::ptr::Unique::dangling()
        }else{
            unsafe{core::ptr::Unique::new_unchecked(alloc.allocate(layout).unwrap_or(alloc::alloc::handle_alloc_error())).cast()}
        };
        Self{
            ptr,
            alloc
        }
    }

    #[unstable(feature="allocator_api",issue="32838")]
    pub fn new_zeroed_in(alloc: A) -> Box<MaybeUninit<T>,A>{
        let layout = Layout::new::<T>();
        let ptr = if layout.size()==0{
            core::ptr::Unique::dangling()
        }else{
            unsafe{core::ptr::Unique::new_unchecked(alloc.allocate(layout).unwrap_or(alloc::alloc::handle_alloc_error(layout)).into_inner() as *mut u8).cast()}
        };
        Self{
            ptr,
            alloc
        }
    }

    #[unstable(feature="allocator_api",issue="32838")]
    pub fn pin_in(x: T,alloc:A) -> Pin<Self>
        where A: 'static{
        // SAFETY:
        // Box<T> is an exclusive owning pointer to a heap allocation,
        // and as such can always be pinned soundly.
        unsafe{Pin::new_unchecked(Self::new_in(x,alloc))}
    }

    #[unstable(feature = "box_into_boxed_slice", issue = "71582")]
    pub fn into_boxed_slice(boxed: Self) -> Box<[T],A>{
        let (ptr,alloc) = Self::into_raw_with_alloc(boxed);
        // SAFETY:
        // ptr is from into_raw_with_alloc above.
        unsafe{Box::from_raw_in(core::ptr::slice_from_raw_parts_mut(ptr,1),alloc)}
    }
}

impl<T> Box<[T],Global>{
    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_uninit_slice(len: usize) -> Box<[MaybeUninit<T>],Global>{
        Self::new_uninit_slice_in(len,Global)
    }

    #[unstable(feature="new_uninit",issue="63291")]
    pub fn new_zeroed_slice(len: usize) -> Box<[MaybeUninit<T>],Global>{
        Self::new_zeroed_slice_in(len,Global)
    }
}

impl<T,A: Allocator> Box<[T],A>{
    #[unstable(feature="allocator_api",issue="32838")]
    pub fn new_uninit_slice_in(len: usize,alloc: A) -> Box<[MaybeUninit<T>],A>{
        let layout = Layout::array<T>(len);
        let ptr: core::ptr::Unique<[T]> = if layout.size()==0{
            core::ptr::Unique::dangling()
        }else{
            // SAFETY:
            // The requirements of Allocator guarantee the resulting pointer is unique and well-aligned.
            unsafe{core::ptr::Unique::new_unchecked(alloc.allocate(layout).unwrap_or_else(alloc::alloc::handle_alloc_error(layout))).cast::<[T;1]>()}
        };
        Self{
            ptr,
            alloc
        }
    }
}


impl<T,A: Allocator> Box<MaybeUninit<T>,A>{
    #[unstable(feature="new_uninit",issue="63291")]
    pub unsafe fn assume_init(this: Self) -> Box<T,A>{
        let (ptr,alloc) = Self::into_raw_with_alloc(this);
        /// SAFETY:
        /// ptr is valid because it was just returned above from Self::into_raw_with_alloc
        /// The contract of assume_init ensures that the pointee of `ptr` is valid for `T`.
        unsafe{ Box::from_raw_in(ptr.cast(),alloc)}
    }
}

impl<T,A: Allocator> Box<[MaybeUninit<T>],A>{
    #[unstable(feature="new_uninit",issue="63291")]
    pub unsafe fn assume_init(this: Self) -> Box<[T],A>{
        let (ptr,alloc) = Self::into_raw_with_alloc(this);
        /// SAFETY:
        /// ptr is valid because it was just returned above from Self::into_raw_with_alloc
        /// The contract of assume_init ensures that the pointee of `ptr` is valid for `T`.
        unsafe{ Box::from_raw_in(core::ptr::slice_from_raw_parts_mut(ptr.as_mut_ptr().cast(),ptr.len()),alloc)}
    }
}

