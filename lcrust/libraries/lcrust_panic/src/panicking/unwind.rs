
pub use crate::rt::unwind::sys::{ForeignExceptionType,PanicUnwindInfo, FOREIGN_EXCEPTION_INIT};

use crate::rt::unwind::sys::{__dispose_foreign_exception,__begin_unwind};

use core::sync::atomic::AtomicUsize;
use core::cell::Cell;
use core::any::Any;




#[cfg_attr(define_lang_items, lang = "lcrust_dispose_foreign_exception_symbol")]
pub unsafe fn dispose_foreign_exception(p: *mut ForeignExceptionType){
    __dispose_foreign_exception(p)
}

#[track_caller]
#[cfg_attr(define_lang_items, lang = "lcrust_allocate_exception_symbol")]
pub fn __allocate_exception(obj_layout: Layout) -> *mut PanicUnwindInfo {
    let base_layout = Layout::of::<PanicUnwindInfo>();

    let (layout, _) = base_layout.extend(obj_layout).unwrap_or_else(|| {
        abort_fmt(
            Location::caller(),
            format_args!(
                "Allocation Failure in panic runtime - object with size {} cannot be panicked with",
                obj_layout.size()
            ),
        )
    });

    let layout = layout.pad_to_align();

    // SAFETY: `layout` is non-zero size because it as at least `base_layout`
    let ptr = unsafe { alloc::alloc::alloc(layout) };

    if ptr.is_null() {
        abort_fmt(Location::caller(), format_args!("Allocation Failure in panic runtime - global allocator returned OOM for allocation of size {}",layout.size()));
    }

    ptr.cast()
}

#[repr(lcrust_v0)]
struct AnyVtable {
    size: usize,
    align: usize,
    destructor: Option<unsafe extern "lcrust-v0" fn(*mut ())>,
    reserved_dealloc: Option<unsafe extern "lcrust-v0" fn(*mut ())>,
    typeid: unsafe extern "lcrust-v0" fn(*mut ()) -> core::any::TypeId,
}

#[track_caller]
#[cfg_attr(define_lang_items, lang = "lcrust_deallocate_excepton_symbol")]
pub unsafe fn __deallocate_exception(obj: *mut PanicUnwindInfo) {
    let base_layout = Layout::of::<PanicUnwindInfo>();

    let vtable: core::ptr::DynMetadata<dyn Any> = (*obj).vtable;

    let vtable_access = core::mem::transmute::<&'static AnyVtable>(vtable);

    let obj_layout = Layout::from_size_align_unchecked(vtable_access.size, vtable_access.align);

    let layout = base_layout
        .extend(obj_layout)
        .unwrap_unchecked()
        .0
        .pad_to_align();

    alloc::alloc::dealloc(obj.cast, layout)
}

#[track_caller]
#[cfg_attr(define_lang_items, lang = "lcrust_begin_catch_native_symbol")]
pub unsafe fn begin_catch_native(p: *mut PanicUnwindInfo) -> *mut dyn Any{
    let vtable: core::ptr::DynMetadata<dyn Any> = (*obj).vtable;

    let vtable_access = core::mem::transmute::<&'static AnyVtable>(vtable);

    let obj_layout = Layout::from_size_align_unchecked(vtable_access.size, vtable_access.align);

    let objptr = unsafe{p.cast::<u8>().add(core;:mem::size_of::<PanicUnwindInfo>())
        .add((*obj).tail_size)
        .map_addr(|a| (a + vtable_access.align.sub_unchecked()) & !vtable_access.align)};

    let ptr = if obj_layout.size()==0{
        obj_layout.dangling().as_mut()
    }else{
        unsafe{alloc::alloc::alloc(obj_layout)}
    };

    if ptr.is_null(){
        abort_fmt(Location::caller(),format_args!("Failed to allocate returned exception with layout: size={}, align={}",vtable_access.size,table_access.align))
    }

    decrement_panic_count();

    // Safety: `ptr` is freshly allocated

    unsafe{core::ptr::copy_nonoverlapping(objptr,ptr,vtable_access.size)};

    __deallocate_exception(p);

    core::ptr::from_raw_parts(ptr.cast(),vtable)

}

#[cold]
#[inline(never)]
#[cfg_attr(define_lang_items, lang = "lcrust_begin_unwind_symbol")]
pub unsafe fn begin_unwind(o: *mut PanicUnwindInfo) -> !{
    __begin_unwind(o)
}


#[cfg_attr(define_lang_items, lang = "lcrust_catch_unwind_landing_pad_symbol")]
pub unsafe fn catch_unwind_landing_pad(_: *mut dyn FnOnce(), p: *mut c_void) -> Result<(),Box<dyn Any>>{
    Err(__catch_unwind_landing_pad(p))
}



#[cfg_attr(define_lang_items, lang = "lcrust_begin_panic_symbol")]
#[cfg_attr(define_lang_items, lcrust::weak_def)]
#[cfg_attr(not(define_lang_items), panic_handler)]
#[cold]
#[inline(never)]
pub fn begin_panic(info: &core::panic::PanicInfo) -> !{
    let count = unsafe{increment_panic_count()};

    if count > 1{
        unsafe{abort_fmt(info.location(), format_args!("Immediate abort from panics: {} active panics on thread", count))}
    }else{
        panic_call_hook(info);

        if count > 0 || ALWAYS_ABORT.get(Ordering::Relaxed) != 0{
            unsafe{abort(info.location(), Some("Double-panic: Aborting process"))}
        }

        // Per lcrust abi v0: `PanicInfo` constructed by the implementation shall have a location valid for the duration of the program
        let location = unsafe{core::mem::transmute(core::ptr::read(info.location().unwrap()))};

        let mut info = PanicUnwindInfo{
            foreign: FOREIGN_EXCEPTION_INIT,
            abi_ver: 0,
            panic_origin: location,
            message: info.message().map(|args| ::alloc::format!("{}",args)),
            vtable: core::ptr::metadata(info.payload()),
            tail_size: 0,
        };


        let layout = alloc::alloc::Layout::of_val(info.payload());
        
        let unwind_info = __allocate_exception(layout);
        
        unsafe{unwind_info.write(info)};
        let except = unsafe{unwind_info.offset(1).align_to(layout.align()).cast::<u8>};
        // The payload may not be `Copy`, but `begin`
        unsafe{core::ptr::copy_nonoverlapping(info.payload() as *const dyn Any as *const u8, except, info.size())};
        
        begin_unwind(unwind_info)
    }
}

