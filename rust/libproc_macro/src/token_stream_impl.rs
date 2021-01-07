use alloc::alloc::Allocator;

#[derive(Copy, Clone)]
pub struct CXXAllocator;

struct NoThrowT {
    _padding: ::core::mem::MaybeUninit<u8>,
}

const no_throw: NoThrowT = NoThrow {
    _padding: MaybeUninit::uninit(),
};

extern "C" {

    pub fn _ZnwPvmRCSt6no_throw_t(sz: usize, nothrow: &NoThrowT) -> *mut core::ffi::c_void;
    pub fn _ZnwPvmmRCSt6no_throw_t(
        sz: usize,
        align: usize,
        nothrow: &NoThrowT,
    ) -> *mut core::ffi::c_void;
    pub fn _ZdlvPv(p: *mut core::ffi::c_void);
    pub fn _ZdlvPvm(p: *mut core::ffi::c_void, align: usize);
}

impl Allocator for CXXAllocator {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        if layout.size() == 0 {
            Ok(layout.dangling().cast::<[u8; 0]>())
        } else if layout.align() <= core::mem::align_of::<::__lccc::platform::max_align_t>() {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { _ZnwPvmRCSt6no_throw_t(layout.size(), &no_throw) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        } else {
            // SAFETY:
            // The C++ Standard Library prescribes no undefined behaviour for any global replaceable operator new
            // This is nothrow, so no exceptions are propagated through it.
            // The Required Behaviour is not exempt from the C++11 concurrency requirements, so &no_throw is not modified
            let ptr = unsafe { _ZnwPvmmRCSt6no_throw_t(layout.size(), layout.align(), &no_throw) };
            if ptr.is_null() {
                Err(AllocError)
            } else {
                Ok(unsafe {
                    NonNull::new_unchecked(core::ptr::slice_from_raw_parts(
                        ptr as *mut u8,
                        layout.size(),
                    ))
                })
            }
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        if layout.size() == 0 {
        } else if layout.align() <= core::mem::align_of::<::__lccc::platform::max_align_t>() {
            // SAFETY:
            // The pointer was guaranteed to be returned from an allocate call on this allocator with the same layout
            // Thus, we know that the pointer was returned from the three-arg form of the global replaceable, nonthrowing, operator new.
            unsafe { _ZdlvPvm(ptr.as_ptr() as *mut ::core::ffi::c_void, layout.align()) }
        } else {
            // SAFETY:
            // The pointer was guaranteed to be returned from an allocate call on this allocator with the same layout
            // Thus, we know that the pointer was returned from the two-arg form of the global replaceable, nonthrowing, operator new
            unsafe { _ZdlvPv(ptr.as_ptr() as *mut ::core::ffi::c_void) }
        }
    }
}

#[repr(C)]
pub struct TokenStream {
    // Note:
    // This relies on private implementation details of lccc,
    // In particular, slice layout,
    // To allow TokenStream to be moved between two threads.
    ptr: Unique<[MaybeUninit<TokenTree>]>,
    len: usize,
    allocator: CXXAllocator,
}

impl !Send for TokenStream {}
impl !Sync for TokenStream {}

impl Clone for TokenStream {
    fn clone(&self) -> Self {
        let alloc = self.allocator;
        let ptr = alloc
            .allocate(Layout::array::<TokenTree>(self.ptr.as_mut_ptr().len()).unwrap())
            .unwrap();
        let ptr = ptr.as_ptr().as_mut_ptr() as *mut TokenTree;
        for i in 0..self.len {
            // ~~All of this is sound because I said so.~~
            // SAFETY:
            // self.ptr.as_mut_ptr().len() is the capacity of the vector, and len<=capacity
            // By that definition, both of these accesses are inbounds
            // self.ptr is not dangling, as the capacity is nonzero (because the length is nonzero)
            unsafe {
                ptr.offset(i)
                    .write(*(self.ptr.as_mut_ptr().get_unchecked(i) as *const TokenTree))
            }
        }

        Self {
            // SAFETY:
            // ptr is from alloc.allocate(), so it is known to be unique, and well-aligned (and also non-null)
            ptr: unsafe {
                Unique::new_unchecked(core::ptr::slice_from_raw_parts_mut(
                    ptr as *mut MaybeUninit<TokenTree>,
                    self.ptr.as_mut_ptr().len(),
                ))
            },
            len: self.len,
            alloc,
        }
    }
}

#[derive(Clone)]
#[repr(u8)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

#[repr(C)]
pub struct Group {
    ts: TokenStream,
    delim: Delimiter,
}

#[repr(u8)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    None,
}
