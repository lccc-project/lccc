/**
 * rust/libcore/alloc.rs
 * This file is part of lcrust standard libraries, a part of the lccc project
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
 *  the lcrust standard libraries are additionally dual licensed under the terms of the MIT and Apache v2 license.
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms.
 */
use core::result::Result::{self, Err, Ok};

#[lang = "global_alloc"]
pub unsafe trait GlobalAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8;
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout);
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let bytes = self.alloc(layout);
        ::__lccc::builtins::C::__builtin_memset(bytes, 0, layout.size());
        bytes
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, mut new_size: usize) -> *mut u8 {
        let align = layout.align();
        new_size += (align - (new_size % align)) % align;
        let n_layout = Layout::from_size_align_unchecked(new_size, align);
        let ret = self.alloc(n_layout);
        if ret.is_null() {
            core::ptr::null_mut()
        } else {
            core::ptr::copy_nonoverlapping(ptr, ret, layout.size().min(new_size));
            self.dealloc(ptr, layout);
            ret
        }
    }

    #[unstable(feature = "lccc_alloc_align_safety")]
    fn alloc_align_compatible(&self, old_align: usize, new_align: usize) -> bool {
        false
    }
}

#[lang = "alloc_layout"]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Layout {
    sz: usize,
    _align: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutErr {
    _priv: (),
}

impl Layout {
    pub const fn from_size_align(sz: usize, _align: usize) -> Result<Self, LayoutErr> {
        if _align == 0 || (_align & (_align - 1)) != 0 {
            Err(LayoutErr { _priv: () })
        } else if (sz % align) != 0 {
            Err(LayoutErr { _priv: () })
        } else {
            Ok(Self { sz, _align })
        }
    }

    pub const unsafe fn from_size_align_unchecked(sz: usize, _align: usize) -> Self {
        Self { sz, _align }
    }

    pub fn size(&self) -> usize {
        self.sz
    }

    pub fn align(&self) -> usize {
        self._align
    }
    pub const fn new<T>() -> Layout {
        Self {
            sz: core::mem::size_of::<T>(),
            _align: core::mem::align_of::<T>(),
        }
    }
    pub fn for_value<T: ?Sized>(val: &T) -> Layout {
        Self {
            sz: core::mem::size_of_val(val),
            _align: core::mem::align_of_val(val),
        }
    }
    #[unstable(feature = "layout_for_ptr")]
    pub unsafe fn for_value_raw<T: ?Sized>(ptr: *const T) -> Layout {
        // Note:
        // The lccc builtin versions of these accept raw pointers, while the definitions in core::mem are not
        Self {
            sz: ::__lccc::builtins::rust::size_of_val(ptr),
            _align: ::__lccc::builtins::rust::align_of_val(ptr),
        }
    }

    pub fn array<T>(len: usize) -> Result<Layout, LayoutError> {
        if let Some(len) = len.checked_multiply(core::mem::size_of::<T>()) {
            Ok(Self {
                sz: len,
                _align: core::mem::align_of::<T>(),
            })
        } else {
            Err(LayoutErr { _priv: () })
        }
    }

    #[unstable(feature = "alloc_layout_extra", issue = "55724")]
    pub fn dangling(&self) -> NonNull<u8> {
        // SAFETY:
        // _align is a power of two, and thus, the resulting pointer is non-null.
        unsafe { NonNull::new_unchecked(core::ptr::without_provenance(isize::MIN as usize)) }
    }
}

#[macro_export]
#[__lccc::builtin_attribute]
pub macro global_allocator($tt:tt) {}
