/**
 * rust/libproc_macro/token_stream_impl.rs
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
