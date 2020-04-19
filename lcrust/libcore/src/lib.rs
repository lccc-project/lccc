#![feature(lang_items,intrinsics,no_core,optin_builtin_traits)]
#![feature(rustc_attrs,const_fn,reciever_trait)]
#![no_core]



pub mod marker;
pub mod clone;
pub mod cmp;
pub mod cell;
pub mod borrow;
pub mod slice;
pub mod intrinsics;
pub mod ops;
pub mod mem;
mod primitive;
pub use primitive::bool;