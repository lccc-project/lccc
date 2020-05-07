#![feature(lang_items,intrinsics,no_core,optin_builtin_traits)]
#![feature(rustc_attrs,const_fn,reciever_trait)]
#![feature(lccc_const_transmute,lccc_slice_layout,lccc_const_zeroed)]
#![feature(unsize,negative_impls,no_niche,untagged_unions,prelude_import)]
#![no_core]

#[prelude_import]
pub use prelude::v1::*;


mod bool;
mod unit;

pub mod marker;
pub mod clone;
pub mod cmp;
pub mod cell;
pub mod borrow;
pub mod slice;
pub mod intrinsics;
pub mod ops;
pub mod mem;
pub mod primitive;
pub mod option;
pub mod prelude;