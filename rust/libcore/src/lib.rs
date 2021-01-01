#![feature(lccc_intrinsic_crate)]
#![deny(__lccc::unprefixed_features)]
#![allow(__lccc::rustc_incomplete_features)]
#![feature(lang_items,intrinsics,no_core,optin_builtin_traits,negative_trait_bounds)]
#![feature(rustc_attrs,const_fn,reciever_trait)]
#![feature(lccc_const_transmute,lccc_slice_layout,lccc_const_zeroed,lccc_slice_layout,lccc_unique_ptr)]
#![feature(unsize,negative_impls,no_niche,untagged_unions,prelude_import)]
#![feature(lccc_borrowck_helpers,lccc_trait_object,lccc_lang_items)]
#![feature(fn_traits,cell_update,unboxed_closures)]
#![feature(lccc_stability_attributes,lccc_same_trait)]
#![feature(or_patterns)]
#![no_core]

extern crate self as core;

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
pub mod ptr;
pub mod alloc;
pub mod raw;
pub mod result;
pub mod convert;
pub mod default;
pub mod iter;
pub mod ffi;
pub mod hint;
pub mod sync;
pub mod pin;

#[__lccc::builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro panic($($input:tt),*){}