#![feature(lang_items,intrinsics,no_core,optin_builtin_traits)]
#![feature(rustc_attrs)]
#![no_core]

use crate::marker::Sized;


#[cfg_attr(feature="enable_stability_attributes",unstable(feature = "reciever_trait", issue = "none"))]
#[lang = "receiver"]
#[doc(hidden)]
pub unsafe trait Reciever{}
// Huh

unsafe impl<T: ?Sized> Reciever for &'_ T{}
unsafe impl<T: ?Sized> Reciever for &'_ mut T{}

pub mod marker;
pub mod clone;
pub mod cmp;
pub mod cell;
pub mod borrow;
pub mod slice;
pub mod intrinsics;
pub mod ops;
pub mod primitive;