use crate::Sized;
use crate::mem::MaybeUninit;

#[lang = "drop_in_place"]
pub unsafe fn drop_in_place<T: ?Sized>(to_drop: *mut T){
    drop_in_place(to_drop)
}