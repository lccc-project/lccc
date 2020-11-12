use crate::clone::Clone;
use crate::cell::UnsafeCell;
use crate::mem::ManuallyDrop;
use crate::ops::Drop;

#[lang = "sized"]
pub trait Sized{}

#[lang = "phantom_data"] // Still needed for variance and auto trait propagation
pub struct PhantomData<#[__lccc::no_unused_type_param] T: ?Sized>;

#[lang = "unpin"]
pub auto trait Unpin{}

pub struct PhantomPinned;

impl !Unpin for PhantomPinned{}

pub unsafe auto trait Send{}
impl<T: ?Sized> !Send for *const T{}
impl<T: ?Sized> !Send for *mut T{}
unsafe impl<T: ?Sized + Send> Send for &'_ mut T{}
unsafe impl<T: ?Sized + Sync> Send for &'_ T{}

pub unsafe auto trait Sync{}
impl<T: ?Sized> !Sync for *const T{}
impl<T: ?Sized> !Sync for *mut T{}
unsafe impl<T: ?Sized + Sync> Sync for &'_ mut T{}
unsafe impl<T: ?Sized + Sync> Sync for &'_ T{}

#[lang = "copy"]
pub trait Copy: Clone{}

#[unstable(feature = "structural_match", issue = "31434")]
#[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(Eq)]`")]
#[lang = "structural_teq"]
pub trait StructuralEq { }

#[unstable(feature = "structural_match", issue = "31434")]
#[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(PartialEq)]`")]
#[lang = "structural_peq"]
pub trait StructuralPartialEq{}


#[lang = "freeze"]
pub(crate) unsafe auto trait Freeze {}

impl<T: ?Sized> !Freeze for UnsafeCell<T> {}
unsafe impl<T: ?Sized> Freeze for PhantomData<T> {}
unsafe impl<T: ?Sized> Freeze for *const T {}
unsafe impl<T: ?Sized> Freeze for *mut T {}
unsafe impl<T: ?Sized> Freeze for &T {}
unsafe impl<T: ?Sized> Freeze for &mut T {}


#[lang = "trivial_destroy"]
#[doc(hidden)]
#[unstable(feature = "lccc_borrowck_helpers")]
#[__lccc::builtin_trait]
pub unsafe auto trait TrivialDestruction{}

impl<T: ?Sized + Drop> !TrivialDestruction for T{}
impl<T: ?Sized + TraitObject> !TrivialDestruction for T{}

impl<T: ?Sized + !TrivialDestruction> !TrivialDestruction for PhantomData<T>{}

unsafe impl<'a,T: ?Sized> TrivialDestruction for &'a T{}
unsafe impl<'a,T: ?Sized> TrivialDestruction for &'a mut T{}
unsafe impl<T: ?Sized> TrivialDestruction for *const T{}
unsafe impl<T: ?Sized> TrivialDestruction for *mut T{}

#[lang = "non_owning"]
#[doc(hidden)]
#[unstable(feature = "lccc_borrowck_helpers")]
#[__lccc::builtin_trait]
pub unsafe auto trait NonOwning<T: ?Sized>{}

impl<T: ?Sized> !NonOwning<T> for T{}

impl<T: ?Sized> !NonOwning<T> for PhantomData<T>{}

#[lang="same"]
#[doc(hidden)]
#[unstable(feature="lccc_same_trait")]
#[__lccc::builtin_trait]
pub trait Same<T: ?Sized>{}

impl<T: ?Sized> Same<T> for T{}

unsafe impl<'a,T: ?Sized,U: ?Sized> NonOwning<T> for &'a U where &'a U: !Same<T>{}
unsafe impl<'a,T: ?Sized,U: ?Sized> NonOwning<T> for &'a mut U where &'a mut U: !Same<T>{}
unsafe impl<T: ?Sized,U: ?Sized> NonOwning<T> for *mut U  where *mut U: !Same<T>{}
unsafe impl<T: ?Sized,U: ?Sized> NonOwning<T> for *const U where *const U: !Same<T>{}



#[lang = "trait_object_marker"]
#[doc(hidden)]
#[unstable(feature = "lccc_trait_object")]
#[__lccc::builtin_trait]
pub unsafe trait TraitObject{}
