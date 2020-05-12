use crate::clone::Clone;
use crate::cell::UnsafeCell;

#[lang = "sized"]
pub trait Sized{}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>{}

#[lang = "unpin"]
pub auto trait Unpin{}

pub struct PhantomPinned{}

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


