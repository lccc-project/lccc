use crate::clone::Clone;

#[lang = "sized"]
pub trait Sized{}

#[lang = "phantom_data"]
#[derive(Copy,Clone,PartialEq,PartialOrd,Eq,Ord,Debug,Default)]
pub struct PhantomData<T: ?Sized>{}

#[lang = "unpin"]
pub auto trait Unpin{}

#[derive(Copy,Clone,PartialEq,PartialOrd,Eq,Ord,Debug,Default)]
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

#[unstable(feature = "unsize", issue = "27732")]
#[lang = "unsize"]
pub trait Unsize<T: ?Sized>{}