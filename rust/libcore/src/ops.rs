use crate::marker::Sized;

#[lang = "not"]
pub trait Not{
    type Output;
    fn not(self)->Self::Output;
}

#[lang = "deref"]
pub trait Deref{
    type Target;
    fn deref(&self) -> &Self::Target;
}

impl<T> Deref for &'_ T{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        *self
    }
}
impl<T> Deref for &'_ mut T{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        *self
    }
}

#[lang = "deref_mut"]
pub trait DerefMut : Deref{
    fn deref_mut(&mut self) -> &mut Self::Target;
}

impl<T> DerefMut for &'_ mut T{
    fn deref_mut(&mut self) -> &mut Self::Target {
        *self
    }
}

#[lang = "add"]
pub trait Add<Rhs=Self>{
    type Output;
    fn add(self,rhs: Rhs) -> Self::Output;
}

#[lang = "sub"]
pub trait Sub<Rhs=Self>{
    type Output;
    fn sub(self,rhs: Rhs) -> Self::Output;
}

#[lang = "mul"]
pub trait Mul<Rhs=Self>{
    type Output;
    fn mul(self,rhs: Rhs) -> Self::Output;
}

#[lang = "div"]
pub trait Div<Rhs=Self>{
    type Output;
    fn div(self,rhs: Rhs) -> Self::Output;
}

#[unstable(feature = "reciever_trait", issue = "none")]
#[lang = "receiver"]
#[doc(hidden)]
pub unsafe trait Reciever{}
// Huh

unsafe impl<T: ?Sized> Reciever for &'_ T{}
unsafe impl<T: ?Sized> Reciever for &'_ mut T{}

#[unstable(feature = "unsize", issue = "27732")]
#[lang = "unsize"]
pub trait Unsize<T: ?Sized>{}

#[unstable(feature = "unsize", issue="277732")]
#[lang = "coerce_unsized"]
pub trait CoereceUnsized<T: ?Sized>{}


#[cfg(feature="lccc_lang_items")]
#[doc(hidden)]
#[unstable(feature="lccc_lang_items",issue="none",reason="This is an internal API of lcrust libcore, it is not guaranteed to be portable")]
#[lang = "unwrapping_deref"]
pub trait DerefMove: DerefMut<Target: Sized> + Sized{
   fn deref_move(self) -> Self::Target;
}