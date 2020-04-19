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

#[unstable(feature = "reciever_trait", issue = "none")]
#[lang = "receiver"]
#[doc(hidden)]
pub unsafe trait Reciever{}
// Huh

unsafe impl<T: ?Sized> Reciever for &'_ T{}
unsafe impl<T: ?Sized> Reciever for &'_ mut T{}
