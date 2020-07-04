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

#[lang = "drop"]
pub trait Drop{
    fn drop(&mut self);
}

#[lang = "fn_once"]
#[unstable(feature="fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait FnOnce<Args>{
    type Output;
    extern"rust-call" fn call_once(self,args: Args)->Self::Output;
}

#[lang = "fn_mut"]
#[unstable(feature="fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait FnMut<Args>: FnOnce<Args>{
    extern"rust-call" fn call_mut(&mut self,args: Args)->Self::Output;
}

#[lang = "fn"]
#[unstable(feature="fn_traits")]
#[must_use = "Closures are lazy and do nothing unless called"]
pub trait Fn<Args>: FnMut<Args>{
    extern"rust-call" fn call(&self,args: Args)->Self::Output;
}

impl<A,F: ?Sized> FnOnce<A> for &'_ mut F where F: FnMut<A>{
    type Output = F::Output;

    fn call_once(self, args: A) -> Self::Output {
        <F as FnMut<A>>::call_mut(self,args)
    }
}

impl<A,F: ?Sized> FnOnce<A> for &'_ F where F: Fn<A>{
    type Output = F::Output;

    fn call_once(self, args: A) -> Self::Output {
        <F as Fn<A>>::call(self,args)
    }
}

impl<A,F: ?Sized> FnMut<A> for &'_ mut F where F: FnMut<A>{
    fn call_mut(&mut self, args: A) -> Self::Output {
        <F as FnMut<A>>::call_mut(*self,args)
    }
}

impl<A,F: ?Sized> FnMut<A> for &'_ F where F: Fn<A>{
    fn call_mut(&mut self, args: A) -> Self::Output {
        <F as Fn<A>>::call(*self,args)
    }
}

impl<A,F: ?Sized> Fn<A> for &'_ F where F: Fn<A>{
    fn call(&self, args: A) -> Self::Output {
        <F as Fn<A>>::call(*self,args)
    }
}