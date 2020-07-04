#[derive(Copy,Clone,Ord, PartialOrd, Eq, PartialEq,Hash)]
pub enum Option<T>{
    Some(T),
    None
}
use Option::*;
use crate::{PartialEq, Copy};
use crate::ops::{FnOnce, Deref, DerefMut};
use crate::result::Result;
use crate::default::Default;
use crate::clone::Clone;
use crate::result::Result::Ok;
use crate::convert::From;
use crate::iter::{Iterator, IntoIterator};

impl<T> Default for Option<T>{
    fn default() -> Self {
        None
    }
}

impl<T> Option<T>{

    #[must_use = "if you intended to assert that this has a value, consider `.unwrap()` instead"]
    pub fn is_some(&self) -> bool{
        if let Some(_) = self{
            true
        }else{
            false
        }
    }

    #[must_use = "if you intended to assert that this doesn't have a value, consider `.and_then(|| panic!(\"`Option` had a value when expected `None`\"))` instead"]
    pub fn is_none(&self) -> bool{
        if let None = self{
            true
        }else{
            false
        }
    }

    #[must_use]
    #[unstable(feature="option_result_contains")]
    pub fn contains<U>(&self,val: &U) -> bool where U: PartialEq<T>{
        if let Some(t) = self{
            val == t
        }else{
            false
        }
    }

    pub fn as_ref(&self) -> Option<&T>{
        if let Some(t) = self{
            Some(t)
        }else{
            None
        }
    }

    pub fn as_mut(&mut self) -> Option<&mut T>{
        if let Some(t) = self{
            Some(t)
        }else{
            None
        }
    }

    pub fn expect(self,msg: &str) -> T{
        if let Some(t) = self{
            t
        }else{
            panic!(msg)
        }
    }

    pub fn unwrap(self) -> T{
        if let Some(t) = self{
            t
        }else{
            panic!()
        }
    }

    pub fn unwrap_or(self,default: T)->T{
        if let Some(t) = self{
            t
        }else{
            default
        }
    }

    pub fn unwrap_or_else<F: FnOnce()->T>(self,op: F) ->T{
        if let Some(t) = self{
            t
        }else{
            op()
        }
    }

    pub fn map<U,F: FnOnce(T)->U>(self,op: F) -> Option<U>{
        if let Some(t) = self{
            Some(op(t))
        }else{
            None
        }
    }

    pub fn map_or<U,F: FnOnce(T)->U>(self,default: U,op: F) -> U{
        if let Some(t) = self{
            op(t)
        } else{
            default
        }
    }
    pub fn map_or_else<U,D: FnOnce()->U,F: FnOnce(T)->U>(self,default: D,op: F) -> U{
        if let Some(t) = self{
            op(t)
        } else{
            default()
        }
    }

    pub fn ok_or<E>(self,err: E) -> Result<T,E>{
        if let Some(t) = self{
            Result::Ok(t)
        }else{
            Result::Err(err)
        }
    }

    pub fn ok_or_else<E,F: FnOnce()->E>(self,err: F) -> Result<T,E>{
        if let Some(t) = self {
            Result::Ok(t)
        }else {
            Result::Err(err())
        }
    }

    pub fn iter(&self) -> Iter<T>{
        Iter {opt: self.as_ref()}
    }

    pub fn iter_mut(&mut self) -> IterMut<T>{
        IterMut{opt: self.as_mut()}
    }

    pub fn and<U>(self,other: Option<U>) -> Option<U>{
        if let None = self{
            None
        }else{
            other
        }
    }

    pub fn and_then<U,F: FnOnce(T)->Option<U>>(self,op: F) -> Option<U>{
        if let Some(t) = self{
            op(t)
        }else{
            None
        }
    }

    pub fn filter<F: FnOnce(&T)->bool>(self,pred: F) -> Option<T>{
        match self{
            Some(t) if pred(&t) => Some(t),
            _ => None
        }
    }

    pub fn or(self,optb: Option<T>) -> Option<T>{
        match self{
            None => optb,
            _ => self
        }
    }

    pub fn or_else<F: FnOnce()->Option<T>>(self,op: F) -> Option<T>{
        match self{
            None => op(),
            _ => self
        }
    }

    pub fn xor(self,optb: Option<T>) -> Option<T>{
        if let Some(t) = self{
            if let None = optb{
                Some(t)
            }else{
                None
            }
        }else{
            optb
        }
    }

    pub fn get_or_insert(&mut self,v: T) -> &mut T{

    }

    pub fn get_or_insert_with<F: FnOnce()->T>(&mut self,op: F) -> &mut T{
        if let None = self{
            *self = Some(op());
        }
        self.as_mut().unwrap_or_else(|| unsafe { crate::intrinsics::unreachable() })
    }

    pub fn take(&mut self) -> Option<T>{
        crate::mem::take(self)
    }

    pub fn replace(&mut self,val: T) -> Option<T>{
        crate::mem::replace(self,Some(val))
    }

    #[unstable(feature="option_zip",issue="70086")]
    pub fn zip<U>(self,other: Option<U>) -> Option<(T,U)>{
        match (self,other){
            (Some(t),Some(u)) => Some((t,u)),
            _ => None
        }
    }

    #[unstable(feature="option_zip",issue="70086")]
    pub fn zip_with<U,R,F: FnOnce(T,U)->R>(self,other: Option<U>,op: F) -> Option<R>{
        match (self,other){
            (Some(t),Some(u)) => Some(op(t,u)),
            _ => None
        }
    }
}

impl<T: Copy> Option<&'_ T>{
    pub fn copied(self) -> Option<T>{
        if let Some(t) = self{
            Some(*t)
        }else{
            None
        }
    }
}

impl<T: Copy> Option<&'_ mut T>{
    pub fn copied(self) -> Option<T>{
        if let Some(t) = self{
            Some(*t)
        }else{
            None
        }
    }
}

impl<T: Clone> Option<&'_ T>{
    pub fn clone(self) -> Option<T>{
        if let Some(t) = self{
            Some(t.clone())
        }else{
            None
        }
    }
}

impl<T: Default> Option<T>{
    pub fn unwrap_or_default(self) -> T{
        if let Some(t) = self{
            t
        }else{
            Default::default()
        }
    }
}

impl<T: Deref> Option<T>{
    pub fn as_deref(&self) -> Option<&T::Target>{
        if let Some(t) = self{
            Some(&*t)
        }else{
            None
        }
    }
}

impl<T: DerefMut> Option<T>{
    pub fn as_deref_mut(&mut self) -> Option<&mut T::Target>{
        if let Some(t) = self{
            Some(&mut *t)
        }else{
            None
        }
    }
}

impl<T,E> Option<Result<T,E>>{
    pub fn transpose(self) -> Result<Option<T>,E>{
        match self{
            Some(Result::Ok(t)) => Result::Ok(Some(t)),
            Some(Result::Err(e)) => Result::Err(e),
            None => Ok(None)
        }
    }
}

impl<T> Option<Option<T>>{
    pub fn flatten(self) -> Option<T>{
        if let Some(opt) = self{
            opt
        }else{
            None
        }
    }
}

impl<'a,T> From<&'a Option<T>> for Option<&'a T>{
    fn from(r: &'a Option<T>) -> Self {
        r.as_ref()
    }
}

impl<'a,T> From<&'a mut Option<T>> for Option<&'a mut T>{
    fn from(r: &'a mut Option<T>) -> Self {
        r.as_mut()
    }
}

impl<T> From<T> for Option<T>{
    fn from(val: T) -> Self {
        Some(val)
    }
}

struct OptionIter<T>{
    opt: Option<T>
}

pub type Iter<'a,A: 'a> = OptionIter<&'a A>;
pub type IterMut<'a,A: 'a> = OptionIter<&'a mut A>;

impl<T> Iterator for OptionIter<T>{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.opt.take()
    }
}

impl<T> IntoIterator for Option<T>{
    type Item = T;
    type IntoIter = OptionIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        OptionIter{opt: self}
    }
}

impl<'a,T: 'a> IntoIterator for &'a Option<T>{
    type Item = &'a T;
    type IntoIter = OptionIter<&'a T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a,T: 'a> IntoIterator for &'a mut Option<T>{
    type Item = &'a mut T;
    type IntoIter = OptionIter<&'a mut T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}