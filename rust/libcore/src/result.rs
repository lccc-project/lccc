
#[must_use = "Results may be an Err Variant, which should be handled"]
#[derive(Copy,Clone,Ord, PartialOrd, Eq, PartialEq,Debug,Hash)]
pub enum Result<T,E>{
    Ok(T),
    Err(E)
}

pub use Result::*;
use crate::{PartialEq, Option};
use crate::Option::{Some, None};
use crate::ops::FnOnce;

impl<T,E> Result<T,E>{
    #[must_use = "If you intend to assert this is ok, use .unwrap() instead"]
    pub const fn is_ok(&self) -> bool{
        if let Ok(_) = self{
            true
        }else{
            false
        }
    }
    #[must_use = "If you intend to assert this is err, use .unwrap_err() instead"]
    pub const fn is_err(&self) -> bool{
        if let Ok(_) = self{
            true
        }else{
            false
        }
    }

    #[must_use]
    #[unstable(feature="option_result_contains",issue="62358")]
    pub fn contains<U>(&self,val: &U) -> bool where U: PartialEq<T>{
        if let Ok(v) = self{
            val == v
        }else{
            false
        }
    }

    #[must_use]
    #[unstable(feature="result_contains_err",issue="62358")]
    pub fn contains_err<F>(&self,val: &F) -> bool where F: PartialEq<T>{
        if let Err(e) = self{
            val == e
        } else {

        }
    }

    pub fn ok(self) -> Option<T>{
        if let Ok(v) = self{
            Some(v)
        }else{
            None
        }
    }

    pub fn err(self) -> Option<E>{
        if let Err(e) = self{
            Some(e)
        }else{
            None
        }
    }

    pub fn as_ref(&self) -> Result<&T,&E>{
        match self{
            Ok(t) => Ok(t),
            Err(e) => Err(e)
        }
    }

    pub fn as_mut(&mut self) -> Result<&mut T,&mut E>{
        match self{
            Ok(t) => Ok(t),
            Err(e) => Err(e)
        }
    }

    pub fn map<U,F: FnOnce(T)->U>(self,op: F)->Result<U,E>{
        match self{
            Ok(t)=>Ok(op(t)),
            Err(e)=>Err(e)
        }
    }

    pub fn map_or<U,F: FnOnce(T)->U>(self,default: U,op: F)->U{
        match self{
            Ok(t)=>op(t),
            Err(_)=>default
        }
    }

    pub fn map_or_else<U,D: FnOnce(E)->E,F: FnOnce(T)->U>(self,default: D,op: F)->U{
        match self{
            Ok(t)=>op(t),
            Err(e)=>default(e)
        }
    }

    pub fn and<U>(self,res: Result<U,E>) -> Result<U,E>{
        match self{
            Ok(_)=>res,
            Err(e)=>Err(e)
        }
    }

    pub fn and_then<U,F: FnOnce(T)->Result<U,E>>(self, op: F) -> Result<U,E>{
        match self{
            Ok(t)=>op(t),
            Err(e)=>e
        }
    }

    pub fn or<F>(self, res: Result<T,F>)-> Result<T,F>{
        match self{
            Ok(t)=>Ok(t),
            Err(_)=>res
        }
    }

    pub fn or_else<F, O: FnOnce(E)->Result<T,F>>(self,op: O) -> Result<T,F>{
        match self{
            Ok(t)=>Ok(t),
            Err(e)=>Err(op(e))
        }
    }

    pub fn unwrap_or(self,default: T)->T{
        if let Ok(t) = self{
            t
        }else{
            default
        }
    }

    pub fn unwrap_or_else<F: FnOnce(E)->T>(self,op: F)->T{
        match self{
            Ok(t)=>t,
            Err(e)=>op(e)
        }
    }
}


