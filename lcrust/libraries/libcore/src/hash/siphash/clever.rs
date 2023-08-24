
#[cfg(target_feature = "vec")]
use super::*;

use crate::arch::clever::*;

#[cfg(target_feature = "vec-ext")]
#[inline(always)]
fn vec_load(x: u64, y: u64) -> __v128{
    unsafe{__vload(x,y)}
}

#[cfg(all(target_feature = "vec", not(target_feature = "vec-ext")))]
#[inline(always)]
fn vec_load(x: u64, y: u64) -> __v128{
    let val = ((y as u128)<<64) | (x as u128);
    unsafe{core::mem::transmute(val)}
}


#[inline(always)]
fn collect_by_place(mut s0: __v128, mut s1: __v128) -> (__v128, __v128){
    __xchg(hi_u64(&mut s0), lo_u64(&mut s1));

    (s0,s1)
}


#[cfg(target_feature = "vec")]
pub struct SipHashState(__v128, __v128);

#[cfg(target_feature = "vec")]
impl SipHashState{
    #[inline]
    pub fn from_keys(k0: u64, k1: u64) -> Self{
        let keys = vec_load(k0,k1)
        let mut s0 =  vec_load(SIPHASH_MAG1,SIPHASH_MAG2);
        let mut s1 = vec_load(SIPHASH_MAG3, SIPHASH_MAG4);

        s0 = unsafe{__vec_binary::<BinaryOp::Xor,8>(s0, keys)};
        s1 = unsafe{__vec_binary::<BinaryOp::Xor,8>(s1, keys)};

        (s0, s1) = collect_by_place(s0, s1);
        Self(s0,s1)
    }

    #[inline]
    pub fn update_before_rounds(&mut self, word: u64){
        *hi_u64(&mut self.1) ^= word;
    }

    #[inline]
    pub fn update_after_rounds(&mut self, word: u64){
        *lo_u64(&mut self.0) ^= word;
    }

    #[inline]
    pub fn update_before_final(&mut self){
        *lo_u64(&mut self.1) ^= 0xff;
    }

    #[inline]
    pub fn finish(&self) -> u64{
        let s0 = self.0;
        let s1 = self.1;

        let s0 = unsafe{__vec_binary::<BinaryOp::Xor,8>(s0,s1)};

        unsafe{__vec_binary::<BinaryOp::Xor,8>(collect, s0)}
    }
}

#[cfg(all(target_feature = "vec", target_feature = "hash-accel"))]
impl SipHashState{
    #[inline]
    pub fn round(&mut self){
        unsafe{__sipround(&mut self,0, &mut self.1)}
    }
}

#[cfg(all(target_feature = "vec", not(target_feature = "hash-accel")))]
impl SipHashState{

    #[inline]
    fn halfround(&mut self, rotate: __v128){
        self.0 = unsafe{__vec_binary::<BinaryOp::Add,8>(self.0,self.1)};
        self.1 = unsafe{__vec_binary::<BinaryOp::Lrot,8>(self.1,rotate)};
        self.1 = unsafe{__vec_binary::<BinaryOp::Xor,8>(self.1,self.0)};
        self.0 = unsafe{__vec_permute::<4, 0x1023>(self.0)};
    }

    #[inline]
    pub fn round(&mut self){

        let rotate1 = vec_load(13,16);
        let rotate2 = vec_load(17,21);

        self.halfround(rotate1);
        self.halfround(rotate2);

    }
}

#[cfg(not(target_feature = "vec"))]
include!("generic.rs");