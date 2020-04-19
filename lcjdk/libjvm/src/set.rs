use core::ops::{BitAnd, BitOr, BitXor, BitAndAssign, BitOrAssign, Not, Div, Sub, SubAssign, DivAssign};
use core::borrow::Borrow;
use core::mem;
use core::intrinsics::assume;

const fn ceil_div(lhs: u16, rhs: u16) -> u16 {
    ((lhs/rhs)+(if lhs%rhs!=0{1}else{0}))
}

///
/// Unsafe Trait for implementing Enums that can be used with EnumSet
/// An implementer of UnitEnum asserts at least one of the following:
/// * The enum is uninhabited, IE. has no variants
/// * Each variant of the enum is a unit variant and can be associated by an opaque injection with a u16 value which is less than HIGHEST_VAL
///
/// Additionally, an implementer must ensure that (E::HIGHEST_VAL/16)+1 is at most 32. This is a limitation of rust's type system, and will likely be removed when const generics are stabalized.
///
/// For Uninhabited enumerations, it is recommended (though not required or assumed) that HIGHEST_VAL is 0
///  (as this will allow the enum set to be optimized away to a ZST)
pub unsafe trait UnitEnum : Copy {
    ///
    /// Returns the 
    fn discriminant(&self) -> u16;
    const HIGHEST_VAL: u16;
}


/// O(1) time, O(n) space efficient Set storing Enumeration values that can be represented as a valid u16 value
///
/// EnumSet shall be valid to initialize with mem::zeroed().
/// This set shall be equivalent to the one produced by EnumSet::none()
#[derive(Copy,Clone)]
pub struct EnumSet<E: UnitEnum>
    where [u16; ceil_div(E::HIGHEST_VAL,16) as usize]: Borrow<[u16]> + Default{
    raw: [u16;ceil_div(E::HIGHEST_VAL,16) as usize]
}


impl<E: UnitEnum> EnumSet<E>{

    ///
    /// Returns the Enum Set containing all values of type E.
    pub const fn all() -> Self{
        Self {raw: [!0;ceil_div(E::HIGHEST_VAL,16) as usize]}
    }

    ///
    /// Returns the set containing no values of E
    pub const fn none() -> Self{
        unsafe{mem::zeroed()}
    }

    ///
    /// Returns the set containing val, and no other values of E
    pub const fn of(val: E) -> Self{
        let raw = [0;ceil_div(E::HIGHEST_VAL,16) as usize];
        let val = val.discriminant();
        unsafe{core::intrinsics::assume(val<E::HIGHEST_VAL)}
        let word = val>>4;
        let bit = val&15;
        raw[word] = 1<<bit;
        Self{raw}
    }

    /// Returns the value of the predicate P(x) = v belongs to self
    pub const fn contains(&self,v: E) -> bool{
        let val = v.discriminant();
        unsafe{core::intrinsics::assume(val<E::HIGHEST_VAL)}
        let word = val>>4;
        let bit = val&15;
        self.raw[word] & (1<<bit) != 0
    }

    /// Drops v from this set (modifying the original set)
    pub fn drop(&mut self,v: E){
        let val = v.discriminant();
        unsafe{core::intrinsics::assume(val<E::HIGHEST_VAL)}
        let word = val>>4;
        let bit = val&15;
        self.raw[word] &= !(1<<bit)
    }

    /// Adds v to this set (modifying the original set)
    pub fn gain(&mut self,v: E){
        let val = v.discriminant();
        unsafe{core::intrinsics::assume(val<E::HIGHEST_VAL)}
        let word = val>>4;
        let bit = val&15;
        self.raw[word] |= !(1<<bit)
    }

}

///
/// Implements the intersection operator for two enum sets.
/// The result of S&R shall be the set T, such that for all x, x belongs to T iff x belongs to S and x belongs to R
impl<E: UnitEnum> BitAnd for EnumSet<E>{
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= rhs.raw[i]
        }
        self
    }
}

///
/// Implements the union operator for two enum sets.
/// The result of S|R shall be the set T, such that for all x, x belongs to T iff x belongs to S or x belongs to R
impl<E: UnitEnum> BitOr for EnumSet<E>{
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] |= rhs.raw[i]
        }
        self
    }
}

///
/// Implements the Set Difference/Disjunction Operator
impl<E: UnitEnum> Div for EnumSet<E>{
    type Output = Self;

    fn div(mut self, rhs: Self) -> Self::Output {
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= !rhs.raw[i]
        }
        self
    }
}

impl<E: UnitEnum> Sub for EnumSet<E>{
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= !rhs.raw[i]
        }
        self
    }
}

impl<E: UnitEnum> DivAssign for EnumSet<E>{

    fn div_assign(&mut self, rhs: Self){
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= !rhs.raw[i]
        }
    }
}

impl<E: UnitEnum> SubAssign for EnumSet<E>{
    fn sub_assign(&mut self, rhs: Self) {
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= !rhs.raw[i]
        }
    }
}

///
/// Implements the "Retain" function for a modifiable set, and a source set.
/// R &= S shall retain all values in R that belong to S
impl<E: UnitEnum> BitAndAssign for EnumSet<E>{
    fn bitand_assign(&mut self, rhs: Self){
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] &= rhs.raw[i]
        }
    }
}


///
/// Implements the "Add All" function for a modifiable set, and a source set
/// R |= S shall include all values that previously belong to R, and those values which belong to S
impl<E: UnitEnum> BitOrAssign for EnumSet<E>{
    fn bitor_assign(&mut self, rhs: Self){
        for i in 0..ceil_div(E::HIGHEST_VAL,16){
            self.raw[i] |= rhs.raw[i]
        }
    }
}

///
/// Implements the compliment operator for a set.
///
/// !S shall be the set T where for all x, x belongs to T iff x does not belong to S.
/// In particular, !EnumSet::of(v) shall contain all values
impl<E: UnitEnum> Not for EnumSet<E>{
    type Output = Self;
    fn not(mut self) -> Self{
        for v in self.raw.iter_mut(){
            *v = !*v;
        }
        self
    }
}

#[macro_export]
macro_rules! unit_enum{
    {$(#[$attr:meta])* $($vis:vis)? unit enum $t:ident{
        $(#[$meta:meta])* $($variants:ident $(= $val:literal)?),*
        }
    } => {
            $(#[$attr:meta])*
            #[repr(u16)]
            #[derive(Copy,Clone,PartialEq,Eq)]
            pub enum $t{
                $($variants:ident $(= $val:literal)?),*
            }

            impl $t{
                const fn __highest_val() -> u16{
                    #[doc(hide)]
                    #[repr(u16)]
                    enum __hidden{
                        $($variants:ident $(= $val:literal)?),*
                        #[doc(hide)]
                        __Last
                    }
                    return __hidden::__Last as u16
                }
            }

            unsafe impl UnitEnum for $t{
                const HIGHEST_VAL: u16 = Self::__highest_val();
                fn as_value(&self) => u16{
                    self as u16
                }
            }
        }
}