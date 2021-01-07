use crate::marker::Sized;
use crate::Option;
use crate::Option::Some;

#[lang = "eq"]
pub trait PartialEq<Rhs = Self> {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn eq(&self, other: &Rhs) -> bool;
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

pub enum Ordering {
    Less,
    Equal,
    Greater,
}

#[lang = "partial_ord"]
pub trait PartialOrd<Rhs = Self>: PartialEq<Rhs> {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn lt(&self, other: &Rhs) -> bool {
        if let Some(Ordering::Less) = self.partial_cmp(other) {
            true
        } else {
            false
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn gt(&self, other: &Rhs) -> bool {
        if let Some(Ordering::Less) = self.partial_cmp(other) {
            true
        } else {
            false
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn le(&self, other: &Rhs) -> bool {
        match self.partial_cmp(other) {
            Some(Ordering::Equal) | Some(Ordering::Less) => true,
            _ => false,
        }
    }
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ge(&self, other: &Rhs) -> bool {
        match self.partial_cmp(other) {
            Some(Ordering::Equal) | Some(Ordering::Greater) => true,
            _ => false,
        }
    }
}

pub trait Eq: PartialEq<Self> {}

pub trait Ord: PartialOrd<Self> + Eq {
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn cmp(&self, other: &Rhs) -> Ordering;

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        match self.cmp(&other) {
            Ordering::Less => other,
            _ => self,
        }
    }
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        match self.cmp(&other) {
            Ordering::Greater => other,
            _ => self,
        }
    }

    #[must_use = "Comparison Operations are generally free from side effects"]
    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        if max < min {
            panic!();
        } else if self < min {
            min
        } else if max < self {
            max
        } else {
            self
        }
    }
}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialEq($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro PartialOrd($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro Ord($item:item) {}

#[rustc_builtin_macro]
#[allow_internal_unstable(core_intrinsics)]
pub macro Eq($item:item) {}
