
#[lang= "eq"]
pub trait PartialEq<Rhs=Self>{
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn eq(&self,other: &Rhs) -> bool;
    #[must_use = "Comparison Operations are generally free from side effects"]
    fn ne(&self,other: &Rhs) -> bool{
        !self.eq(other)
    }
}

#[lang= "partial_ord"]
pub trait PartialOrd<Rhs=Self> : PartialEq<Rhs>{

}