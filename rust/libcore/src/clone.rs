use crate::marker::Sized;

#[lang = "clone"] // Does this even need to be a lang item?
// Ohhhh. Of course, Closures are Clone, as are fn items.
pub trait Clone: Sized{
    #[must_use = "cloning is an expensive operation, and should not have side effects"]
    fn clone(&self) -> Self;

    fn clone_from(&mut self,other: &Self){
        *self = other.clone()
    }
}