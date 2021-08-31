use std::mem::MaybeUninit;

// Hides the Niches of T for ABI purposes
#[repr(transparent)]
pub struct HideNiches<T>(MaybeUninit<T>);

impl<T> HideNiches<T> {
    pub const fn new(x: T) -> Self {
        Self(MaybeUninit::new(x))
    }

    pub fn into_inner(self) -> T {
        unsafe { self.0.assume_init() }
    }
}
