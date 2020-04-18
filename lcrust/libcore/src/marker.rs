
#[lang = "sized"]
pub trait Sized{}

#[lang = "phantom_data"]
pub struct PhantomData<T: ?Sized>{}