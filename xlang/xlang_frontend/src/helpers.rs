pub trait FetchIncrement {
    fn fetch_increment(&mut self) -> Self;
}

pub trait FetchDecrement {
    fn fetch_decrement(&mut self) -> Self;
}

macro_rules! impl_for_primitives {
    ($($ty:ty),* $(,)?) => {
        $(
            impl FetchIncrement for $ty {
                fn fetch_increment(&mut self) -> Self {
                    let val = *self;
                    *self += 1;
                    val
                }
            }
            impl FetchDecrement for $ty{
                fn fetch_decrement(&mut self) -> Self{
                    let val = *self;
                    *self -= 1;
                    val
                }
            }
        )*
    }
}

impl_for_primitives!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
