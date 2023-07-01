pub trait FetchIncrement {
    fn fetch_increment(&mut self) -> Self;
}

macro_rules! impl_for_primitives{
    ($($ty:ty),* $(,)?) => {
        $(
            impl FetchIncrement for $ty{
                fn fetch_increment(&mut self) -> Self{
                    let val = *self;
                    *self += 1;
                    val
                }
            }
        )*
    }
}

impl_for_primitives!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

#[derive(Copy, Clone)]
pub struct TabPrinter(u32);

impl TabPrinter {
    pub const fn new() -> Self {
        TabPrinter(0)
    }

    pub const fn nest(&self) -> Self {
        TabPrinter(self.0 + 1)
    }
}

impl core::fmt::Display for TabPrinter {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        for _ in 0..self.0 {
            f.write_str("  ")?;
        }
        Ok(())
    }
}
