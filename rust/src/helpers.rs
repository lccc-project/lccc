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

macro_rules! nzu16 {
    ($val:expr) => {{
        const __VAL: ::core::num::NonZeroU16 = {
            const __LIT: u16 = $val;
            [()][(__LIT == 0) as usize];

            unsafe { ::core::num::NonZeroU16::new_unchecked(__LIT) }
        };
        __VAL
    }};
}

pub(crate) use nzu16;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CyclicOperationStatus {
    Complete,
    Incomplete,
}

impl core::ops::BitAnd for CyclicOperationStatus {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        match (self, rhs) {
            (CyclicOperationStatus::Complete, CyclicOperationStatus::Complete) => {
                CyclicOperationStatus::Complete
            }
            _ => CyclicOperationStatus::Incomplete,
        }
    }
}

impl core::ops::BitAndAssign for CyclicOperationStatus {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
