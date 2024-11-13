use core::ops::{Index, IndexMut};

mod sealed {
    pub trait Sealed {
        fn len(&self) -> usize;
    }
}

/// Helper Trait for spliting out a given element of a slice
pub trait SplitOut: Index<usize> + sealed::Sealed {
    /// Splits out the `idx`th element from the slice.
    ///
    /// The triplet order is the slice before `idx`, the element `idx`, and the remaining slice after `idx`
    ///
    /// # Panics
    /// Panics if `idx >= self.len()`
    #[track_caller]
    fn split_out(&self, idx: usize) -> (&Self, &Self::Output, &Self) {
        match self.split_out_checked(idx) {
            Some(ret) => ret,
            None => panic!(
                "Index {idx} is out of bounds for slice of length {}",
                self.len()
            ),
        }
    }
    /// Splits out the `idx`th element from the slice as a mutable reference
    ///
    /// The triplet order is the slice before `idx`, the element `idx`, and the remaining slice after `idx`
    ///
    /// # Panics
    /// Panics if `idx >= self.len()`
    fn split_out_mut(&mut self, idx: usize) -> (&mut Self, &mut Self::Output, &mut Self)
    where
        Self: IndexMut<usize>,
    {
        match self.split_out_mut_checked(idx) {
            // SAFETY:
            // Hecking nll
            // transmute here to launder lifetimes, since I'm too lazy to unpack the tuple, manually launder all 3 elements, and repack it
            // Tuple Layout (and layout in general) doesn't depend on lifetime parameters
            // Lifetime is correct since we immediately return the value in the branch, and it's actually borrowed for the same lifetime, but because the error branch we want to inspect the value,
            // Rustc thinks we're infringing upon the borrow that doesn't exist in the error branch
            Some(ret) => unsafe { core::mem::transmute(ret) },
            None => panic!(
                "Index {idx} is out of bounds for slice of length {}",
                self.len()
            ),
        }
    }

    /// Checked version of [`SplitOut::split_out`]. Returns [`None`] if `idx` is out of bounds
    fn split_out_checked(&self, idx: usize) -> Option<(&Self, &Self::Output, &Self)>;

    /// Checked version of [`SplitOut::split_out_mut`]. Returns [`None`] if `idx` is out of bounds
    fn split_out_mut_checked(
        &mut self,
        idx: usize,
    ) -> Option<(&mut Self, &mut Self::Output, &mut Self)>;
}

impl<T> sealed::Sealed for [T] {
    fn len(&self) -> usize {
        self.len()
    }
}

impl<T> SplitOut for [T] {
    fn split_out_checked(&self, idx: usize) -> Option<(&Self, &Self::Output, &Self)> {
        if idx > self.len() {
            return None;
        }

        let (start, rest) = self.split_at(idx);
        let (mid, rest) = rest.split_first()?;
        Some((start, mid, rest))
    }

    fn split_out_mut_checked(
        &mut self,
        idx: usize,
    ) -> Option<(&mut Self, &mut Self::Output, &mut Self)> {
        if idx > self.len() {
            return None;
        }

        let (start, rest) = self.split_at_mut(idx);
        let (mid, rest) = rest.split_first_mut()?;
        Some((start, mid, rest))
    }
}
