

pub mod bool {
    use crate::ops::Not;
    use crate::marker::Copy;
    use crate::clone::Clone;

    #[lang = "bool"]
    impl bool {}
    impl Copy for bool{}
    impl Clone for bool{
        fn clone(&self) -> Self {
            *self
        }
    }
    impl Not for bool {
        type Output = bool;

        fn not(self) -> Self::Output {
            // Naive implementation.
            // Shouldn't this be magic
            if self { false } else { true }
        }
    }
}

