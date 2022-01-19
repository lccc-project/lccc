/// The first version prelude of `xlang_abi`
pub mod v1 {
    pub use crate::boxed::Box;
    pub use crate::collection::HashMap;
    pub use crate::option::{None, Option, Some};
    pub use crate::pair::Pair;
    pub use crate::string::String;
    pub use crate::traits::{DynBox, DynMut, DynRef};
    pub use crate::vec::Vec;

    pub use crate::const_sv;
    pub use crate::format;
    pub use crate::span;
    pub use crate::vec;
}
