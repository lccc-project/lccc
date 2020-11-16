
pub mod v1{
    pub use crate::marker::{Copy,Sync,Send,Sized,Unpin};

    pub use crate::option::*;

    pub use crate::cmp::{Eq,PartialEq,Ord,PartialOrd,Ordering};

    pub use crate::result::*;
}