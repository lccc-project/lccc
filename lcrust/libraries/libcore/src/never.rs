use crate::hash::{Hash, Hasher};

use crate::clone::Clone;

#[lang = "never"]
#[__lccc::mangle_as("std::never::__never")]
impl ! {}

impl Clone for ! {
    fn clone(&self) -> Self {
        match self {}
    }
}

impl Copy for ! {}

impl Hash for ! {
    fn hash<H: Hasher>(&self, h: &mut H) {
        match self {}
    }
}

impl PartialEq for ! {
    fn eq(&self, rhs: &Self) -> bool {
        match self {}
    }
}

impl PartialOrd for ! {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match self {}
    }
}

impl Ord for ! {
    fn cmp(&self, rhs: &Self) -> Ordering {
        match self {}
    }
}

impl Eq for ! {}
