use crate::option::*;

#[lang = "bool"]
impl bool {
    #[unstable(feature = "bool_to_option", issue = "64260")]
    #[inline]
    pub fn then_some<T>(self, t: T) -> Option<T> {
        if self {
            Some(t)
        } else {
            None
        }
    }

    #[unstable(feature = "bool_to_option", issue = "64260")]
    #[inline]
    pub fn then<T, F: FnOnce() -> T>(self, f: F) -> Option<T> {
        if self {
            Some(f())
        } else {
            None
        }
    }
}
