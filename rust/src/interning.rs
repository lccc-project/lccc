use core::borrow::Borrow;
use core::convert::AsRef;
use core::num::NonZeroU32;
use core::ops::Deref;
use core::sync::atomic::{AtomicUsize, Ordering};

use std::sync::RwLock;
use xlang::abi::collection::HashMap;
use xlang::abi::string::StringView;

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Symbol(NonZeroU32);

impl Default for Symbol {
    fn default() -> Self {
        "<unsaved symbol>".into()
    }
}

impl core::cmp::Ord for Symbol {
    fn cmp(&self, rhs: &Self) -> core::cmp::Ordering {
        if self == rhs {
            core::cmp::Ordering::Equal
        } else {
            self.as_str().cmp(rhs.as_str())
        }
    }
}

impl core::cmp::PartialOrd for Symbol {
    fn partial_cmp(&self, rhs: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl core::cmp::PartialOrd<str> for Symbol {
    fn partial_cmp(&self, rhs: &str) -> Option<core::cmp::Ordering> {
        self.as_str().partial_cmp(rhs)
    }
}

impl core::cmp::PartialOrd<Symbol> for str {
    fn partial_cmp(&self, rhs: &Symbol) -> Option<core::cmp::Ordering> {
        self.partial_cmp(rhs.as_str())
    }
}

impl core::cmp::PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl core::cmp::PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl core::cmp::PartialEq<Symbol> for str {
    fn eq(&self, other: &Symbol) -> bool {
        self == other.as_str()
    }
}

impl core::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl core::fmt::Display for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl core::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.as_str().fmt(f)
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(INIT_DYN_VAL as usize);

lazy_static::lazy_static! {
    static ref MAP: RwLock<InitBlob> = {
        let mut map: InitBlob = Default::default();

        init_static_symbols(&mut map);

        RwLock::new(map)
    };
}

// Do not remove
type InitBlob = (
    HashMap<&'static str, NonZeroU32>,
    HashMap<NonZeroU32, &'static str>,
);

// Do not remove
#[allow(dead_code)]
fn insert_static_symbol(map: &mut InitBlob, x: &'static str, key: NonZeroU32) {
    map.0.insert(x, key);
    map.1.insert(key, x);
}

// Form: Each line is either `$id:ident;` or `$id:ident: $tt:tt;` where `$tt` is a lifetime, punctuation, identifier, or literal
interning_static_syms::gen_sym_map! {
    SYNTHETIC: "<synthetic symbol>";
    STD: std;
    SELF_TY: Self;
}

impl From<StringView<'_>> for Symbol {
    fn from(x: StringView) -> Self {
        Self::intern(&x)
    }
}

impl From<&'_ str> for Symbol {
    fn from(x: &str) -> Self {
        Self::intern(x)
    }
}

impl From<String> for Symbol {
    fn from(x: String) -> Self {
        Self::intern_by_val(x)
    }
}

impl From<xlang::abi::string::String> for Symbol {
    fn from(x: xlang::abi::string::String) -> Self {
        Self::intern(&*x)
    }
}

impl<S: AsRef<str>> From<&S> for Symbol {
    fn from(x: &S) -> Self {
        Self::intern(x.as_ref())
    }
}

impl Symbol {
    pub fn intern_by_val(st: String) -> Self {
        let rdgrd = MAP.read().unwrap();
        if let Some(sym) = rdgrd.0.get(&*st).copied() {
            Symbol(sym)
        } else {
            drop(rdgrd);
            let val = COUNTER
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |val| {
                    <usize>::checked_add(val, 1)
                })
                .expect("Overflowed number of symbols")
                .try_into()
                .expect("Overflowed number of symbols");

            let leaked = Box::leak(st.into_boxed_str());

            let mut guard = MAP.write().unwrap();

            let sym = unsafe { NonZeroU32::new_unchecked(val) };
            guard.0.insert(leaked, sym);
            guard.1.insert(sym, leaked);
            drop(guard);
            Symbol(sym)
        }
    }
    pub fn intern(st: &str) -> Self {
        let rdgrd = MAP.read().unwrap();
        if let Some(sym) = rdgrd.0.get(st).copied() {
            Symbol(sym)
        } else {
            drop(rdgrd);
            let val = COUNTER
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |val| {
                    <usize>::checked_add(val, 1)
                })
                .expect("Overflowed number of symbols")
                .try_into()
                .expect("Overflowed number of symbols");
            let leaked = Box::leak(Box::<str>::from(st));
            let mut guard = MAP.write().unwrap();
            let sym = unsafe { NonZeroU32::new_unchecked(val) };
            guard.0.insert(leaked, sym);
            guard.1.insert(sym, leaked);
            drop(guard);
            Symbol(sym)
        }
    }

    pub fn as_str(&self) -> &str {
        MAP.read().unwrap().1[&self.0]
    }
}

impl Deref for Symbol {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Borrow<str> for Symbol {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
