use std::{
    borrow::Borrow,
    hash::Hash,
    num::NonZeroU32,
    ops::Deref,
    sync::{
        atomic::{self, AtomicUsize},
        RwLock,
    },
};

use xlang::abi::{collection::HashMap, string::String, string::StringView, sync::OnceCell};

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Symbol(NonZeroU32);

type SymHashMap = (
    HashMap<NonZeroU32, &'static str>,
    HashMap<&'static str, NonZeroU32>,
);

struct SymbolMap {
    inner: OnceCell<RwLock<SymHashMap>>,
    counter: AtomicUsize,
}

impl SymbolMap {
    pub const fn new() -> Self {
        Self {
            inner: OnceCell::new(),
            counter: AtomicUsize::new(1),
        }
    }
    fn get_maps(&self) -> &RwLock<SymHashMap> {
        self.inner
            .get_or_insert_with(|| RwLock::new((HashMap::new(), HashMap::new())))
    }

    fn allocate_counter(&self) -> NonZeroU32 {
        let val = self.counter.fetch_add(1, atomic::Ordering::Relaxed);
        NonZeroU32::new(val.try_into().expect("Exhausted Counter Limit"))
            .expect("Exhausted Counter Limit")
    }
}

static SYMBOL_MAP: SymbolMap = SymbolMap::new();

impl Symbol {
    pub fn intern(x: &str) -> Symbol {
        let maps = SYMBOL_MAP.get_maps();

        let mlock = maps.read().unwrap_or_else(|e| e.into_inner());

        if let Some(&val) = mlock.1.get(x) {
            Symbol(val)
        } else {
            drop(mlock);

            let mut mlock = maps.write().unwrap_or_else(|e| e.into_inner());

            if let Some(&val) = mlock.1.get(x) {
                Symbol(val)
            } else {
                let string = String::from(x);

                let leaked = string.leak();

                let val = SYMBOL_MAP.allocate_counter();

                if let Some(_) = mlock.0.insert(val, &*leaked) {
                    panic!("Symbol counter wrapped")
                }

                mlock.1.insert(&*leaked, val);

                Symbol(val)
            }
        }
    }

    pub fn intern_by_value(x: String) -> Symbol {
        let maps = SYMBOL_MAP.get_maps();

        let mlock = maps.read().unwrap_or_else(|e| e.into_inner());

        if let Some(&val) = mlock.1.get(&*x) {
            Symbol(val)
        } else {
            drop(mlock);

            let mut mlock = maps.write().unwrap_or_else(|e| e.into_inner());

            if let Some(&val) = mlock.1.get(&*x) {
                Symbol(val)
            } else {
                let string = x;

                let leaked = string.leak();

                let val = SYMBOL_MAP.allocate_counter();

                if let Some(_) = mlock.0.insert(val, &*leaked) {
                    panic!("Symbol counter wrapped")
                }

                mlock.1.insert(&*leaked, val);

                Symbol(val)
            }
        }
    }

    pub fn as_str(&self) -> &str {
        let maps = SYMBOL_MAP.get_maps();
        let mlock = maps.read().unwrap_or_else(|e| e.into_inner());

        mlock
            .0
            .get(&self.0)
            .copied()
            .expect("Got a `Symbol` with an invalid name")
    }

    pub fn as_view(&self) -> StringView {
        StringView::new(self.as_str())
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for Symbol {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
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

impl<'a> core::cmp::PartialOrd<StringView<'a>> for Symbol {
    fn partial_cmp(&self, rhs: &StringView<'a>) -> Option<core::cmp::Ordering> {
        self.as_str().partial_cmp(rhs.as_str())
    }
}

impl<'a> core::cmp::PartialOrd<Symbol> for StringView<'a> {
    fn partial_cmp(&self, rhs: &Symbol) -> Option<core::cmp::Ordering> {
        self.as_str().partial_cmp(rhs.as_str())
    }
}

impl core::cmp::PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<'a> core::cmp::PartialEq<StringView<'a>> for Symbol {
    fn eq(&self, other: &StringView<'a>) -> bool {
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

impl core::fmt::Display for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(self)
    }
}

impl core::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl<'a, S: ?Sized + AsRef<str>> From<&'a S> for Symbol {
    fn from(value: &'a S) -> Self {
        Self::intern(value.as_ref())
    }
}

impl<'a> From<StringView<'a>> for Symbol {
    fn from(value: StringView<'a>) -> Self {
        Self::intern(&value)
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Self::intern_by_value(value)
    }
}
