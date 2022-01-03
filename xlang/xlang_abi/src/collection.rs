use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::{BuildHasher, Hash, Hasher};
use std::mem::MaybeUninit;

use crate::pair::Pair;

use crate::alloc::{Allocator, Layout, XLangAlloc};
use crate::hash::{BuildHasherDefault, XLangHasher};
use crate::ptr::Unique;

#[repr(C)]
struct HashMapSlot<K, V> {
    ecount: usize,
    entries: [MaybeUninit<Pair<K, V>>; 16],
}

/// An ABI Safe [`HashMap`] that uses an Allocator to obtain the memory to store the slots
#[repr(C)]
pub struct HashMap<
    K,
    V,
    H: BuildHasher = BuildHasherDefault<XLangHasher>,
    A: Allocator = XLangAlloc,
> {
    htab: Unique<HashMapSlot<K, V>>,
    buckets: usize,
    hash: H,
    alloc: A,
}

impl<K, V, H: BuildHasher, A: Allocator> Drop for HashMap<K, V, H, A> {
    fn drop(&mut self) {
        for i in 0..self.buckets {
            let bucket = unsafe { &mut *self.htab.as_ptr().add(i) };
            for i in 0..bucket.ecount {
                unsafe { core::ptr::drop_in_place(bucket.entries[i].as_mut_ptr()) }
            }
        }

        if self.buckets != 0 {
            unsafe {
                self.alloc.deallocate(
                    self.htab.as_nonnull().cast(),
                    Layout::array::<HashMapSlot<K, V>>(self.buckets).unwrap(),
                );
            }
        }
    }
}

impl<K, V, H: BuildHasher + Default, A: Allocator + Default> HashMap<K, V, H, A> {
    ///
    /// Returns a new [`HashMap`] with defaults for both the hasher and the allocator, containing no entries
    #[must_use]
    pub fn new() -> Self {
        Self {
            htab: Unique::dangling(),
            buckets: 0,
            hash: Default::default(),
            alloc: Default::default(),
        }
    }
}

impl<K, V, H: BuildHasher + Default, A: Allocator + Default> Default for HashMap<K, V, H, A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V, H: BuildHasher + Default, A: Allocator> HashMap<K, V, H, A> {
    /// Returns a new [`HashMap`] with the given allocator and a default hasher, containing no entries.
    pub fn new_in(alloc: A) -> Self {
        Self {
            htab: Unique::dangling(),
            buckets: 0,
            hash: Default::default(),
            alloc,
        }
    }
}

impl<K, V, H: BuildHasher, A: Allocator + Default> HashMap<K, V, H, A> {
    /// Returns a new [`HashMap`] with the given hasher and a default alocator, containing no entries.
    pub fn with_hasher(hash: H) -> Self {
        Self {
            htab: Unique::dangling(),
            buckets: 0,
            hash,
            alloc: Default::default(),
        }
    }
}

impl<K, V, H: BuildHasher, A: Allocator> HashMap<K, V, H, A> {
    /// Returns a new [`HashMap`] with the given hasher and allocator, containing no entries
    pub fn with_hasher_in(hash: H, alloc: A) -> Self {
        Self {
            htab: Unique::dangling(),
            buckets: 0,
            hash,
            alloc,
        }
    }

    /// Produces an [`Iterator`] over pairs of keys and values in this [`HashMap`]
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter(
            unsafe { core::slice::from_raw_parts(self.htab.as_ptr(), self.buckets) }.iter(),
            None,
        )
    }

    /// Produces an [`Iterator`] over the values in this [`HashMap`]
    pub fn values(&self) -> Values<'_, K, V> {
        Values(self.iter())
    }
}

impl<K: Eq + Hash, V, H: BuildHasher, A: Allocator> HashMap<K, V, H, A> {
    #[allow(clippy::similar_names)]
    fn rehash(&mut self) {
        let ocount = self.buckets;
        let ncount = self.buckets * 2;
        assert!(
            ncount <= (std::isize::MAX as usize),
            "We have too many buckets, bail before we maybe cause UB"
        );

        let ptr = self
            .alloc
            .allocate_zeroed(Layout::array::<HashMapSlot<K, V>>(ncount).unwrap())
            .unwrap_or_else(|| {
                crate::alloc::handle_alloc_error(Layout::array::<HashMapSlot<K, V>>(16).unwrap())
            });
        let ptr = core::mem::replace(&mut self.htab, unsafe {
            Unique::new_nonnull_unchecked(ptr.cast())
        });
        self.buckets = ncount;

        for i in 0..self.buckets {
            let bucket = unsafe { &mut *ptr.as_ptr().add(i) };
            for i in 0..bucket.ecount {
                let Pair(key, value) = unsafe { bucket.entries[i].as_ptr().read() };
                self.insert(key, value).unwrap();
            }
        }
        unsafe {
            self.alloc.deallocate(
                ptr.as_nonnull().cast(),
                Layout::array::<HashMapSlot<K, V>>(ocount).unwrap(),
            );
        }
    }

    ///
    /// Inserts `value` into the map with `key`, returning the existing value in the slot if present
    #[allow(clippy::cast_possible_truncation, clippy::missing_panics_doc)]
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if self.buckets == 0 {
            let ptr = self
                .alloc
                .allocate_zeroed(Layout::array::<HashMapSlot<K, V>>(16).unwrap())
                .unwrap_or_else(|| {
                    crate::alloc::handle_alloc_error(
                        Layout::array::<HashMapSlot<K, V>>(16).unwrap(),
                    )
                });
            self.htab = unsafe { Unique::new_nonnull_unchecked(ptr.cast()) };
            self.buckets = 16;
        }
        loop {
            let mut hasher = self.hash.build_hasher();
            key.hash(&mut hasher);
            let hash = hasher.finish() as usize % self.buckets;
            // SAFETY:
            // htab is as long as the number of available buckets, thus this offset is guaranteed to be valid
            let bucket = unsafe { &mut *self.htab.as_ptr().add(hash) };
            for i in 0..bucket.ecount {
                // SAFETY:
                // bucket.ecount never exceeds 16, by the check following this loop
                let key_val = unsafe { &mut *bucket.entries.get_unchecked_mut(i).as_mut_ptr() };

                if key_val.0.eq(&key) {
                    return Some(core::mem::replace(&mut key_val.1, value));
                }
            }

            if bucket.ecount == 16 {
                self.rehash();
                continue;
            }
            // SAFETY:
            // This is literally MaybeUninit::write, but valid in 1.39
            // TODO: Change this to MaybeUninit::write, since we are no longer in 1.39
            unsafe {
                bucket.entries[bucket.ecount]
                    .as_mut_ptr()
                    .write(Pair(key, value));
            }
            bucket.ecount += 1;
            return None;
        }
    }

    /// Returns a mutable reference to the value with the given key, inserting `value` in that slot if necessary
    #[allow(clippy::cast_possible_truncation, clippy::missing_panics_doc)]
    pub fn get_or_insert_mut(&mut self, key: K, value: V) -> &mut V {
        if self.buckets == 0 {
            let ptr = self
                .alloc
                .allocate_zeroed(Layout::array::<HashMapSlot<K, V>>(16).unwrap())
                .unwrap_or_else(|| {
                    crate::alloc::handle_alloc_error(
                        Layout::array::<HashMapSlot<K, V>>(16).unwrap(),
                    )
                });
            self.htab = unsafe { Unique::new_nonnull_unchecked(ptr.cast()) };
            self.buckets = 16;
        }
        loop {
            let mut hasher = self.hash.build_hasher();
            key.hash(&mut hasher);
            let hash = hasher.finish() as usize % self.buckets;
            // SAFETY:
            // htab is as long as the number of available buckets, thus this offset is guaranteed to be valid
            let bucket = unsafe { &mut *self.htab.as_ptr().add(hash) };
            for i in 0..bucket.ecount {
                // SAFETY:
                // bucket.ecount never exceeds 16, by the check following this loop
                let key_val = unsafe { &mut *bucket.entries.get_unchecked_mut(i).as_mut_ptr() };

                if key_val.0.eq(&key) {
                    return &mut key_val.1;
                }
            }

            if bucket.ecount == 16 {
                self.rehash();
                continue;
            }
            // SAFETY:
            // This is literally MaybeUninit::write, but valid in 1.39
            // TODO: Change this to MaybeUninit::write, since we're no longer bound to 1.39
            unsafe {
                bucket.entries[bucket.ecount]
                    .as_mut_ptr()
                    .write(Pair(key, value));
            }
            bucket.ecount += 1;
            return &mut unsafe { &mut *bucket.entries[bucket.ecount - 1].as_mut_ptr() }.1;
        }
    }

    /// Returns a mutable reference to the value with the given key, inserting the value produced by `value`
    #[allow(clippy::cast_possible_truncation, clippy::missing_panics_doc)]
    pub fn get_or_insert_with_mut<F: FnOnce(&K) -> V>(&mut self, key: K, value: F) -> &mut V {
        if self.buckets == 0 {
            let ptr = self
                .alloc
                .allocate_zeroed(Layout::array::<HashMapSlot<K, V>>(16).unwrap()) //
                .unwrap_or_else(|| {
                    crate::alloc::handle_alloc_error(
                        Layout::array::<HashMapSlot<K, V>>(16).unwrap(),
                    )
                });
            self.htab = unsafe { Unique::new_nonnull_unchecked(ptr.cast()) };
            self.buckets = 16;
        }
        loop {
            let mut hasher = self.hash.build_hasher();
            key.hash(&mut hasher);
            let hash = hasher.finish() as usize % self.buckets;
            // SAFETY:
            // htab is as long as the number of available buckets, thus this offset is guaranteed to be valid
            let bucket = unsafe { &mut *self.htab.as_ptr().add(hash) };
            for i in 0..bucket.ecount {
                // SAFETY:
                // bucket.ecount never exceeds 16, by the check following this loop
                let key_val = unsafe { &mut *bucket.entries.get_unchecked_mut(i).as_mut_ptr() };

                if key_val.0.eq(&key) {
                    return &mut key_val.1;
                }
            }

            if bucket.ecount == 16 {
                self.rehash();
                continue;
            }
            // SAFETY:
            // This is literally MaybeUninit::write, but valid in 1.39
            // TODO: Change this to MaybeUninit::write, since we're no longer bound to 1.39
            let value = value(&key);
            unsafe {
                bucket.entries[bucket.ecount]
                    .as_mut_ptr()
                    .write(Pair(key, value));
            }
            bucket.ecount += 1;
            return &mut unsafe { &mut *bucket.entries[bucket.ecount - 1].as_mut_ptr() }.1;
        }
    }

    /// Gets an immutable reference to the value given by `key`, if present, otherwise returns `None`.
    #[allow(clippy::cast_possible_truncation)]
    pub fn get<Q: Hash + Eq>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        if self.buckets == 0 {
            return None;
        }
        let mut hasher = self.hash.build_hasher();
        key.hash(&mut hasher);
        let hash = hasher.finish() as usize % self.buckets;

        let bucket = unsafe { &mut *self.htab.as_ptr().add(hash) };
        for i in 0..bucket.ecount {
            // SAFETY:
            // bucket.ecount never exceeds 16, by the check following this loop
            let key_val = unsafe { &*bucket.entries.get_unchecked_mut(i).as_mut_ptr() };

            if key_val.0.borrow().eq(key) {
                return Some(&key_val.1);
            }
        }

        None
    }

    /// Gets a mutable reference to the value given by `key`, if present, otherwise returns `None`.
    #[allow(clippy::cast_possible_truncation)]
    pub fn get_mut<Q: Hash + Eq>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
    {
        if self.buckets == 0 {
            return None;
        }
        let mut hasher = self.hash.build_hasher();
        key.hash(&mut hasher);
        let hash = hasher.finish() as usize % self.buckets;

        let bucket = unsafe { &mut *self.htab.as_ptr().add(hash) };
        for i in 0..bucket.ecount {
            // SAFETY:
            // bucket.ecount never exceeds 16, by the check following this loop
            let key_val = unsafe { &mut *bucket.entries.get_unchecked_mut(i).as_mut_ptr() };

            if key_val.0.borrow().eq(key) {
                return Some(&mut key_val.1);
            }
        }

        None
    }
}

impl<K: Clone, V: Clone, H: BuildHasher + Clone, A: Allocator + Clone> Clone
    for HashMap<K, V, H, A>
{
    #[allow(clippy::similar_names)]
    fn clone(&self) -> Self {
        let buckets = self.buckets;
        let hash = self.hash.clone();
        let alloc = self.alloc.clone();
        let htab = alloc
            .allocate_zeroed(Layout::array::<HashMapSlot<K, V>>(buckets).unwrap())
            .unwrap()
            .cast::<HashMapSlot<K, V>>();
        let ptr = htab.as_ptr();
        for i in 0..buckets {
            let bucket = unsafe { &mut *ptr.add(i) };
            let obucket = unsafe { &*self.htab.as_ptr().add(i) };
            for j in 0..obucket.ecount {
                unsafe {
                    bucket.entries[j]
                        .as_mut_ptr()
                        .write((&*obucket.entries[j].as_ptr()).clone());
                }
            }
        }
        Self {
            htab: unsafe { Unique::new_nonnull_unchecked(htab) },
            buckets,
            hash,
            alloc,
        }
    }
}

/// An [`Iterator`] over the keys and values of a [`HashMap`]
pub struct Iter<'a, K, V>(
    core::slice::Iter<'a, HashMapSlot<K, V>>,
    Option<(&'a HashMapSlot<K, V>, usize)>,
);

impl<'a, K, V> Iter<'a, K, V> {
    fn current_slot(&mut self) -> std::option::Option<(&'a HashMapSlot<K, V>, usize)> {
        if let Some((slot, offset)) = self.1 {
            Some((slot, offset))
        } else {
            let x = self.0.next()?;
            self.1 = Some((x, 0));
            Some((x, 0))
        }
    }
}

impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = &'a Pair<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        let (mut slot, mut offset) = self.current_slot()?;

        while offset == slot.ecount {
            slot = self.0.next()?;
            offset = 0;
            self.1 = Some((slot, 0));
        }
        self.1.as_mut().unwrap().1 += 1;
        Some(unsafe {
            &*(&slot.entries[offset] as *const MaybeUninit<Pair<K, V>>).cast::<Pair<K, V>>()
        })
    }
}

/// An [`Iterator`] over the values of a [`HashMap`]
pub struct Values<'a, K, V>(Iter<'a, K, V>);

impl<'a, K, V> Iterator for Values<'a, K, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.next() {
            Some(Pair(_, v)) => Some(v),
            None => None,
        }
    }
}

impl<'a, K, V, H: BuildHasher, A: Allocator> IntoIterator for &'a HashMap<K, V, H, A> {
    type Item = &'a Pair<K, V>;

    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K: Hash, V: Hash, H: BuildHasher, A: Allocator> Hash for HashMap<K, V, H, A> {
    fn hash<P: Hasher>(&self, state: &mut P) {
        for i in self {
            i.hash(state);
        }
    }
}

impl<'a, K: Debug, V: Debug, H: BuildHasher, A: Allocator> Debug for HashMap<K, V, H, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|Pair(k, v)| (k, v)))
            .finish()
    }
}
