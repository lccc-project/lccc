use core::ops::IndexMut;
use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::{BuildHasher, Hash, Hasher};
use std::iter::{FromIterator, FusedIterator};
use std::marker::PhantomData;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ops::Index;

use crate::pair::Pair;

use crate::alloc::{Allocator, Layout, XLangAlloc};
use crate::hash::{BuildHasherDefault, XLangHasher};
use crate::ptr::Unique;
use crate::vec::Vec;

/// A trait for maps and sets, that allows you to check for whether a particular key is present
pub trait Searchable<Q: ?Sized> {
    /// Searches [`Self`] for the given `key`.
    fn contains_key(&self, key: &Q) -> bool;
}

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

    /// Produces an [`Iterator`] over pairs of keys and mutable values in this [`HashMap`]
    /// Note that they keys are not mutable, to avoid mutation that would affect the implementation of [`Eq`] and [`Hash`]
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        let begin = self.htab.as_ptr();
        let end = unsafe { begin.add(self.buckets) };
        IterMut {
            buckets: begin,
            buckets_end: end,
            bucket_position: 0,
            phantom: PhantomData,
        }
    }

    /// Produces an [`Iterator`] over the values in this [`HashMap`]
    pub fn values(&self) -> Values<'_, K, V> {
        Values(self.iter())
    }

    /// Produces an [`Iterator`] over the keys in this [`HashMap`]
    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys(self.iter())
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
    pub fn get<Q: ?Sized + Hash + Eq>(&self, key: &Q) -> Option<&V>
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
    pub fn get_mut<Q: ?Sized + Hash + Eq>(&mut self, key: &Q) -> Option<&mut V>
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

    /// Checks whether the given key is present in the map
    pub fn contains_key<Q: ?Sized + Hash + Eq>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
    {
        self.get(key).is_some()
    }

    /// Removes an entry by Key from this map, and returns the Key and Value
    #[allow(clippy::cast_possible_truncation)]
    pub fn remove<Q: ?Sized + Hash + Eq>(&mut self, key: &Q) -> Option<Pair<K, V>>
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
                let val = unsafe { core::ptr::read(key_val) };
                bucket.ecount -= 1;
                for j in i..bucket.ecount {
                    unsafe {
                        core::ptr::write(
                            bucket.entries.as_mut_ptr().add(j),
                            core::ptr::read(bucket.entries.as_mut_ptr().add(j + 1)),
                        );
                    }
                }
                return Some(val);
            }
        }
        None
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F: FnMut(&K, &mut V) -> bool>(&mut self, mut f: F) {
        for bucket in 0..self.buckets {
            let bucket = unsafe { &mut *(self.htab.as_ptr().add(bucket)) };

            for i in (0..bucket.ecount).rev() {
                let key_val = unsafe { &mut *bucket.entries.get_unchecked_mut(i).as_mut_ptr() };
                if !f(&key_val.0, &mut key_val.1) {
                    bucket.entries.swap(i, bucket.ecount - 1);
                    unsafe {
                        core::ptr::drop_in_place(
                            bucket
                                .entries
                                .get_unchecked_mut(bucket.ecount - 1)
                                .as_mut_ptr(),
                        )
                    }
                    bucket.ecount -= 1;
                }
            }
        }
    }
}

impl<K: Hash + Eq, V, H: BuildHasher, A: Allocator, Q: ?Sized + Hash + Eq> Searchable<Q>
    for HashMap<K, V, H, A>
where
    Q: Borrow<K>,
{
    fn contains_key(&self, key: &Q) -> bool {
        HashMap::contains_key(self, key.borrow())
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
            bucket.ecount = obucket.ecount;
        }
        Self {
            htab: unsafe { Unique::new_nonnull_unchecked(htab) },
            buckets,
            hash,
            alloc,
        }
    }
}

impl<K, V, A: Allocator, H: BuildHasher> IntoIterator for HashMap<K, V, H, A> {
    type Item = Pair<K, V>;

    type IntoIter = IntoIter<K, V, A>;

    fn into_iter(self) -> Self::IntoIter {
        let mut this = ManuallyDrop::new(self);

        let htab = unsafe { core::ptr::read(core::ptr::addr_of!(this.htab)) };
        let buckets = this.buckets;
        let alloc = unsafe { core::ptr::read(core::ptr::addr_of!(this.alloc)) };
        unsafe { core::ptr::drop_in_place(core::ptr::addr_of_mut!(this.hash)) };

        IntoIter {
            htab,
            buckets,
            alloc,
            pos: (0, 0),
        }
    }
}

/// A by-value iterator over the keys and values in a [`HashMap`]
pub struct IntoIter<K, V, A: Allocator> {
    htab: Unique<HashMapSlot<K, V>>,
    buckets: usize,
    alloc: A,
    pos: (usize, usize),
}

impl<K, V, A: Allocator> Drop for IntoIter<K, V, A> {
    fn drop(&mut self) {
        if core::mem::needs_drop::<Pair<K, V>>() {
            for i in &mut *self {
                core::mem::drop(i);
            }
        }
        unsafe {
            self.alloc.deallocate(
                self.htab.as_nonnull().cast(),
                Layout::array::<HashMapSlot<K, V>>(self.buckets).unwrap(),
            );
        }
    }
}

impl<K, V, A: Allocator> Iterator for IntoIter<K, V, A> {
    type Item = Pair<K, V>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (bucket, pos) = self.pos;
            if bucket >= self.buckets {
                break None;
            }
            // SAFETY:
            // Check above ensures this is inbounds
            let slot = unsafe { &mut *self.htab.as_ptr().add(bucket) };

            if pos >= slot.ecount {
                self.pos = (bucket + 1, 0);
                continue;
            }

            self.pos = (bucket, pos + 1);

            // SAFETY:
            // We're init and inbounds by the above check.
            break Some(unsafe { core::ptr::read(slot.entries.as_ptr().add(pos).cast()) });
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

#[allow(clippy::borrow_as_ptr)]
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

/// An [`Iterator`] over the keys of a [`HashMap`]
pub struct Keys<'a, K, V>(Iter<'a, K, V>);

impl<'a, K, V> Iterator for Keys<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.next() {
            Some(Pair(k, _)) => Some(k),
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

impl<'a, K, V, H: BuildHasher, A: Allocator> IntoIterator for &'a mut HashMap<K, V, H, A> {
    type Item = Pair<&'a K, &'a mut V>;

    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<K: Hash, V: Hash, H: BuildHasher, A: Allocator> Hash for HashMap<K, V, H, A> {
    fn hash<P: Hasher>(&self, state: &mut P) {
        for i in self {
            i.hash(state);
        }
    }
}

impl<K: Debug, V: Debug, H: BuildHasher, A: Allocator> Debug for HashMap<K, V, H, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|Pair(k, v)| (k, v)))
            .finish()
    }
}

impl<'a, K: Eq + Hash, V, Q: Eq + Hash + 'a, H: BuildHasher, A: Allocator> Index<&'a Q>
    for HashMap<K, V, H, A>
where
    K: Borrow<Q>,
{
    type Output = V;
    fn index(&self, idx: &'a Q) -> &V {
        self.get(idx).expect("no entry found for key")
    }
}

impl<'a, K: Eq + Hash, V, Q: Eq + Hash + 'a, H: BuildHasher, A: Allocator> IndexMut<&'a Q>
    for HashMap<K, V, H, A>
where
    K: Borrow<Q>,
{
    fn index_mut(&mut self, idx: &'a Q) -> &mut V {
        self.get_mut(idx).expect("no entry found for key")
    }
}

impl<K: Eq + Hash, V: PartialEq, H: BuildHasher, A: Allocator> PartialEq for HashMap<K, V, H, A> {
    fn eq(&self, other: &Self) -> bool {
        if core::ptr::eq(self, other) {
            return true;
        }
        for Pair(k, v) in self {
            if other.get(k).map_or(false, |v2| v.eq(v2)) {
                return false;
            }
        }
        for Pair(k, v) in other {
            if self.get(k).map_or(false, |v2| v.eq(v2)) {
                return false;
            }
        }

        true
    }
}

impl<K: Eq + Hash, V: Eq, H: BuildHasher, A: Allocator> Eq for HashMap<K, V, H, A> {}

impl<K: Eq + Hash, V, H: BuildHasher + Default, A: Allocator + Default> FromIterator<Pair<K, V>>
    for HashMap<K, V, H, A>
{
    fn from_iter<I>(it: I) -> Self
    where
        I: IntoIterator<Item = Pair<K, V>>,
    {
        let mut ret = Self::new();
        for Pair(k, v) in it {
            ret.insert(k, v);
        }
        ret
    }
}

impl<K: Eq + Hash, V, H: BuildHasher, A: Allocator> Extend<Pair<K, V>> for HashMap<K, V, H, A> {
    fn extend<I>(&mut self, it: I)
    where
        I: IntoIterator<Item = Pair<K, V>>,
    {
        for Pair(k, v) in it {
            self.insert(k, v);
        }
    }
}

impl<K: Eq + Hash, V, H: BuildHasher + Default, A: Allocator + Default> FromIterator<(K, V)>
    for HashMap<K, V, H, A>
{
    fn from_iter<I>(it: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        let mut ret = Self::new();
        for (k, v) in it {
            ret.insert(k, v);
        }
        ret
    }
}

impl<K: Eq + Hash, V, H: BuildHasher, A: Allocator> Extend<(K, V)> for HashMap<K, V, H, A> {
    fn extend<I>(&mut self, it: I)
    where
        I: IntoIterator<Item = (K, V)>,
    {
        for (k, v) in it {
            self.insert(k, v);
        }
    }
}

/// An [`Iterator`] over the keys and mutable values of a [`HashMap`].
pub struct IterMut<'a, K, V> {
    buckets: *mut HashMapSlot<K, V>,
    buckets_end: *mut HashMapSlot<K, V>,
    bucket_position: usize,
    phantom: PhantomData<&'a mut [HashMapSlot<K, V>]>,
}

impl<'a, K, V> IterMut<'a, K, V> {
    fn next_item(&mut self) -> Option<&'a mut Pair<K, V>> {
        if self.buckets == self.buckets_end {
            None
        } else {
            let mut ecount = unsafe { (*self.buckets).ecount };
            let mut entries =
                unsafe { core::ptr::addr_of_mut!((*self.buckets).entries) }.cast::<Pair<K, V>>();

            while self.bucket_position >= ecount {
                self.bucket_position = 0;
                self.buckets = unsafe { self.buckets.offset(1) };
                if self.buckets == self.buckets_end {
                    return None;
                }
                ecount = unsafe { (*self.buckets).ecount };
                entries = unsafe { core::ptr::addr_of_mut!((*self.buckets).entries) }
                    .cast::<Pair<K, V>>();
            }

            let ret = unsafe { entries.add(self.bucket_position) };
            self.bucket_position += 1;

            Some(unsafe { &mut *ret })
        }
    }
}

impl<'a, K, V> Iterator for IterMut<'a, K, V> {
    type Item = Pair<&'a K, &'a mut V>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_item().map(|Pair(k, v)| Pair(k as &K, v))
    }
}

impl<'a, K, V> FusedIterator for IterMut<'a, K, V> {}

/// An [`Iterator`] over the mutable values in a [`HashMap`]
#[allow(dead_code)] // Public api, false positive
pub struct ValuesMut<'a, K, V>(IterMut<'a, K, V>);

impl<'a, K, V> Iterator for ValuesMut<'a, K, V> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<&'a mut V> {
        self.0.next().map(|Pair(_, v)| v)
    }
}

///
/// A [`HashSet`] that has a stable ABI and obtains storage backed by an [`Allocator`]
#[derive(
    Clone, Debug /* may not be good to derive(Debug) here. Maybe should provide impl instead */,
)]
pub struct HashSet<K, H: BuildHasher = BuildHasherDefault<XLangHasher>, A: Allocator = XLangAlloc> {
    inner: HashMap<K, (), H, A>,
}

impl<K: Eq + Hash, H: BuildHasher, A: Allocator> PartialEq for HashSet<K, H, A> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<K: Eq + Hash, H: BuildHasher, A: Allocator> Eq for HashSet<K, H, A> {}

impl<K: Eq + Hash, H: BuildHasher, A: Allocator> Hash for HashSet<K, H, A> {
    fn hash<Hash: Hasher>(&self, hasher: &mut Hash) {
        self.inner.hash(hasher);
    }
}

impl<K, H: BuildHasher + Default, A: Allocator + Default> HashSet<K, H, A> {
    /// Constructs a new [`HashSet`] with a [`Default`] allocator and [`Default`] hasher
    #[must_use]
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }
}

impl<K, H: BuildHasher + Default, A: Allocator + Default> Default for HashSet<K, H, A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, H: BuildHasher, A: Allocator + Default> HashSet<K, H, A> {
    /// Constructs a new [`HashSet`] with the given hasher and a [`Default`] allocator
    #[must_use]
    pub fn with_hasher(hasher: H) -> Self {
        Self {
            inner: HashMap::with_hasher(hasher),
        }
    }
}

impl<K, H: BuildHasher + Default, A: Allocator> HashSet<K, H, A> {
    /// Constructs a new [`HashSet`] with the given allocator and a [`Default`] hasher
    #[must_use]
    pub fn new_in(alloc: A) -> Self {
        Self {
            inner: HashMap::new_in(alloc),
        }
    }
}

impl<K, H: BuildHasher, A: Allocator> HashSet<K, H, A> {
    /// Constructs a new [`HashSet`] with the given hasher and allocator
    #[must_use]
    pub fn with_hasher_in(hasher: H, alloc: A) -> Self {
        Self {
            inner: HashMap::with_hasher_in(hasher, alloc),
        }
    }

    /// Produces an [`Iterator`] over each element of the set
    pub fn iter(&self) -> SetIter<'_, K> {
        SetIter(self.inner.keys())
    }
}

impl<K, H: BuildHasher, A: Allocator> IntoIterator for HashSet<K, H, A> {
    type Item = K;
    type IntoIter = SetIntoIter<K, A>;
    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter(self.inner.into_iter())
    }
}

impl<K: Eq + Hash, H: BuildHasher, A: Allocator> HashSet<K, H, A> {
    ///
    /// Checks if `self` contains the given val.
    pub fn contains<Q: ?Sized + Hash + Eq>(&self, val: &Q) -> bool
    where
        K: Borrow<Q>,
    {
        self.inner.get(val).is_some()
    }

    ///
    /// Inserts `val` into the set if it is not already present (according to the [`Eq`] implementation), or returns it (as an Err) instead.
    #[allow(clippy::missing_errors_doc)] // Not an Error to return the `K`
    pub fn insert(&mut self, val: K) -> Result<(), K> {
        if self.inner.get(&val).is_some() {
            Err(val)
        } else {
            self.inner.insert(val, ());
            Ok(())
        }
    }

    /// Removes a value from the set and returns it if present
    pub fn remove<Q: ?Sized + Hash + Eq>(&mut self, val: &Q) -> Option<K>
    where
        K: Borrow<Q>,
    {
        self.inner.remove(val).map(|Pair(k, _)| k)
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F: FnMut(&K) -> bool>(&mut self, mut f: F) {
        self.inner.retain(|k, _| f(k));
    }
}

impl<K: Eq + Hash, H: BuildHasher, A: Allocator, Q: ?Sized + Hash + Default> Searchable<Q>
    for HashSet<K, H, A>
where
    Q: Borrow<K>,
{
    fn contains_key(&self, key: &Q) -> bool {
        HashSet::contains(self, key.borrow())
    }
}

impl<K: Eq + Hash, H: BuildHasher + Default, A: Allocator + Default> FromIterator<K>
    for HashSet<K, H, A>
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = K>,
    {
        let mut out = Self::new();
        for item in iter {
            drop(out.insert(item));
        }
        out
    }
}

impl<K: Eq + Hash, H: BuildHasher + Default, A: Allocator + Default> Extend<K>
    for HashSet<K, H, A>
{
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = K>,
    {
        for item in iter {
            drop(self.insert(item));
        }
    }
}

/// Creates a [`HashSet`] from given elements, discarding duplicates
#[macro_export]
macro_rules! set{
    {$($expr:expr),*} => {{
        let __array = [$($expr),*];
        $crate::collection::HashSet::from_iter(::core::iter::IntoIterator::into_iter(__array))
    }}
}

/// An [`Iterator`] over the (borrowed) values in a [`HashSet`]
pub struct SetIter<'a, K>(Keys<'a, K, ()>);

impl<'a, K> Iterator for SetIter<'a, K> {
    type Item = &'a K;

    fn next(&mut self) -> Option<&'a K> {
        self.0.next()
    }
}

/// An [`Iterator`] over the (owned) values in a [`HashSet`]
pub struct SetIntoIter<K, A: Allocator = XLangAlloc>(IntoIter<K, (), A>);

impl<K, A: Allocator> Iterator for SetIntoIter<K, A> {
    type Item = K;

    fn next(&mut self) -> Option<K> {
        self.0.next().map(|Pair(k, _)| k)
    }
}

/// A double ended queue backed by an expandable ring buffer
pub struct VecDeque<T, A: Allocator = XLangAlloc> {
    storage: Vec<MaybeUninit<T>, A>,
    head: usize,
    tail: usize,
}

impl<T> VecDeque<T, XLangAlloc> {
    /// Creates a new [`VecDeque`] with no initial capacity
    pub const fn new() -> Self {
        Self::new_in(XLangAlloc::new())
    }
    /// Constructs a [`VecDeque`] that can hold at least `cap` elements before reallocating
    pub fn with_capacity(cap: usize) -> Self {
        Self::with_capacity_in(cap, XLangAlloc::new())
    }
}
impl<T, A: Allocator> VecDeque<T, A> {
    /// Creates a new [`VecDeque`] with no initial capacity using `alloc`
    pub const fn new_in(alloc: A) -> Self {
        Self {
            storage: Vec::new_in(alloc),
            head: 0,
            tail: 0,
        }
    }
    /// Constructs a [`VecDeque`] that can hold at least `cap` elements before reallocating, using `alloc`
    pub fn with_capacity_in(cap: usize, alloc: A) -> Self {
        Self {
            storage: Vec::with_capacity_in(cap, alloc),
            head: 0,
            tail: 0,
        }
    }

    /// Determines the number of elements in the [`VecDeque`]
    pub const fn len(&self) -> usize {
        (self.tail - self.head) & (self.storage.capacity())
    }

    /// Determines if the [`VecDeque`] is empty (IE. has no elements)
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Determines the capacity of the [`VecDeque`]
    pub const fn capacity(&self) -> usize {
        self.storage.capacity()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_hash_map_get() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        assert_eq!(map.get("Hi"), Some(&0));
    }
    #[test]
    fn test_hash_map_missing() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        assert_eq!(map.get("Bye"), None);
    }

    #[test]
    fn test_hash_map_iter_count() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        assert_eq!(map.iter().count(), 2);
    }

    #[test]
    fn test_hash_map_iter_mut_count() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        assert_eq!(map.iter_mut().count(), 2);
    }

    #[test]
    fn test_hash_map_iter_mut_count1() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        assert_eq!(map.iter_mut().count(), 1);
    }

    #[test]
    fn test_hash_map_iter_present() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        let mut iter = map.iter();
        assert!(iter.any(|Pair(s, _)| *s == "Hi"));
        assert!(!iter.any(|Pair(s, _)| *s == "Hi"));
    }

    #[test]
    fn test_hash_map_iter_present2() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        let mut iter = map.iter();
        assert!(iter.any(|Pair(s, _)| *s == "Hello"));
        assert!(!iter.any(|Pair(s, _)| *s == "Hello"));
    }

    #[test]
    fn test_hash_map_clone() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        let map2 = map.clone();
        let mut iter = map2.iter();
        assert!(iter.any(|Pair(s, _)| *s == "Hello"));
        assert!(!iter.any(|Pair(s, _)| *s == "Hello"));
    }

    #[test]
    fn test_hash_map_into_iter_count() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        let iter = map.into_iter();
        assert_eq!(iter.count(), 2);
    }

    #[test]
    fn test_hash_map_into_iter() {
        let mut map = HashMap::<&str, i32>::new();
        map.insert("Hi", 0);
        map.insert("Hello", 1);
        let mut iter = map.into_iter();
        assert!(iter.any(|Pair(s, _)| s == "Hello"));
    }

    #[test]
    fn test_hash_map_remove() {
        let mut map = HashMap::<String, String>::new();
        map.insert("Hello".to_string(), "World".to_string());
        assert!(map.remove("Hello").unwrap() == Pair("Hello", "World"));
    }

    #[test]
    fn test_hash_map_remove_and_drop() {
        let mut map = HashMap::<String, String>::new();
        map.insert("Hello".to_string(), "World".to_string());
        map.insert("Goodbye".to_string(), "Universe".to_string());
        assert!(map.remove("Hello").unwrap() == Pair("Hello", "World"));
    }

    #[test]
    fn test_hash_map_add_a_whole_hecking_lot_of_random_data() {
        let mut map = HashMap::<u8, u8>::new();
        let mut vals = Vec::<u8>::new();
        for i in 0u8..=255 {
            let val = i.wrapping_mul(37);
            vals.push(val);
            map.insert(i, val);
        }
        for i in 0..=255 {
            assert!(map[&i] == vals[i as usize]);
        }
    }
}
