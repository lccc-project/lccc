use std::mem::MaybeUninit;

use crate::pair::Pair;

struct HashMapSlot<K, V> {
    ecount: usize,
    entries: [MaybeUninit<Pair<K, V>>],
}
