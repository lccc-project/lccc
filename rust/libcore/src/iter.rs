/**
 * rust/libcore/iter.rs
 * This file is part of lcrust standard libraries, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  the lcrust standard libraries are additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */
use crate::ops::FnMut;
use crate::Option::{None, Some};
use crate::{Option, Sized};
use alloc::str::EncodeUtf16;

#[must_use = "Iterators are lazy, and most operations will have no effect unless consumed"]
pub trait Iterator {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;

    fn size_hint(&mut self) -> (usize, Option<usize>) {
        (usize::MAX, None)
    }

    fn count(mut self) -> usize {
        let mut c = 0usize;
        while let Some(_) = self.next() {
            c += 1;
        }
        c
    }
    fn last(mut self) -> Option<T> {
        let mut item = None;
        while let Some(t) = self.next() {
            item = Some(t);
        }
        item
    }

    fn nth(&mut self, mut n: usize) -> Option<T> {
        let mut item = self.next();
        while n > 0 {
            n -= 1;
            item = self.next();
            if let None = item {
                break;
            }
        }
        item
    }

    fn step_by(self, by: usize) -> StepBy<Self>
    where
        Self: Sized,
    {
        if by == 0 {
            panic!("step_by called with 0 as step");
        } else {
            StepBy { iter: self, by }
        }
    }

    fn chain<U: IntoIterator<Item = Self::Item>>(self, other: U) -> Chain<Self, U::IntoIter>
    where
        Self: Sized,
    {
        Chain { a: self, b: other }
    }

    fn zip<U: IntoIterator>(self, other: U) -> Zip<Self, U::IntoIter>
    where
        Self: Sized,
    {
        Zip { a: self, b: other }
    }

    fn map<U, F: FnMut(Self::Item) -> U>(self, op: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map { it: self, op }
    }

    fn for_each<F: FnMut(Self::Item)>(mut self, op: F)
    where
        Self: Sized,
    {
        while let Some(v) = self.next() {
            op(v)
        }
    }

    fn filter<P: FnMut(&Self::Item) -> bool>(self, pred: P) -> Filter<Self, P>
    where
        Self: Sized,
    {
        Filter { it: self, pred }
    }

    fn filter_map<B, F: FnMut(Self::Item) -> Option<B>>(self, op: F) -> FilterMap<Self, F>
    where
        Self: Sized,
    {
        FilterMap { it: self, op }
    }

    fn enumerate(self) -> Enumerate<Self>
    where
        Self: Sized,
    {
        Enumerate {
            it: self,
            cnt: 0usize,
        }
    }

    fn peekable(self) -> Peekable<Self>
    where
        Self: Sized,
    {
        Peekable { it: self, v: None }
    }

    fn skip_while<P: FnMut(&Self::Item) -> bool>(self, pred: P) -> SkipWhile<Self, P>
    where
        Self: Sized,
    {
        SkipWhile {
            it: self,
            pred: Some(pred),
        }
    }

    fn take_while<P: FnMut(&Self::Item) -> bool>(self, pred: P) -> TakeWhile<Self, P>
    where
        Self: Sized,
    {
        TakeWhile {
            it: self,
            pred,
            done: false,
        }
    }
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct StepBy<I> {
    iter: I,
    by: usize,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Chain<A, B> {
    a: A,
    b: B,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Zip<A, B> {
    a: A,
    b: B,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Map<I, F> {
    it: I,
    op: F,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Filter<I, P> {
    it: I,
    pred: P,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct FilterMap<I, F> {
    it: I,
    op: F,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Enumerate<I> {
    it: I,
    cnt: usize,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct Peakable<I: Iterator> {
    it: I,
    val: Option<I::Item>,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct SkipWhile<I, P> {
    it: I,
    pred: Option<P>,
}

#[must_use = "Iterators are lazy and most operations will have no effect unless consumed"]
pub struct TakeWhile<I, P> {
    it: I,
    pred: P,
    done: bool,
}

impl<I: Iterator> Iterator for StepBy<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.nth(self.by)
    }
}

impl<A: Iterator, B: Iterator<Item = A::Item>> Iterator for Chain<A, B> {
    type Item = A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.a.next();
        if let None = val {
            self.b.next()
        } else {
            val
        }
    }
}

impl<A: Iterator, B: Iterator> Iterator for Zip<A, B> {
    type Item = (A::Item, B::Item);

    fn next(&mut self) -> Option<Self::Item> {
        self.a.next().zip(self.b.next())
    }
}

impl<I: Iterator, U, F: FnMut(I::Item) -> U> Iterator for Map<I, F> {
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(&mut self.op)
    }
}

impl<I: Iterator, P: FnMut(&I::Item) -> bool> Iterator for Filter<I, P> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.it.next() {
                Some(v) if self.pred(&v) => break Some(v),
                None => break None,
                _ => {}
            }
        }
    }
}

impl<I: Iterator, U, F: FnMut(I::Item) -> Option<U>> Iterator for FilterMap<I, F> {
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().and_then(&mut self.op)
    }
}

impl<I: Iterator> Iterator for Enumerate<I> {
    type Item = (usize, I::Item);

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|v| {
            let cnt = self.cnt;
            self.cnt += 1;
            (cnt, v)
        })
    }
}

impl<I: Iterator> Iterator for Peakable<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(t) = self.val.take() {
            Some(t)
        } else {
            self.it.next()
        }
    }
}

impl<I: Iterator> Peakable<I> {
    pub fn peak(&mut self) -> Option<&I::Item> {
        if let None = self.val {
            self.val = self.it.next();
        }
        self.val.as_ref()
    }
}

impl<I: Iterator, P: FnMut(&I::Item) -> bool> Iterator for SkipWhile<I, P> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let pred = self.pred.take();
        if let Some(mut pred) = pred {
            while let Some(val) = self.it.next() {
                if !pred(&val) {
                    return Some(val);
                }
            }
            None
        } else {
            self.it.next()
        }
    }
}

impl<I: Iterator, P: FnMut(&I::Item) -> bool> Iterator for TakeWhile<I, P> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.done {
            match self.it.next() {
                Some(val) if self.pred(&val) => Some(val),
                _ => {
                    self.done = true;
                    None
                }
            }
        } else {
            None
        }
    }
}

pub trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter;
}

impl<I: Iterator> IntoIterator for I {
    type Item = Self::Item;
    type IntoIter = Self;

    fn into_iter(self) -> Self::IntoIter {
        self
    }
}

impl<I: Iterator> Iterator for &'_ mut I {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        <I as Iterator>::next(*self)
    }
}
