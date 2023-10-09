use std::{collections::VecDeque, iter::FusedIterator}; // FIXME: Use `xlang::abi::VecDeque` instead

use xlang::abi::ops::{ControlFlow, FromResidual, Try};

pub struct PeekMoreIterator<I: Iterator> {
    inner: I,
    buf: VecDeque<I::Item>,
    cursor: usize,
    mark: Vec<usize>,
}

impl<I: Iterator> Iterator for PeekMoreIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(val) = self.buf.pop_front() {
            self.cursor = self.cursor.saturating_sub(1);
            self.mark = self
                .mark
                .iter_mut()
                .for_each(|x| *x = (*x).saturating_sub(1));
            Some(val)
        } else {
            self.inner.next()
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.buf.len();

        let (min, max) = self.inner.size_hint();

        let min = min.saturating_add(len);
        let max = max.and_then(|max| max.checked_add(len));

        (min, max)
    }
}

impl<I: FusedIterator> FusedIterator for PeekMoreIterator<I> {}

impl<I: Iterator> PeekMoreIterator<I> {
    pub fn peek(&mut self) -> Option<&Self::Item> {
        if self.cursor < self.buf.len() {
            Some(&self.buf[self.cursor])
        } else {
            let val = self.inner.next()?;
            self.cursor = self.buf.len();
            self.buf.push_back(val);
            Some(&self.buf[self.cursor])
        }
    }

    pub fn peek_next(&mut self) -> Option<&Self::Item> {
        if self.cursor < self.buf.len() {
            self.cursor += 1;
        }
        self.peek()
    }

    pub fn peek_last(&mut self) -> Option<&Self::Item> {
        self.cursor = self.cursor.checked_sub(1)?;
        self.peek()
    }

    pub fn mark(&mut self) {
        self.mark.push(self.cursor);
    }
    pub fn clear_mark(&mut self) {
        self.mark.pop();
    }
    pub fn rewind(&mut self) {
        if let Some(mark) = self.mark.pop() {
            self.cursor = mark;
        }
    }
}

pub trait Peekmore: Iterator {
    fn peekmore(self) -> PeekMoreIterator<Self>;
}

impl<I: Iterator> Peekmore for I {
    fn peekmore(self) -> PeekMoreIterator<Self> {
        PeekMoreIterator {
            inner: self,
            buf: VecDeque::with_capacity(8),
            cursor: 0,
            mark: None,
        }
    }
}
pub struct Rewinder<'a, T: Iterator> {
    inner: &'a mut PeekMoreIterator<T>,
}

impl<T: Iterator> Rewinder<'_, T> {
    pub fn accept(self) {
        self.inner.clear_mark();
        core::mem::forget(self)
    }
}

impl<'a, T: Iterator> From<&'a mut PeekMoreIterator<T>> for Rewinder<'a, T> {
    fn from(inner: &'a mut PeekMoreIterator<T>) -> Self {
        Self { inner }
    }
}

impl<'a, T: Iterator> Drop for Rewinder<'a, T> {
    fn drop(&mut self) {
        self.inner.rewind()
    }
}

impl<'a, T: Iterator> Deref for Rewinder<'a, T> {
    type Target = PeekMoreIterator<T>;

    fn deref(&self) -> &PeekMoreIterator<T> {
        &self.inner
    }
}

impl<'a, T: Iterator> DerefMut for Rewinder<'a, T> {
    fn deref_mut(&mut self) -> &mut PeekMoreIterator<T> {
        &mut self.inner
    }
}

pub trait IntoRewinder<'a> {
    type Iter: Iterator;
    fn into_rewinder(self) -> Rewinder<'a, Self::Iter>;
}

impl<'a, T: Iterator> IntoRewinder<'a> for &'a mut PeekMoreIterator<T> {
    type Iter = T;
    fn into_rewinder(self) -> Rewinder<'a, T> {
        Rewinder::from(self)
    }
}

pub fn with_rewinder_accept_on_continue<
    C: Try,
    I: Iterator,
    F: FnOnce(&mut PeekMoreIterator<I>) -> C,
>(
    tree: &mut PeekMoreIterator<I>,
    f: F,
) -> C {
    let mut tree = tree.into_rewinder();
    let val = f(&mut tree);
    match val.branch() {
        ControlFlow::Break(res) => {
            drop(tree);
            C::from_residual(res)
        }
        ControlFlow::Continue(val) => {
            tree.accept();
            C::from_output(val)
        }
    }
}
pub fn with_rewinder_accept_on_break<
    C: Try,
    I: Iterator,
    F: FnOnce(&mut PeekMoreIterator<I>) -> C,
>(
    tree: &mut PeekMoreIterator<I>,
    f: F,
) -> C {
    let mut tree = tree.into_rewinder();
    let val = f(&mut tree);
    match val.branch() {
        ControlFlow::Break(res) => {
            tree.accept();
            C::from_residual(res)
        }
        ControlFlow::Continue(val) => {
            drop(tree);
            C::from_output(val)
        }
    }
}
