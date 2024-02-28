use std::{
    collections::VecDeque,
    iter::FusedIterator,
    ops::{Deref, DerefMut},
}; // FIXME: Use `xlang::abi::VecDeque` instead

use xlang::abi::ops::{ControlFlow, Try};

use crate::{
    span::{Pos, Speekable},
    symbol::Symbol,
};

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
            self.mark
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

impl<I: Iterator<Item = char>> PeekMoreIterator<Speekable<I>> {
    pub fn last_pos(&mut self) -> Pos {
        if let Some((pos, _)) = self.cursor() {
            *pos
        } else {
            self.inner.last_pos()
        }
    }

    pub fn file_name(&self) -> Symbol {
        self.inner.file_name()
    }
}

impl<I: Iterator> PeekMoreIterator<I> {
    fn cursor(&self) -> Option<&I::Item> {
        if self.cursor != 0 && self.cursor <= self.buf.len() {
            Some(&self.buf[self.cursor - 1])
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        if let Some(item) = self.cursor() {
            Some(unsafe { &*(item as *const I::Item) })
        } else {
            let val = self.inner.next()?;
            self.buf.push_back(val);
            self.cursor = self.buf.len();
            Some(&self.buf[self.cursor - 1])
        }
    }

    pub fn peek_next(&mut self) -> Option<&I::Item> {
        if self.cursor <= self.buf.len() {
            self.cursor += 1;
        }
        self.peek()
    }

    pub fn peek_last(&mut self) -> Option<&I::Item> {
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
    pub fn reset(&mut self) {
        self.cursor = 0;
    }
    pub fn consume_peaked(&mut self) {
        if self.cursor == self.buf.len() {
            self.buf.clear();
        } else {
            drop(self.buf.drain(..(self.cursor.saturating_sub(1))));
        }

        self.cursor = 0;
        self.mark.clear();
    }
}

pub trait Peekmore: Iterator {
    fn peekmore(self) -> PeekMoreIterator<Self>
    where
        Self: Sized;
}

impl<I: Iterator> Peekmore for I {
    fn peekmore(self) -> PeekMoreIterator<Self> {
        PeekMoreIterator {
            inner: self,
            buf: VecDeque::with_capacity(8),
            cursor: 0,
            mark: Vec::new(),
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
        inner.mark();
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum DecodeUtf8Error {
    UnexpectedEof,
    InvalidStartChar(u8),
}

#[derive(Debug, Clone)]
pub struct DecodeUtf8<I>(I);

impl<I> DecodeUtf8<I> {
    pub const fn new(inner: I) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> I {
        self.0
    }
}

impl<I: Iterator<Item = u8>> Iterator for DecodeUtf8<I> {
    type Item = Result<char, DecodeUtf8Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let b = self.0.next()?;
        match b {
            0x00..=0x7f => Some(Ok(b as char)),
            0xC0..=0xDF => {
                let n = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n & 0xC0) != 0x80 {
                    return None;
                }

                let c = (((b & 0x1F) as u32) << 6) | (n & 0x3F) as u32;

                char::from_u32(c).map(Ok)
            }
            0xE0..=0xEF => {
                let n1 = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n1 & 0xC0) != 0x80 {
                    return None;
                }
                let n2 = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n2 & 0xC0) != 0x80 {
                    return None;
                }

                let c = (((b & 0x1F) as u32) << 12)
                    | (((n1 & 0x3F) as u32) << 6)
                    | ((n2 & 0x3F) as u32);
                char::from_u32(c).map(Ok)
            }
            0xF0..=0xF7 => {
                let n1 = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n1 & 0xC0) != 0x80 {
                    return None;
                }
                let n2 = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n2 & 0xC0) != 0x80 {
                    return None;
                }
                let n3 = match self.0.next() {
                    Some(n) => n,
                    None => return Some(Err(DecodeUtf8Error::UnexpectedEof)),
                };
                if (n3 & 0xC0) != 0x80 {
                    return None;
                }

                let c = (((b & 0x1F) as u32) << 18)
                    | (((n1 & 0x3F) as u32) << 12)
                    | (((n2 & 0x3F) as u32) << 6)
                    | ((n3 & 0x3F) as u32);
                char::from_u32(c).map(Ok)
            }
            b => Some(Err(DecodeUtf8Error::InvalidStartChar(b))),
        }
    }
}
