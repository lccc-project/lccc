use core::fmt::Write;

use xlang::prelude::v1::HashMap;

use crate::{
    helpers::FetchIncrement,
    interning::{self, Symbol},
};

pub trait Mangle {
    fn write<W: core::fmt::Write>(&self, buf: &mut W) -> core::fmt::Result;
}

impl<M: Mangle + ?Sized> Mangle for &M {
    fn write<W: core::fmt::Write>(&self, buf: &mut W) -> core::fmt::Result {
        <M as Mangle>::write(self, buf)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SeqId(u32);

const ALPHA: [u8; 36] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E', b'F',
    b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R', b'S', b'T', b'U', b'V',
    b'W', b'X', b'Y', b'Z',
];

impl Mangle for SeqId {
    fn write<W: core::fmt::Write>(&self, buf: &mut W) -> core::fmt::Result {
        let mut nbuf = [0; 7];
        let mut cursor = 7;
        let mut val = self.0;

        loop {
            cursor -= 1;
            let char = val % 36;
            val /= 36;
            nbuf[cursor] = ALPHA[char as usize];
            if val == 0 {
                break;
            }
        }

        buf.write_str(unsafe { core::str::from_utf8_unchecked(&nbuf[cursor..]) })
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum Substitution {
    St,
    Sn(Option<SeqId>),
}

impl Mangle for Substitution {
    fn write<W: core::fmt::Write>(&self, buf: &mut W) -> core::fmt::Result {
        match self {
            Substitution::St => buf.write_str("St"),
            Substitution::Sn(n) => {
                buf.write_str("S")?;
                if let Some(n) = n {
                    n.write(buf)?;
                }
                buf.write_str("_")
            }
        }
    }
}

impl Mangle for Symbol {
    fn write<W: core::fmt::Write>(&self, buf: &mut W) -> core::fmt::Result {
        buf.write_fmt(format_args!("{}{}", self.len(), self))
    }
}

pub struct MangleEngine<'a> {
    submap: HashMap<&'a [Symbol], Substitution>,
    next_sub: Option<u32>,
}

impl<'a> MangleEngine<'a> {
    pub fn new() -> Self {
        let mut submap = HashMap::new();
        submap.insert(&[interning::STD] as &[Symbol], Substitution::St);

        Self {
            submap,
            next_sub: None,
        }
    }

    fn mangle_rest<W: core::fmt::Write>(
        &mut self,
        sub: Option<Substitution>,
        x: &'a [Symbol],
        writer: &mut W,
    ) -> core::fmt::Result {
        match (sub, x) {
            (Some(sub), []) => sub.write(writer),
            (None, [x]) => x.write(writer),
            (Some(sub), rest) => {
                writer.write_str("N")?;
                sub.write(writer)?;

                for x in rest {
                    x.write(writer)?;
                }
                writer.write_str("E")
            }
            (None, st) => {
                writer.write_str("N")?;

                for x in st {
                    x.write(writer)?;
                }
                writer.write_str("E")
            }
        }
    }

    pub fn mangle<W: core::fmt::Write>(
        &mut self,
        x: &'a [Symbol],
        writer: &mut W,
    ) -> core::fmt::Result {
        writer.write_str("_Z")?;
        match x {
            [interning::STD, a] => {
                writer.write_str("St")?;
                a.write(writer)
            }
            symbols => {
                let mut insert_subs = Vec::new();
                let mut found_sub = None;
                let mut suffix = symbols;
                for x in (0..symbols.len()).rev() {
                    let prefix = &symbols[..x];

                    if let Some(sub) = self.submap.get(prefix) {
                        found_sub = Some(*sub);
                        suffix = &symbols[x..];
                        break;
                    }
                    insert_subs.push(prefix);
                }

                for sub in insert_subs.into_iter().rev() {
                    let mut id = match &mut self.next_sub {
                        None => {
                            self.next_sub = Some(0);
                            None
                        }
                        Some(x) => Some(SeqId(x.fetch_increment())),
                    };
                    self.submap.insert(sub, Substitution::Sn(id));
                }
                self.mangle_rest(found_sub, suffix, writer)
            }
        }
    }
}
