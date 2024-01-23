use std::{
    fs,
    io::{self, Read as _},
    path::Path,
};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct FileHash([u8; 32]);

impl FileHash {
    pub const ZERO: FileHash = FileHash([0; 32]);
}

pub trait FileHasher {
    type Output: AsRef<[u8]> + 'static;

    /// The size of blocks given to the hash function
    /// It is a logic error if this is not a power of two (but the result is not undefined behaviour)
    const BLOCK_SIZE: usize;

    fn update(&mut self, buf: &[u8]);

    fn do_final(self, buf: &[u8]) -> Self::Output;
}

pub struct HashingReader<R, S> {
    buf: Vec<u8>,
    state: S,
    inner: R,
}

impl<R, S> HashingReader<R, S> {
    pub const fn new(state: S, inner: R) -> Self {
        Self {
            state,
            inner,
            buf: Vec::new(),
        }
    }

    pub fn into_inner(self) -> R {
        self.inner
    }
}

impl<R, S: FileHasher> HashingReader<R, S> {
    pub fn init(&mut self, k: FileHash) {
        self.buf.clear();
        self.buf.reserve(S::BLOCK_SIZE);

        if S::BLOCK_SIZE < 32 {
            for v in k.0.chunks_exact(S::BLOCK_SIZE) {
                self.state.update(v);
            }
        } else {
            self.buf.extend_from_slice(&k.0);
        }
    }
    pub fn finish(self) -> FileHash {
        let mut output = [0; 32];

        let val = self.state.do_final(&self.buf);

        let val = val.as_ref();

        let len = val.len().min(32);

        output[..len].copy_from_slice(&val[..len]);

        FileHash(output)
    }
}

impl<R: io::Read, S: FileHasher> io::Read for HashingReader<R, S> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        let read = self.inner.read(buf)?;

        let mut bytes = &buf[..read];

        let buf_pos = self.buf.len();

        if bytes.len() > (S::BLOCK_SIZE - buf_pos) {
            if buf_pos != 0 {
                let (l, r) = bytes.split_at(S::BLOCK_SIZE - buf_pos);
                self.buf.extend_from_slice(l);
                bytes = r;
                self.state.update(&self.buf);
                self.buf.clear();
            }

            let ce = bytes.chunks_exact(S::BLOCK_SIZE);
            self.buf.extend_from_slice(ce.remainder());
            for chunk in ce {
                self.state.update(chunk);
            }
        } else {
            self.buf.extend_from_slice(bytes);
            self.buf.resize(S::BLOCK_SIZE, 0);
            self.state.update(&self.buf);
            self.buf.clear();
        }

        Ok(read)
    }
}

pub mod sha;

pub fn hash_file<S: FileHasher, P: AsRef<Path>>(
    path: P,
    hasher: S,
    key: FileHash,
) -> io::Result<FileHash> {
    let mut buf = vec![0; S::BLOCK_SIZE];
    let file = fs::File::open(path)?;

    let mut reader = HashingReader::new(hasher, file);

    reader.init(key);

    loop {
        if reader.read(&mut buf)? == 0 {
            break;
        }
    }

    Ok(reader.finish())
}

const ALPHA: [u8; 16] = [
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c', b'd', b'e', b'f',
];

impl Serialize for FileHash {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut string = Vec::<u8>::with_capacity(64);

        for b in self.0.iter().rev() {
            string.push(ALPHA[(b >> 4) as usize]);
            string.push(ALPHA[(b & 0xf) as usize]);
        }

        // SAFETY: The Vec is entirely ASCII
        let string = unsafe { String::from_utf8_unchecked(string) };

        serializer.serialize_str(&string)
    }
}

impl<'de> Deserialize<'de> for FileHash {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        pub struct HexStringVisitor;
        impl<'de> serde::de::Visitor<'de> for HexStringVisitor {
            type Value = [u8; 32];

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a 32-byte value as a hex string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v.len() != 64 {
                    return Err(E::invalid_value(serde::de::Unexpected::Str(v), &self));
                }
                let mut bytes = [0u8; 32];
                let mut arrays = v
                    .as_bytes()
                    .chunks_exact(2)
                    .map(|octet| {
                        let hi = octet[0];
                        let lo = octet[1];

                        match (hi, lo) {
                            (
                                hi @ (b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f'),
                                lo @ (b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f'),
                            ) => {
                                let mut mask = (hi << 1) & 0xF0 | (lo >> 3) & 0xF;

                                mask |= mask >> 1;
                                mask |= mask >> 2;

                                let val = (hi << 4) & 0xF0 | lo & 0x0F;

                                let val = val + (0xAA & mask);

                                Ok(val)
                            }
                            _ => Err(E::invalid_value(serde::de::Unexpected::Str(v), &self)),
                        }
                    })
                    .zip(bytes.iter_mut().rev())
                    .map(|(b, r)| Ok(*r = b?))
                    .collect::<Result<(), E>>()?;

                Ok(bytes)
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_str(&v)
            }
        }

        let bytes = deserializer.deserialize_str(HexStringVisitor)?;

        Ok(FileHash(bytes))
    }
}
