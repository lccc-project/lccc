use std::hash::{Hash, Hasher};

use xlang::{
    abi::hash::XLangHasher,
    ir::StringEncoding,
    prelude::v1::{format, HashMap, Pair, String, Vec},
};

/// A string encoding (either a byte string, or some encoding valid for XLang)
#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
pub enum Encoding {
    /// A byte String that does not contain UTF-8 bytes internally
    Byte,
    /// An encoded String that contains UTF-8 bytes internally
    XLang(StringEncoding),
}

impl Encoding {
    /// Encodes bytes from utf8 (or raw bytes) into the given encoding
    ///
    /// # Panics
    /// Panics if self is not [`Encoding::Byte`], and `bytes` is not valid UTF-8 text
    pub fn encode_utf8(&self, bytes: &[u8]) -> Vec<u8> {
        match self {
            Self::Byte => Vec::from(bytes),
            Self::XLang(enc) => {
                let st = core::str::from_utf8(bytes).unwrap();
                match enc {
                    StringEncoding::Utf8 => Vec::from(bytes),
                    StringEncoding::Utf16BE => {
                        st.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()
                    }
                    StringEncoding::Utf32BE => {
                        st.chars().flat_map(|c| (c as u32).to_be_bytes()).collect()
                    }
                    StringEncoding::Utf16LE => {
                        st.encode_utf16().flat_map(|u| u.to_le_bytes()).collect()
                    }
                    StringEncoding::Utf32LE => {
                        st.chars().flat_map(|c| (c as u32).to_le_bytes()).collect()
                    }
                    StringEncoding::Wtf8 => Vec::from(bytes),
                    StringEncoding::Wtf16BE => {
                        st.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()
                    }
                    StringEncoding::Wtf16LE => {
                        st.encode_utf16().flat_map(|u| u.to_le_bytes()).collect()
                    }
                }
            }
        }
    }
}

impl std::fmt::Display for Encoding {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Byte => f.write_str("byte"),
            Self::XLang(StringEncoding::Utf8) => f.write_str("utf8"),
            Self::XLang(StringEncoding::Utf16BE) => f.write_str("utf16be"),
            Self::XLang(StringEncoding::Utf32BE) => f.write_str("utf32be"),
            Self::XLang(StringEncoding::Utf16LE) => f.write_str("utf16le"),
            Self::XLang(StringEncoding::Utf32LE) => f.write_str("utf32le"),
            Self::XLang(StringEncoding::Wtf8) => f.write_str("wtf8"),
            Self::XLang(StringEncoding::Wtf16BE) => f.write_str("wtf16be"),
            Self::XLang(StringEncoding::Wtf16LE) => f.write_str("wtf16le"),
        }
    }
}

/// A type for internalizing String literals within a particular translation unit
/// This allows mapping string literals to pointers into the [`StringMap`]
#[derive(Clone, Debug)]
pub struct StringMap {
    inner: HashMap<Encoding, StringInterner>,
}

impl Default for StringMap {
    fn default() -> Self {
        Self::new()
    }
}

impl StringMap {
    /// Construts a new [`StringMap`]
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    /// Obtains a symbol for the given string, interning the content if necessary
    pub fn get_string_symbol(&mut self, bytes: Vec<u8>, enc: Encoding) -> &str {
        let StringInterner { map, idx } =
            self.inner.get_or_insert_with_mut(enc, |_| StringInterner {
                map: HashMap::new(),
                idx: HashMap::new(),
            });

        map.get_or_insert_with_mut(bytes, |key| {
            let mut hasher = XLangHasher::new();
            key.hash(&mut hasher);
            let hash = hasher.finish();
            let st = if let Some(val) = idx.get_mut(&hash) {
                let st = format!(".{}", val);
                *val += 1;
                st
            } else {
                idx.insert(hash, 0);
                String::new()
            }; // Wish this was format_args!() not format!()

            format!(
                ".__string_literal.{}.{}.{}{}",
                enc,
                hash,
                xlang::version(),
                st
            )
        })
    }

    /// Enumerates all of the symbols in the file
    pub fn symbols(&self) -> impl Iterator<Item = (&str, &[u8])> {
        self.inner.iter().flat_map(|Pair(_, intern)| {
            intern
                .map
                .iter()
                .map(move |Pair(bytes, sym)| (&**sym, &**bytes))
        })
    }
}

#[derive(Clone, Debug, Hash)]
struct StringInterner {
    map: HashMap<Vec<u8>, String>,
    idx: HashMap<u64, u32>,
}
