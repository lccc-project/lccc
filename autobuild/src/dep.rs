use core::convert::TryFrom;
use std::fs::File;

use serde::{Deserialize, Serialize};

use itertools::Itertools as _;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileHash([u8; 32]);

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
                                let mut mask = (hi << 5) & 0xF0 | (lo << 1) & 0xF;

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
