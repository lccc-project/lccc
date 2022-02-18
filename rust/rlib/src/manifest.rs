use bytemuck::{Pod, Zeroable};

#[derive(Copy, Clone, Hash, Zeroable, Pod)]
#[repr(C, align(8))] // Note: The file is always present at offset 0, which is well-aligned. This is a hint to show that the file can be copied from memory that is aligned to 8 bytes
pub struct RManifestHeader {
    pub magic: [u8; 4],
    pub format_ver: [u8; 2],
    pub order: u16,
    pub abi_version: i64,
    pub file_contents: u32,
    pub stroff: u32,
    pub croff: u32,
    pub reftaboff: u32,
}

impl RManifestHeader {
    #[must_use]
    pub const fn swap_bytes(self) -> Self {
        Self {
            magic: self.magic,
            format_ver: self.format_ver,
            order: self.order.swap_bytes(),
            abi_version: self.abi_version.swap_bytes(),
            file_contents: self.file_contents.swap_bytes(),
            stroff: self.stroff.swap_bytes(),
            croff: self.croff.swap_bytes(),
            reftaboff: self.reftaboff.swap_bytes(),
        }
    }
}

pub const ORDER: u16 = 0xAABB;
pub const ORDER_SWAPPED: u16 = 0xBBAA;

pub mod file_contents {
    /// Target-specific Object Files
    pub const OBJECT: u32 = 0x0000_0001;

    /// Macro Definition Files
    pub const MACRO: u32 = 0x0000_0002;

    /// Additional Manifest files
    pub const MANIFEST: u32 = 0x0000_0004;

    /// Rust Source Files
    pub const RSRC: u32 = 0x0000_0008;

    /// Embedded rlib files
    pub const RLIB: u32 = 0x0000_0010;

    /// Implementation-specific file type 1
    pub const IMPLSPEC1: u32 = 0x0000_0100;

    /// Implementation-specific file type 2
    pub const IMPLSPEC2: u32 = 0x0000_0200;

    /// Implementation-specific file type 3
    pub const IMPLSPEC3: u32 = 0x0000_0400;

    /// Implementation-specific file type 4
    pub const IMPLSPEC4: u32 = 0x0000_0800;

    /// Implementation-specific file type 5
    pub const IMPLSPEC5: u32 = 0x0000_1000;

    /// Implementation-specific file type 6
    pub const IMPLSPEC6: u32 = 0x0000_2000;

    /// Implementation-specific file type 7
    pub const IMPLSPEC7: u32 = 0x0000_4000;

    /// Implementation-specific file type 8
    pub const IMPLSPEC8: u32 = 0x0000_8000;

    /// Implementation-specific file type 9
    pub const IMPLSPEC9: u32 = 0x0001_0000;

    /// Implementation-specific file type 10
    pub const IMPLSPEC10: u32 = 0x0002_0000;

    /// Implementation-specific file type 11
    pub const IMPLSPEC11: u32 = 0x0004_0000;

    /// Implementation-specific file type 12
    pub const IMPLSPEC12: u32 = 0x0008_0000;

    /// Implementation-specific file type 13
    pub const IMPLSPEC13: u32 = 0x0010_0000;

    /// Implemenation-specific file type 14
    pub const IMPLSPEC14: u32 = 0x0020_0000;

    /// Implementation-specific file type 15
    pub const IMPLSPEC15: u32 = 0x0040_0000;

    /// Implementation-specific file type 16
    pub const IMPLSPEC16: u32 = 0x0080_0000;

    /// GZip Compressed Files
    #[allow(clippy::doc_markdown)]
    pub const COMPGZ: u32 = 0x1000_0000;

    /// XZip Compressed Files
    #[allow(clippy::doc_markdown)]
    pub const COMPXZ: u32 = 0x2000_0000;

    /// LZMA Compressed Files
    pub const COMPLZMA: u32 = 0x4000_0000;

    /// ZStd Compressed Files
    #[allow(clippy::doc_markdown)]
    pub const COMPZSTD: u32 = 0x8000_0000;
}
