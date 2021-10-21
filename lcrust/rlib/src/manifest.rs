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
    pub const OBJECT: u32 = 0x00000001;

    /// Macro Definition Files
    pub const MACRO: u32 = 0x00000002;

    /// Additional Manifest files
    pub const MANIFEST: u32 = 0x00000004;

    /// Rust Source Files
    pub const RSRC: u32 = 0x00000008;

    /// Embedded rlib files
    pub const RLIB: u32 = 0x00000010;

    /// Implementation-specific file type 1
    pub const IMPLSPEC1: u32 = 0x00000100;

    /// Implementation-specific file type 2
    pub const IMPLSPEC2: u32 = 0x00000200;

    /// Implementation-specific file type 3
    pub const IMPLSPEC3: u32 = 0x00000400;

    /// Implementation-specific file type 4
    pub const IMPLSPEC4: u32 = 0x00000800;

    /// Implementation-specific file type 5
    pub const IMPLSPEC5: u32 = 0x00001000;

    /// Implementation-specific file type 6
    pub const IMPLSPEC6: u32 = 0x00002000;

    /// Implementation-specific file type 7
    pub const IMPLSPEC7: u32 = 0x00004000;

    /// Implementation-specific file type 8
    pub const IMPLSPEC8: u32 = 0x00008000;

    /// Implementation-specific file type 9
    pub const IMPLSPEC9: u32 = 0x00010000;

    /// Implementation-specific file type 10
    pub const IMPLSPEC10: u32 = 0x00020000;

    /// Implementation-specific file type 11
    pub const IMPLSPEC11: u32 = 0x00040000;

    /// Implementation-specific file type 12
    pub const IMPLSPEC12: u32 = 0x00080000;

    /// Implementation-specific file type 13
    pub const IMPLSPEC13: u32 = 0x00100000;

    /// Implemenation-specific file type 14
    pub const IMPLSPEC14: u32 = 0x00200000;

    /// Implementation-specific file type 15
    pub const IMPLSPEC15: u32 = 0x00400000;

    /// Implementation-specific file type 16
    pub const IMPLSPEC16: u32 = 0x00800000;

    /// GZip Compressed Files
    pub const COMPGZ: u32 = 0x10000000;

    /// XZip Compressed Files
    pub const COMPXZ: u32 = 0x20000000;

    /// LZMA Compressed Files
    pub const COMPLZMA: u32 = 0x40000000;

    /// ZStd Compressed Files
    pub const COMPZSTD: u32 = 0x80000000;
}
