use xlang_abi::string::{CowStr, StringView};

use crate::File;

use crate::meta::MetadataFor;

#[repr(C)]
#[derive(Clone, Debug)]
pub struct RequiredMetadata {
    pub meta: Vec<RequiresEntry>,
}

impl super::sealed::Sealed for RequiredMetadata {}

impl MetadataFor<File> for RequiredMetadata {
    const TAG: StringView<'static> = StringView::new("requires");
}

impl core::fmt::Display for RequiredMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";

        for m in &self.meta {
            f.write_str(sep)?;
            sep = ", ";
            m.fmt(f)?;
        }
        Ok(())
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default, Copy, Clone, Debug, Hash, PartialEq, Eq)]
    pub struct RequiredMetadataContext : u32{
        /// Any plugin that views the incoming IR *must* not process a file that sets the metadata type it doesn't recognize
        /// (Metadata is mandatory for interpreting the file at all)
        const PROCESSOR   = 0x00000001;
        /// An output plugin (code generator) must not process a file that sets the metadata type it doesn't recognize
        /// (Metadata is mandatory for emitting code from the metadata)
        const OUTPUT      = 0x00000002;
        /// Any plugin that mutates the incoming IR in place must not process a file that sets the metadata type it doesn't recognize
        const TRANSFORMER = 0x00000004;
        /// Any transformer plugin that is an optimizer must not process a file that sets the metadata type it doesn't recognize
        const OPTIMIZER   = 0x00000008;
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default, Copy, Clone, Debug, Hash, PartialEq, Eq)]
    pub struct RequiredMetadataPosition : u128{
        const FILE = 0x0001;
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct RequiresEntry {
    pub key: CowStr<'static>,
    pub ctx: RequiredMetadataContext,
    pub positions: RequiredMetadataPosition,
}

impl core::fmt::Display for RequiresEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.key.as_str())?;
        f.write_str(" ")?;

        for (name, _) in self.ctx.iter_names() {
            f.write_str(&name.to_ascii_lowercase())?;
            f.write_str(" ")?;
        }

        let mut sep = "";
        f.write_str("{")?;
        for (pos, _) in self.positions.iter_names() {
            f.write_str(sep)?;
            sep = ", ";
            f.write_str(&pos.to_ascii_lowercase())?;
        }

        f.write_str("}")
    }
}
