use super::{sealed::Sealed, MetadataFor};
use crate::Block;

use xlang_abi::prelude::v1::*;

#[derive(Clone, Debug)]
pub struct ReachableFrom {
    pub reachable: Vec<u32>,
}

impl core::fmt::Display for ReachableFrom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut sep = "";

        f.write_str("(")?;

        for elem in &self.reachable {
            f.write_str(sep)?;
            sep = ", ";
            f.write_str("@")?;
            elem.fmt(f)?;
        }

        f.write_str(")")
    }
}

impl Sealed for ReachableFrom {}

impl MetadataFor<Block> for ReachableFrom {
    const TAG: StringView<'static> = StringView::new("reachable");
}
