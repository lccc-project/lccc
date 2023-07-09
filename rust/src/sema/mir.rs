#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct RegionId(u32);

impl core::fmt::Display for RegionId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("'{}", self.0))
    }
}

impl core::fmt::Debug for RegionId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("'{}", self.0))
    }
}
