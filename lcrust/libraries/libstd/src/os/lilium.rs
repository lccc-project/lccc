#![unstable(feature = "lccc_os_lilium")]

#[repr(C, align(16))]
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct OsUuid {
    pub lo: u64,
    pub hi: u64,
}
