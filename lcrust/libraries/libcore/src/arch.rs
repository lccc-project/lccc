#[cfg(target_arch = "x86")]
pub mod x86;

#[cfg(target_arch = "clever")]
#[unstable(feature = "lccc_core_arch_clever")]
pub mod clever;
