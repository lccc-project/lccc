#![allow(non_camel_case_types)]

#[cfg(doc)]
mod unspecified {
    pub type UnspecifiedInt = isize;
    pub type UnpsecifiedUInt = usize;
}

#[cfg(doc)]
/// An unspecified unsigned integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type uintptr = unspecified::UnspecifiedUInt;

#[cfg(doc)]
/// An unspecified signed integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type intptr = unspecified::UnspecifiedInt;

#[cfg(doc)]
/// An unspecified unsigned integer type which matches the size of the C typedef `size_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type size_t = unspecified::UnspecifiedUInt;

#[cfg(doc)]
/// An unspecified signed integer type which matches the size of the C typedef `ptrdiff_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type ptrdiff_t = unspecified::UnspecifiedInt;

#[cfg(all(not(doc), target_pointer_width = "16"))]
/// An unspecified unsigned integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type uintptr = u16;

#[cfg(all(not(doc), target_pointer_width = "32"))]
/// An unspecified unsigned integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type uintptr = u32;

#[cfg(all(not(doc), target_pointer_width = "64"))]
/// An unspecified unsigned integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type uintptr = u64;

#[cfg(all(not(doc), target_pointer_width = "16"))]
/// An unspecified signed integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type intptr = i16;

#[cfg(all(not(doc), target_pointer_width = "32"))]
/// An unspecified signed integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type intptr = i32;

#[cfg(all(not(doc), target_pointer_width = "64"))]
/// An unspecified signed integer type which is the same size as a pointer on the current architecture
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type intptr = i64;

#[cfg(all(not(doc), target_pointer_width = "16"))]
/// An unspecified unsigned integer type which matches the size of the C typedef `size_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type size_t = u16;

#[cfg(all(not(doc), target_pointer_width = "32"))]
/// An unspecified unsigned integer type which matches the size of the C typedef `size_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type size_t = u32;

#[cfg(all(not(doc), target_pointer_width = "64"))]
/// An unspecified unsigned integer type which matches the size of the C typedef `size_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `usize`
pub type size_t = u64;

#[cfg(all(not(doc), target_pointer_width = "16"))]
/// An unspecified signed integer type which matches the size of the C typedef `ptrdiff_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type ptrdiff_t = i16;

#[cfg(all(not(doc), target_pointer_width = "32"))]
/// An unspecified signed integer type which matches the size of the C typedef `ptrdiff_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type ptrdiff_t = i32;

#[cfg(all(not(doc), target_pointer_width = "64"))]
/// An unspecified signed integer type which matches the size of the C typedef `ptrdiff_t`
/// Note: See [Lang Team MCP#125](https://github.com/rust-lang/lang-team/issues/125) for why this type is not `isize`
pub type ptrdiff_t = i64;
