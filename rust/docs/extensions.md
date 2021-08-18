# LCRust Extensions

This document lists the extensions to the rust language made by the lccc rust frontend. Many of these extensions are under feature flags,
 and many are internal and subject to change without notice.

## ABI

Subject to control by frontend options, the lccc rust frontend guarantees the use of the LCRust ABI version 0, or a later version. 
This abi provides guarantees about mangling for crates, layout of types, and other unspecified details of rust's abi. 

The ABI is documented [here](abi/v0.md).

The abi version is controlled by the `-Z abi-version` flag, additionally, crates can control the layout of types defined by that crate using the `-Z repr-rust-layout` flag, the values for which are `abi`, which lays out repr(Rust) types according to the abi; `c`, which layouts out repr(Rust) types the same as repr(C) types; and `randomize`, which randomizes the layout of rust types by randomly reordering fields, inserting additional internal and trailing padding. 



