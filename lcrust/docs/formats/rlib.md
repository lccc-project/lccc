# LCCC Rust Library Format

The Rust Manifest File Format is a format that provides a structural description of the contents of an rlib, or of a module.

An rlib produced by lccc is a standard static library for the compilation that contains at least one Rust Manifest file, present at the path `.rmanifest`. 
It may contain other rust manifests, depicting modules within the crate.

Additionally, many rlibs produced by lccc will contain XIR Manfiest files, particularily the standard `.xmanifest`. These will appear whenever xir files are contained within the archive (bit 8 of the `file_contents` header field), which occurs when:
- Generic or inline definitions are provided within the file,
- The crate is compiled with LTO enabled (including mixed LTO), or
- The generation of an xir manifest is explicitly requested with the compilation option `-Z archive-include=xmanfiest`(rustc CLI) or `-fadd-file=xmanifest` (gcc CLI) when using `--crate-type rlib` (rustc CLI) or `-flinker-output=static-with-rmanifest` (gcc CLI).

The default name for rust libs is `lib<crate-name>.rlib`. If the `-Z crate-abi-version=<ver>`option is set, then it is appended to the name, following a period.

As an example, the file name for `core` version 1.0.0 is `libcore.rlib.1.0.0`. 

rlibs may be compressed using any of the following methods:
1. gzip
2. xz
3. lzma
4. zstd


## Rust Library Manfiest

### Header

The Rust Manifest Format starts with a 32-byte header which is defined as follows:

```rust
#[repr(C,align(8))] // Note: The file is always present at offset 0, which is well-aligned. This is a hint to show that the file can be copied from memory that is aligned to 8 bytes
pub struct RManifestHeader{
    magic: [u8;4],
    format_ver: [u8;2],
    order: u16,
    abi_version: i64,
    file_contents: u32,
    stroff: u32,
    croff: u32,
    reftaboff: u32
}
```

The magic field shall be set to precisely the bytes `[0xFE, 0xEF, 0x52, 0x4D]`.

The format_ver field shall be set as follows:
- The first byte of the field shall be the major version of the format, minus 1. 
- The second byte of the field shall be the exact value of the minor version of the format.

The current format version is 1.0, and shall be encoded as the bytes `[0x00, 0x00]`.

The order shall be set to the value `0x1234`. This field is used to determine the byte order used to interpret the rest of the file. 
[Note: Implementations that parse this format should accept both byte orders. This may cause it to read the value `0x3412` for this file. If it reads that value,
 the bytes of every multibyte primitive needs to be interpreted in reserve order of the native byte order.]

The abi_version field shall either be the version of the LCRust abi the crate was compiled for, or a negative value which indicates the crate was compiled with randomized layout mode, and the seed is the least significant 63 bits of the field.

file_contents shall be set as a bitfield, to indicate the kinds of files present in the rlib that contains this manifest. If the rmanifest file is for a module (and not the top level crate module), is produced standalone, or otherwise does not contain files or doesn't know what files it contains, the field should be blank
 The bits of file_contents are as follows:
- 0x00000001: The rlib contains plain object files for the target.
- 0x00000002: The rlib contains lccc rust macro definitions
- 0x00000004: The rlib contains other (non-root) rmanifest files
- 0x00000008: The rlib contains rust source files
- 0x00000100: The rlib contains xir bytecode files.
- 0x00000200: The rlib contains xir manfiest files.
- 0x00008000: The rlib contains other, embedded rlib files.
- 0x00010000-0x00800000: The rlib contains other, compiler specific files. The compiler producing the rlib may assign an arbitrary meaning to these bits and to the contents of such files.
- 0x1000000: The rlib contains gzip compressed files. Those files, and their content types, are described in an archive file called `.gz.rmanifest`.
- 0x2000000: The rlib contains xz compressed files. Those files, and their content types, are described in an archive file called `.xz.rmanifest`.
- 0x4000000: The rlib contains lzma compressed files. Those files, and their content types, are described in an archive file called `.lzma.rmanifest`.
- 0x8000000: The rlib contains zstd compressed files. Those files, and their content types, are described in an archive file called `.zstd.rmanifest`.

