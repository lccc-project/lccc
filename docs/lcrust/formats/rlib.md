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

The order shall be set to the value `0xAABB`. This field is used to determine the byte order used to interpret the rest of the file. 
[Note: Implementations that parse this format should accept both byte orders. This may cause it to read the value `0xBBAA` for this file. If it reads that value,
 the bytes of every multibyte primitive needs to be interpreted in reserve order of the native byte order.]

The abi_version field shall either be the version of the LCRust abi the crate was compiled for, or a negative value which indicates the crate was compiled with randomized layout mode, and the seed is the least significant 63 bits of the field.

file_contents shall be set as a bitfield, to indicate the kinds of files present in the rlib that contains this manifest. If the rmanifest file is for a module (and not the top level crate module), is produced standalone, or otherwise does not contain files or doesn't know what files it contains, the field should be blank
 The bits of file_contents are as follows:
- 0x00000001: The rlib contains plain object files for the target.
- 0x00000002: The rlib contains lccc rust macro definitions
- 0x00000004: The rlib contains other (non-root) rmanifest files
- 0x00000008: The rlib contains rust source files
- 0x00000010: The rlib contains other, embedded rlib files.
- 0x00000100-0x00800000: The rlib contains other, compiler specific files. The compiler producing the rlib may assign an arbitrary meaning to these bits and to the contents of such files. 
    - It is RECOMMENDED that compilers producing such files define a manifest file containing information about these compiler-specific files, and refuse to interpret rlib files that set any of these bits and do not have a proper manifest present. 
    - [Note: The lccc compiler uses bits 0x00000100 and 0x00000200 to indicate the presence of xir bytecode and manifest files respectively, this is used for generic and inlineable functions, and when LTO is produced. Additionally, the lccc compiler uses bits 0x00001000 to indicate the presence of the language item table in the rlib, under a file called `.rlang`]
- 0x10000000: The rlib contains gzip compressed files. Those files, and their content types, are described in an archive file called `.gz.rmanifest`.
- 0x20000000: The rlib contains xz compressed files. Those files, and their content types, are described in an archive file called `.xz.rmanifest`.
- 0x40000000: The rlib contains lzma compressed files. Those files, and their content types, are described in an archive file called `.lzma.rmanifest`.
- 0x80000000: The rlib contains zstd compressed files. Those files, and their content types, are described in an archive file called `.zstd.rmanifest`.

Other bits have undefined meanings, and implementations should not set those bits and should ignore them if they are present. They may be assigned meaning in a future version.

stroff specifies the offset in the file to the string table header, or 0 if the file contains no string table
croff specifies the offset in the file to the crate header, or 0 if the file contains no crate header.
reftaboff specifies the offset in the file to the reference table header, or 0 if the file contains no reference table.

There may be bytes in the file that are not part of any structure defined by this format (for example, padding inserted between structures to correctly align headers within the file). The content of these bytes is undefined, and programs reading rmanifest files shall ignore these bytes. 

### String Table

rmanifest files contain a table of interned string constants, which other parts of the file reference. These strings are encoded in UTF-8, and are Null-terminated. Entries referenced in the file may overlap, to allow optimizations where common suffixes can be contained in larger entries. A file may contain more than 1 string table, referenced by the `next` field of the previous String table's header. The maximum total size of all string tables, excluding the headers, is 2147483648 bytes (1<<31). If multiple string tables exist, references to entries in the string table shall occur as though there was a single string table in the file, that contains every byte of each string table in order by the `next` fields of each header directly concatenated (excluding the headers of each table). The first string table in a file is the one designated by the `stroff` field of the manifest header. 

offsets into the string table start at 0. The entry at offset zero shall be an empty string (that is, the first byte of the string table shall be a null terminator).

Each string table starts with a 8-byte header, which occurs at an offset in the file which is aligned to 8-bytes. The header is defined as follows

```rust
#[repr(C,align(8))]
pub struct StringTableHeader{
    extent: u32,
    next: u32,
}
```

The extent field is the length (in bytes) of the string table, the next field is the offset of the header for the next string table in the file from the *beginning* of the last byte of the string table, or 0 if there is no next string table [Note: using the beginning here instead of the end of the last byte in the string table, causes a string table that occurs immediately after the end of the current header to have an offset of 1. This allows such cases while designating `0` the sentinel for no remaining string tables].

If a string contained within the file is not terminated prior to the end of a particular string table, the behavior of the file parser is not specified. Implementations MUST produce files that do not violate this requirement.
[Note: This allows programs to parse the string table as sparse entities rather than contiguous entities. Programs are expected to behave in a reasonable manner if files are encountered that violate this requirement - for example, by rejecting them, terminating the string early, or continuing the string from subsequent files]


### Crate Information

Top level rmanifest files contain information about the compiled crate, such as the crate name, mangled name component, stability, and dependencies.

The crate information starts with a 48-byte header, that occurs at an offset in the file which is aligned to 16-bytes. The header is defined as follows:

```rust
#[repr(C,align(16))]
pub struct CrateHeader{
    crate_name: u32,
    mangled_name: u32,
    abi_ver_name: u32,
    links_off: i32,
    compiler_id: u32,
    crate_flags: u32,
    crateid: u64,
    stability: Stability,
    extra_off: i32,
}
```

The `crate_name` field is an offset into the file's string table which refers to a Null-terminated UTF-8 string that contains the name of the crate (set by the `--crate-name` option in the rustc CLI). 

The `mangled_name` field is an offset into the file's string table which refers to a Null-terminated UTF-8 string that contains the source-component of the mangled name, including any discriminator [Note: This is not the same as the mangled name of the crate in itanium form. In particular, it neither has a `_Z` prefix, nor is in `<length><name>` form as per the itanium spec. Rather it is the string that is included in the mangling of symbols defined by the crate. There is no requirement that this correspond to the `crate_name` in any meaningful way, though implementations should not provide a stable mechanism to set this to an arbitrary value]. 

The `abi_ver_name` is an offset into the file's string table, which refers to a Null-terminated UTF-8 which is either an empty string, or contains a valid version string in `<major>.<minor>.<revision>` format. If non-empty, this defines the abi version of the crate [note: this may not be the same as the crate version, and many crates will not set this field]. 

The `links_off` field is the (signed) offset from the beginning of the crate header in the file to the header of the crate's links table, or 0 if the file contains no `links` table.

The `compiler_id` field is an offset into the file's string table which refers to a Null-terminated UTF-8 string that is a meaningful, but implementation-defined representation of the compiler that produced the file. This field is provided for diagnostic purposes, and should be ignored by implementations.

The `crate_flags` field is the flags set for the crate, as follows:
- `no_std`  (0x00000001): The crate uses the `#[no_std]` crate-level attribute
- `no_core` (0x00000002): The crate uses the `#[no_core]` crate-level attribute

All other flags have undefined meaning. Implementations should not set those flags and should ignore them when reading. 


The `stability` field shall contain a value of the `Stability` enum, defined below, representing the stability of the crate.

The `extra_off` field shall contain a (signed) offset from the beginning of the crate header in the file to the header of the crate's extra information table, or 0 if the file contains no extra information table.


### Item Stability

The Stability type is an 12-byte enum that is well-aligned to 4 bytes. It is defined as follows:

```rust
#[repr(u32,align(4))]
#[non_exhaustive]
pub enum Stability{
    Stable{since: u32} = 0,
    Unstable{feature: u32, issue: u32} = 1,
    ImplicitCallStable{edition: u32} = 2,
    StableInEdition{edition: u32} = 3,
    RemovedInEdition{edition: u32} = 4,
    ConstStable{since: u32} = 5,
    ConstUnstable{feature: u32, issue: u32} = 6,
    ConstStableInEdition{edition: u32} = 7,
    ConstRemovedInEdition{edition: u32} = 8,
    SafeInEdition{edition: u32} = 9,
    UnsafeInEdition{edition: u32} = 10,
    SafeStable{since: u32} = 11,
    SafeUnstable{feature: u32, issue: u32} = 12,
}
```

Other variants are reserved and may be given meaning in future versions.
Only the `Stable`, `Unstable`, and `StableInEdition` variants are meaningful for the `stability` field of a crate header. 

`since`shall be an index in the string table for the file which is either an empty string or a version string which contains a rust language version, *major*.*minor*, which the version of the rust language the item was stabilized or was stabilized for use in `const` fns. 
`feature` shall be an index in the string table for the file which is the name of the feature it applies to.

`edition` for `StableInEdition`



### Extra Information

Each crate may contain additional, implementation-specific information, in the extra information table. Except for defined fields, the content of this table is undefined, and implementations that do not understand particular entries should ignore those entries. 

The Extra Information Table contains a 8-byte header, which is shall bet at an offset within the file that is aligned to 8 bytes. The Header is defined as follows:
```rust
#[repr(C,align(8))]
pub struct ExtraHeader{
    entries: u32,
    extent: u32
}
```

The `entries` field shall be the number of entries in the Extra Information Table. 
The `extent` field shall be the total number of bytes in the Extra Information Table, including the header.

Following the header is `extent` bytes which collectively makes up `entries` entries. Each entry has an 16-byte header, which shall be at an offset in the file which is aligned to 8 bytes, which is then followed by a number of bytes given by the header. Between each entry shall be the minimum number of additional padding bytes necessary such that the next header is at an offset aligned to 8 bytes within the file, and after the last entry, there shall be a minimum number of additional trailing padding bytes such that the total number of bytes in the Extra Information Table is a multiple of 8. The content of the padding bytes is undefined and shall be ignored. 

The header for each extra entry is defined as follows:
```rust
#[repr(C,align(8))]
pub struct ExtraEntryHeader{
    id: u32,
    len: u32,
    flags: u64
}
```

The `id` field shall be an offset in the file's string table, which refers to a null-terminated UTF-8 string that is the identifier for the type of the extra entry. The format of these strings is not specified, but should be unique between implementations. The interpretation for any particular entry is not specified, except as defined below. Any id that is a valid rust identifier may be assigned meaning in a future revision and is reserved for that purpose.

The `len` field shall be the total number of bytes of the entry, not including trailing padding, if any, but including the header. The remaining bytes of the entry, followed by any padding, trails the header.

The `flags` field specifies flags of the entry. The flags are as follows:
* required (0x0000000000000001): The extra entry is mandatory to correctly interpreting the manifest file, and an implementation that does not understand the entry with this flag set must issue an error when parsing the file.


Special types are defined by this specification, and have the meaning given below, including the content of the trailing bytes.

### Stability

```rust
#[repr(C,align(8))]
pub struct ExtraEntryStability{
    id: u32,
    len: u32,
    flags: u64,
    stability: Stability,
    pad: u32,
}
```

The `Stability` extra information entry provides additional stability information from the `stability` field in the crate header. 

The `id` field shall be an offset into the file's string table, which refers to a null-terminated UTF-8 String that is exactly equal to the string "Stability". 
The `len` field shall be the value 32. 
The `flags` field shall set the `REQUIRED` flag.

The `stability` field shall be a value of the `Stability` enum defined above. It is used in combination with the `stability` field in the crate header to define the stability of the crate.

The `pad` field is reserved as an extension to the `Stability` enum, shall be set to `0` and ignored.

### Crate Contents

```rust
#[repr(C,align(8))]
pub struct ExtraEntryContents{
    id: u32,
    len: u32,
    flags: u64,
    items: [ItemRef]
}
```

The `Contents` extra entry provides the list of items defined within the crate.

The `id` field shall be an offset into the file's string table, which refers to a null-terminated UTF-8 string that is exactly equal to the string "Contents".
The `len` field shall be the total number of bytes of the entry, not including trailing padding, if any, but including the header. The remaining bytes of the entry, followed by any padding, trails the header.
The `flags` field shall set the `REQUIRED` flag.

The `items` shall be an array of `ItemRef`s, given below. The length of the array is the length of the entire entry, minus the length of the header (16 bytes), divided by 32.

```rust
#[repr(C,align(8))]
pub struct ItemRef{
    pub ikind: u16,
}
