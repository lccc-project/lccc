# Dylib Format

Dylib information is embedded within the platform-specific dynamic linking format, and uses the same format as an rlib manifest file, as well as macros and inline definitions.

How the information is encoded depends on the platform dynamic library format. If no dynamic libraries are used, then dylibs are unsupported on the platform and rlib format is used for all types.

## Section Hash Table

The Cross-reference Section Hash Table is used by the Elf and Mach-O dylib formats. It contains a hashtable of section names to section references. In the case of Elf, this refers to the global section table. In the case of Mach-O, this refers to the sections within the `__LCRUST` segment.

The table consists of a header, followed by an array of 8-byte bucket entries, followed by an array of 8-byte section references.

The header is given by the following 8-byte structure:
```rust
#[repr(C,align(8))]
pub struct XRefSecHashHeader{
    numbuckets: u32,
    numentries: u32,
}
```

Where `numbuckets` is the number of hash bucket entries search by the hash key, and `numentries` is the number of total section references in the hashtable.  

The buckets are given by the following 8-byte structure:
```rust
#[repr(C,align(8))]
pub struct XRefSecBucket{
    offset: u32,
    sizehint: u32
}
```

Where `index` is the offset (in 8-byte chunks) into the hashtable that contains the first section entry in the bucket, and `sizehint` is a hint about the number of entries to search. The implementation shall handle this value being incorrect, but may offer significantly degraded performance if it does not match the actual number of entries.

The entries are given by the following 8-byte structure:

```rust
#[repr(C,align(8))]
pub struct XRefSecEntry{
    nameref: u32,
    secno: U32
}
```

Where `nameref` is the offset into the string table for the section's name and `secno` is the section number that corresponds to the name.

Before the first entry referenced by a bucket, and after all entries within a bucket, there shall be a null entry (nameref=0, secno=0). The `sizehint` of a bucket does not include this extra entry, and adjacent empty buckets can use the same null entry. 
There is no requirement that the bucket order match the entry order (for example, the first set of entries could be referred to by bucket 5, then the second set could be referred to by bucket 2).

The implementation should ensure that the number of buckets is a power of two, and no more than one third of the total entries ends up in a single bucket. Implementations shall handle a non-power of two bucket count, but may offer degraded performance for non-powers of two bucket count.

The hash algorithm to use is FNV-1a 32-bit of the section name, excluding the null terminator. The resulting 32-bit hash shall then be taken modulo the number of buckets, and, after adding 1, is the offset (in 8-byte chunks) into the hashtable of the bucket reference. 

## Elf 

Elf encodes the manifest file within a SHT_NOTE section

The root manifest is placed within a section called `.note.lcrust.rmanifest`. 

The note name shall be `lcrscompat ` and use type `0`. Type `1` for `lcrust` is abi information (see the `abi` crate), type `2` is for macros, type `3` is the language item table, and type `4` is the cross-reference section hash table.

The description is the entire manifest file that would be contained within `.rmanifest`, with the following exceptions:
* The `sh_link` field contains the section number of the string table used for the manifest, and the strtab field of the manifest header shall be set to `0` (that is, the manifest contains no string table). Any item that requires a string uses the string table in `sh_link` instead. If no string table is used, then the null section, SHN_NULL, is acceptable. The name `.strtab.lcrust.manifest` is recommended for the root manifest, though no requirement is placed upon the implementation to ensure this.
* the `order` field of the manifest file shall match the byte order in the e_ident array (EI_DATA). That is, for an ELFDATA2LSB file, it must be `[BB AA]` and, for an ELFDATA2MSB File, it must be `[AA BB]`.
* In the cross reference table, where items reference other files in the archive by name, the field is repurposed to reference the name of a section in the file. `sh_info` contains a hash table for section names referenced by the manifest's cross-reference table. The name `.note.lcrust.sechash` is recommended for this section used for the root manifest, though no requirement is placed on the implementation to ensure this.
* The section must be aligned to 8. The start of the description portion of the note is aligned to 8 bytes.
* Only a single manifest may be present within a single note section. Additional manifests, if any, must appear within their own section.

The cross-reference section hash table is present within a section of type SHT_NOTE with a name of `lcrscompat ` and type `4`.

## Mach-O

Mach-O dynlibs encodes the manifest within a `__LCRUST` segment. 

The root manifest is contained within a `__manifest` section, and the string table for the root manifest is contained within a `__strtab` section. The may be a `__sechash` section, which contains a hash table for section name references.

`__manifest` has type `S_REGULAR` and `__strtab` has type `S_CSTRING_LITERALS`. It is recommended that `maxprot` and `initprot` contain the values `0x01` (READ).

The contents of the section are the regular manifest file for a dylib with the following exceptions:
* The strtab in the manifest header is set to `0`, and the string table is externalized,
* the `order` field shall match the byte order of the Mach-O file
* In the cross reference table, whree items reference other files in the archive by name, the field is repurposed to reference the name of a section within the `__LCRUST` segment. The `__sechash` section contains a hashtable to use


