# Dylib Format

Dylib information is embedded within the platform-specific dynamic linking format, and uses the same format as an rlib manifest file, as well as macros and inline definitions.

How the information is encoded depends on the platform dynamic library format. If no dynamic libraries are used, then dylibs are unsupported on the platform and rlib format is used for all types.


## Elf 

Elf encodes the manifest file within a SHT_NOTE section

The root manifest is placed within a section called `.note.lcrust.rmanifest`.

The note name shall be `lcrscompat ` and use type `0`. Type `1` for `lcrust` is abi information (see the `abi` crate), type `2` is for macros, and type `3` is the language item table.

The description is the entire manifest file that would be contained within `.rmanifest`, with the following exceptions:
* The `sh_link` field contains the section number of the string table used for the manifest, and the strtab field of the manifest header shall be set to `0` (that is, the manifest contains no string table). Any item that requires a string uses the string table in `sh_link` instead. If no string table is used, then the null section, SHN_NULL, is acceptable.
* the `order` field of the manifest file shall match the byte order in the e_ident array (EI_DATA). That is, for an ELFDATA2LSB file, it must be `[BB AA]` and, for an ELFDATA2MSB File, it must be `[AA BB]`.
* In the cross reference table, where items reference other files in the archive by name, the field is repurposed to reference the name of a section in the file. `sh_info` contains a hash table for section names referenced by the manifest's cross-reference table.
* The section must be aligned to 8. The start of the description portion of the note is aligned to 8 bytes.
* Only a single manifest may be present within a single note section. Additional manifests, if any, must appear within their own section.


## Mach-O

Mach-O dynlibs encodes the manifest within an LC_NOTE command.

The root manifest