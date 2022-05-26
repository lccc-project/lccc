# Compiled Rust Macro Language

Embedded in rlibs are files in the Compiled Rust Macro Language (CRML) file format, described herein.

These files contain a compressed binary description of the expansion of rust macros.

## Header

```rust
#[repr(C,align(16))]
pub struct Header{
    magic: [u8;4],
    version: [u8;2],
    order: [u8;2],
    hygiene_table_size: u32,
    expansion_size: u32,
}
```

magic shall be set to the value `*b"\xC0RML`. `version` and `order` have the same meaning as in the rmanifest format.

`hygine_table_size` and `expansion_size` are the sizes (in elements) of the respective tables in the file

## Hygiene Table

The hygine table consists of a number of 16-byte elements defined as follows:
```rust
#[repr(C,align(16))]
pub struct HygieneEntry{
    pub crate_id: u64,
    pub xref_index: u32,
    pub hygiene_mode: u16,
    pub flags: u16,
}
```

The first entry in the Hygiene table has all fields set to `0`. This is the null hygiene and acts as a sentinel value for hygiene references

`crate_id` is set to the `crate_id` field of the crate header for the crate that contains the token. This is usually the same as the current crate, but may differ if an interior macro is inlined into a containing macro.
`xref_index` is set to the entry in the cross-reference table of the current crate that refers to the macro the token originates from.

The lower 12 bits of `hygiene_mode` are set to the edition the token originates in, with the same value meanings as the `crate_edition` field of the crate header in the manifest format. 
The upper 4 bits are set to the hygiene mode, for which the values are given as follows:
* `0`: Call-site only (no hygiene)
* `1`: Mixed-site (macro_rules hygiene)
* `2`: Def-site only (`macro`s 2.0 hygiene)
* `3`: No globals (No access to names outside of the expansion)
* Other values are reserved.

The `flags` field are set as follows:
* NOEDITION (0x00000001): If set, The Edition field of `hygiene_mode` is validated, then subsequently ignored for hygiene purposes - the edition of the expansion site determines token validity
* LINT_EXPANSION (0x00000002): If set, the expansion should be linted.
* INLINED (0x80000000): If set, this token was expanded from a nested macro call within the definition. The negation of this flag is a short-circuit test for `crate_id==current_crate.crate_id`

## Expansion Entries

The expansion of the macro immediately follows the last entry in the hygiene table.
Each Expansion Entry is a dynamically sized value of the following enum (Only as many bytes as are necessary to encode the value and pad up to 8 bytes are used for each entry):
```rust
#[repr(u64,/*see above*/,align(8))]
pub enum ExpansionEntry{
    BareToken{hygiene: u32, size: u16, tok: str} = 0,
    CrateRoot{crateref: u32} = 1,
    LitDollar = 2,
    Group{c: char,count: u32} = 3,
    Repetition{count: u32, mode: u16, size: u16, tok: str} = 4,
    Interpolation{mode: u32, ref_idx: u32} =5,
    EndOfExpansion = 6,
}
```

`BareToken` is a single rust token: Either a literal, identifier, keyword, lifetime, or sigil. `hygiene` is the index in the hygiene table for the token. `size` is the number of bytes in the token, and `tok` is the entire lexical token as a UTF-8 string with `size` length.  
`CrateRoot` is the `$crate` specal metavariable. `crateref` is the index in the hygiene table for the `$crate` reference.  
`LitDollar` is the `$$` special expansion. Note that, rarely, a `$` token may appear as `BareToken` instead (for example, due to the macro definition being expanded from a macro expansion) 
`Group` is a raw group expansion. `c` is the character of the leading group delimeter. For None-delimited groups, a null character is used. `count` is the number of following `ExpansionEntries` that are nested within the group.  
`Repetition` is a `$()*`, `$()?`, or `$()+` expansion. `count` is the number of following `ExpansionEntries` that are nested within this expansion. `mode` is either MODE_OPTION (0), or MODE_REPEAT (1) denoting `?` and `*` modes respectively, and other values are reserved. `+` mode uses `MODE_REPEAT`. `size` is the length of the delimiter token, or `0` if there is no delimiter token. `tok` is the entire lexical delimiter token that appears between individual expansions of the `Repetition`, encoded as a UTF-8 string with `size` length.  
`Interpolation` is a metavariable interpolation. `idx_ref` is the index in the macro's definition that refers to the metavariable to apply. `mode` is given as follows:
* 0: Verbaitim (token) expansion
* 1: Ignore (no expansion).
* 2: Count Repetition.
* 3: Count Nests.
* Other values are reserved

`EndOfExpansion` occurs in arms of `macro_rules!` macros to indicate that the current expansion is complete. Following expansion entries are referred to by offsets for different macro arms. [Note: It is not required that `EndOfExpansion` macros occur within the expansion of an arm of a `macro_rules` definition, nor is it required that they only appear in such expansions]

