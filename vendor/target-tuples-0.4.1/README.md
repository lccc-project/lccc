# rust target-tuples

Implements parsing of target tuples. Additionally provides a small program that implements config.sub. 

## Copyright

Copyright (C) 2020 Connor Horman

This program is dual-licensed under the terms of the MIT and Apache v2 licenses. 
See <LICENSE-MIT> and <LICENSE-APACHE> for details. 

Persuant to the Apache v2 License, if you contribute to this software,
 you agree to allow the copyright holder to dual-license those contributions as above.


## Building

This library and it's associated programs can be built using cargo. 

Additionally, it can be configured and built by running the configure script present in this repo with a Bourne-compatible shell. The standard options are available for the configure scripts. 

## Semver Policy

The MSRV for rust-target-tuples is rust 1.38. Increases to this value represents a semver major change.

This library does not guarantee the output of any of it's string parsing methods, or the canonical name of any component. Changes to both parsing and canonicalization may happen in a minor release. Additional component values may also be added in a minor release. 




