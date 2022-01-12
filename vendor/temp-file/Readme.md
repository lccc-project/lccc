[![crates.io version](https://img.shields.io/crates/v/temp-file.svg)](https://crates.io/crates/temp-file)
[![license: Apache 2.0](https://gitlab.com/leonhard-llc/ops/-/raw/main/license-apache-2.0.svg)](https://gitlab.com/leonhard-llc/ops/-/raw/main/temp-file/LICENSE)
[![unsafe forbidden](https://gitlab.com/leonhard-llc/ops/-/raw/main/unsafe-forbidden.svg)](https://github.com/rust-secure-code/safety-dance/)
[![pipeline status](https://gitlab.com/leonhard-llc/ops/badges/main/pipeline.svg)](https://gitlab.com/leonhard-llc/ops/-/pipelines)

# temp-file

Provides a `TempFile` struct.

## Features
- Makes a file in a system temporary directory
- Deletes the file on drop
- Optional file name prefix
- Optional file contents
- Depends only on `std`
- `forbid(unsafe_code)`
- 100% test coverage

## Limitations
- Not security-hardened. See
  [Secure Programming for Linux and Unix HOWTO - 7.10. Avoid Race Conditions](https://tldp.org/HOWTO/Secure-Programs-HOWTO/avoid-race.html)
  and [`mkstemp`](https://linux.die.net/man/3/mkstemp).

## Alternatives
- [`tempfile`](https://crates.io/crates/tempfile)
  - Popular and mature
  - Supports some security-sensitive use cases
  - Contains `unsafe`, dependencies full of `unsafe`
  - Heavy dependencies (libc, winapi, rand, etc.)
- [`test-temp-file`](https://crates.io/crates/test-temp-file)
  - Depends on crates which contain `unsafe`
  - Incomplete documentation
- [`temp_file_name`](https://crates.io/crates/temp_file_name)
  - Does not delete file
  - Usage is not straightforward.  Missing example.
- [`mktemp`](https://crates.io/crates/mktemp)
  - Sets file mode 0600 on unix
  - Contains `unsafe`
  - No readme or online docs

## Related Crates
- [`temp-dir`](https://crates.io/crates/temp-dir)

## Example
```rust
let t = temp_file::with_contents(b"abc");
// Prints "/tmp/1a9b0".
println!("{:?}", t.path());
assert_eq!(
  "abc",
  std::fs::read_to_string(t.path()).unwrap(),
);
// Prints "/tmp/1a9b1".
println!("{:?}", temp_file::empty().path());
```

## Cargo Geiger Safety Report
```

Metric output format: x/y
    x = unsafe code used by the build
    y = total unsafe code found in the crate

Symbols: 
    🔒  = No `unsafe` usage found, declares #![forbid(unsafe_code)]
    ❓  = No `unsafe` usage found, missing #![forbid(unsafe_code)]
    ☢️  = `unsafe` usage found

Functions  Expressions  Impls  Traits  Methods  Dependency

0/0        0/0          0/0    0/0     0/0      🔒  temp-file 0.1.6

0/0        0/0          0/0    0/0     0/0    

```
## Changelog
- v0.1.6
  - Return `std::io::Error` instead of `String`.
  - Add
    [`cleanup`](https://docs.rs/temp-file/latest/temp_file/struct.TempFile.html#method.cleanup).
- v0.1.5 - Increase test coverage
- v0.1.4 - Add
  [`leak`](https://docs.rs/temp-file/latest/temp_file/struct.TempFile.html#method.leak)
  and
  [`panic_on_cleanup_error`](https://docs.rs/temp-file/latest/temp_file/struct.TempFile.html#method.panic_on_cleanup_error).
- v0.1.3 - Update docs
- v0.1.2 - Update example
- v0.1.1 - Minor code cleanup, update docs
- v0.1.0 - Initial version

## Happy Contributors 🙂
Fixing bugs and adding features is easy and fast.
Send us a pull request and we intend to:
- Always respond within 24 hours
- Provide clear & concrete feedback
- Immediately make a new release for your accepted change

License: Apache-2.0
