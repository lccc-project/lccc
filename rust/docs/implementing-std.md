# Implemeting std for lccc

lccc's stdlib is publically equivalent[^1] to the stdlib provided by rustc, but it has very different internals. Thus, a standard library written for lccc must be done differently from one written for rustc. This document describes the minimum process necessary to implement it.

[^1]: Modulo specific changes to unspecified layout details, as well as symbols produced by various attributes.

## Required Features

In order to implement the standard library, you will need a number of features. Without these features, implementing `core` is impossible, and both `std` and `alloc` are difficult:
- `lang_items`: This is similar to the `lang_items` feature from `rustc`, and provides the `lang` attribute. Many things in `core` and some things in `alloc` are lang items, and this feature allows such language items to be defined. [Some](#Required-Language-Items) lang items *must* be defined in order for the compiler to function. 
- `lccc_intrinsic_crate`: The `__lccc` "crate" is implicitly inserted into the extern prelude of every (edition 2018 on) crate by the compiler, though it is unstable. It exports a number of attributes, a couple macros, and a large number of functions that act as compiler intrinsics. 
- `no_core`: In order to develop core, you need the `#![no_core]` attribute. Otherwise, lccc (and incidentally, rustc) will attempt to link `core` to the library (which is not what you want if you want to define `core`).

## Required Language Items

Like in rustc, lccc allows the standard library to indicate certain items are "special" to the compiler, using the `lang` attribute. Programs and libraries that use the `lang_items` feature can define their own implementations for language items. Many language items are implemented in `core`. Note that it may not be an error if a required language item is not defined, and errors may not be useful (in particular, missing required language items can trigger `lccc::lcrust::diag::internal_diagnostic` errors)

The following Language Items must be defined by every crate or a dependency thereof:
- `sized`
- `trivial_destruction`
- `type_destructor`

Some language features also require that certain language items be defined in crates that use those features or in a dependency thereof:
- `global_allocator`: Any crate that uses the `#[global_allocator]` attribute. 
- `reciever`: Any crate that defines a method with a `self` argument.
- `dispatch_for_dyn`: Any crate that references a trait object type, or provides a non-generic trait implementation
- `slice_ptr_layout`: Any crate that creates a pointer or reference to a slice type (possibly indirectly)
- `trait_obj_ptr_layout`: Any crate that creates a pointer or reference to a trait object type (possibly indirectly)
- `termination`: Any program crate that does not use the `#![no_main]` attribute, and any crate that uses the `#[test]`.
- `panic_no_fmt`: Any crate that uses the `panic` macro.
- `panic_fmt`: Any crate that uses the `panic` maco (available by defining a macro using declmacro v2 syntax with the name `panic` that has the `#[__lccc::builtin_macro]` attribute), or that does any of the following:
    - Subscripts an array or slice or primitive reference to an array or slice
    - Performs a primitive arithmetic operation outside of required constant evaluation, if overflow_checking or debug_assertions are enabled in that crate
- `drop_in_place`: Any crate that performs an unsize coercion to a trait object, or provides a non-generic trait implementation
- Many other language items are required. The full list can be found at <https://github.com/LightningCreations/lccc/blob/main/rust/lcrust/front_end/include/LangItem.hpp>


## Implementing a Minimal Core

The most minimal core, that can function requires the language items described in the above section, but they have to be on the correct things.

Thus, the minimal core is
```rust
#![no_core]
#![feature(no_core,lang_items,lccc_intrinsic_crate)]

#[lang = "sized"]
pub trait Sized{}

#[lang = "trivial_destruction"]
pub unsafe auto trait TrivialDestruction{}


pub trait Drop{
    #[lang = "type_destructor"]
    fn drop(&mut self);
}

```


Breaking down the requirements, we can describe precisely what each lang item needs to be present on, and what it does:


