# Itanium Names

## Suggested Names for Rust Programs

_This section is not normative_

1. When mangling itanium names for rust programs, it is recommended to use the following conventions:
    - A reference should be mangled as `R<type>` (for `&mut`) or `RC<type>` (for `&`). The restrict qualifier should not be used for `&mut` (as in `rR<type>`), even if the C++ implementation distinguishes overloads with the `restrict` qualifier. 
    - A raw pointer should be mangled as `P<type>` (for `*mut`) or `PC<type>` (for `*const`)
    - The unit type should be mangled as the extended type `u4unit`. All other tuple types should be mangled as the extended type `u7product` with template arguments for each parameter type.
        - As a further special case, the unit (`()`) and never type (`!`) should be mangled as `v` when it appears in a return type postion as a non-dependant type, and in no other context.
    - The slice type should be mangled as the extended type `u5slice`, with exactly one template argument for the type of the slice.
    - The type `::core::raw::c_void` should be mangled as `v`. The mangling of member enumerators of the type should be prefixed by the full name of `c_void` (IE. `4core3raw6c_void`). 
    - The never (`!`) type should be mangled as the extended type `u5never`.
    - The mangling of generic lifetimes is not specified. Whether or not generic lifetimes are mangled is not specified. 
    - The mangling of Function-item types should be `DtL_Z<encoding>EE`, where `<encoding>` is the encoding of the function type
    - `impl <Trait>` should be encoded as `u4impl` in a context with the template arguments being each trait included as type names. Whether or not lifetime bounds are mangled, and how they are mangled, is not specified
        - When mangling the trait `?Sized`, this should be encoded as `u7unsized`. The trait `Sized` should be mangled if and only if it is explicitly specified. 
        - When used in a return position, it shall be designated using a `local-name`, with an unnamed entity name. In this case, the return type for the function encoding used in the local-name should be the type designated above. 
            - When referring to a function name, with this return type, the `local-name` is used
            - _Note - For example the Rust signature `fn bar() -> impl Any+Send+Sync` at the top level in the crate `foo` would be mangled as the (interesting) name `_Z3foo3barZNS0_u4implIN4core3any3AnyNS1_6marker4SendNS2_4SyncEvEUt_v`. - End Note_
            - If multiple `impl Trait` types occur within a return type, they are named in order, so the first will have the local name `Ut_`, the second `Ut0_`, third `Ut1_`, etc. 
    - `dyn <Trait>` should be encoded as `u3dyn` with the template arguments being each trait included as type names. Whether or not lifetime bounds are mangled, and how they are mangled, is not specified. `dyn Trait`, where `Trait` contains a single trait, and no lifetime bounds, should be encoded as a `<type>`. 
    - The typename of a trait should be mangled as a `class-or-enum` name without a specifier.
        - Each generic argument, if present, is mangled in order as template argument. Whether lifetime generic arguments are included, and how they are mangled, is not specified.
        - After generic arguments, each associated type is mangled in declaration order. Any associated type which is not present is mangled as `Da` (IE. as `auto`), and any associated type which is constrained by a trait is mangled as the equivalent `impl Trait` (do not use rules for mangling in return type position). 
        
2. Compliance with the above requirements is not necessary for conformance with this specification, but is recommended.

