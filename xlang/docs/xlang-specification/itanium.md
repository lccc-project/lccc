# Itanium Names

## Suggested Names for Rust Programs

_This section is not normative_

1. When mangling itanium names for rust programs, it is recommended to use the following conventions:
    - A reference should be mangled as `R<type>` (for `&mut`) or `RC<type>` (for `&`). The restrict qualifier should not be used for `&mut` (as in `rR<type>`), even if the C++ implementation distinguishes overloads with the `restrict` qualifier. 
        - A reference type with an explicit static lifetime should be mangled as a type qualifier of the reference, with the name `U6static` (So `&'static T` should be mangled as `U6staticRCT`) A reference type with a parameterized lifetime should be mangled as `U8lifetimeI<item>E`, where `<item> is a reference to the generic parameter item. Whether elided or implicit lifetimes are mangled, and how such lifetimes are mangled, is not specified
    - A raw pointer should be mangled as `P<type>` (for `*mut`) or `PC<type>` (for `*const`)
    - The unit type should be mangled as the extended type `u4unit`. All other tuple types should be mangled as the extended type `u7product` with template arguments for each parameter type.
        - As a further special case, the unit (`()`) and never type (`!`) should be mangled as `v` when it appears in a return type postion as a non-dependant type, and in no other context.
    - The slice type should be mangled as the extended type `u5slice`, with exactly one template argument for the type of the slice.
        - The type `str` should be mangled as `u5sliceIDuE` (IE. slice of `char8_t`)
        - The type `::std::ffi::CStr` should be mangled as `u5sliceIcE` (IE. slice of `char`)
    - The type `char` should be mangled as `Di` 
    - The type `::core::raw::c_void` should be mangled as `v`. The mangling of member enumerators of the type should be prefixed by the full name of `c_void` (IE. `4core3raw6c_void`). 
    - The never (`!`) type should be mangled as the extended type `u5never`.
    - The mangling of Function-item types should be `DtL_Z<encoding>EE`, where `<encoding>` is the encoding of the function item
    - `impl <Trait>` should be encoded as `u4impl` in a context with the template arguments being each trait included as type names. Whether or not lifetime bounds are mangled, and how they are mangled, is not specified
        - When mangling the trait `?Sized`, this should be encoded as `u7unsized`. The trait `Sized` should be mangled if and only if it is explicitly specified. 
        - When used in a return position, it shall be designated using a `local-name`, with an unnamed entity name. In this case, the return type for the function encoding used in the local-name should be the type designated above. 
            - When referring to a function name, with this return type, the `local-name` is used
            - _Note - For example the Rust signature `fn bar() -> impl Any+Send+Sync` at the top level in the crate `foo` would be mangled as the (interesting) name `_Z3foo3barZNS0_u4implIN4core3any3AnyNS1_6marker4SendNS2_4SyncEvEUt_v`. - End Note_
            - If multiple `impl Trait` types occur within a return type, they are named in order, so the first will have the local name `Ut_`, the second `Ut0_`, third `Ut1_`, etc. 
    - `dyn <Trait>` should be encoded as `u3dyn` with the template arguments being each trait included as type names. Whether or not lifetime bounds are mangled, and how they are mangled, is not specified. `dyn Trait`, where `Trait` contains a single trait, and no lifetime bounds, should be encoded as a `<type>`. 
    - The typename of a trait should be mangled as a `class-or-enum` name without a specifier.
        - Each generic argument, if present, is mangled in order as template arguments. 
        - After generic arguments, each associated type is mangled in declaration order. Any associated type which is not present is mangled as `Da` (IE. as `auto`), and any associated type which is constrained by a trait is mangled as the equivalent `impl Trait` (do not use rules for mangling in return type position). 
    - Explicit generic lifetimes should be mangled as `u6static` (for `'static`) or the respective template position (for references to generic lifetimes), except that a static lifetime in the first template parameter position, and any immediately subsequent `'static` lifetimes should not be mangled. Whether or not elided or implicit lifetimes are mangled, and how they are mangled is not specified. 
    - The virtual table for `impl Trait for Type` should use the encoding `TV<trait encoding>.impl<implementation type>[I<implementation template-args>+E]`, where `trait` is mangled as above, except associated types are not mangled. `generic-args` represent the deduced generic arguments of the impl, are are present if the implementation is generic. If multiple equivalent trait implementations are defined (for example, using specialization), the first shall be mangled as `TV<trait encoding>.impl<implementation type>Q_[I<implementation template-args>+E]` and subsequent ones shall be mangled as `TV<trait encoding>.impl<implementation type>Q<seq num>_[I<implementation template-args>+E]`, with an increasing sequence number. 
    - Linkage of virtual tables for such trait impls are as follows:
        - A trait implementation is generic if it is declared with 1 or more template parameters that is not a lifetime parameter.
        - A trait implementation that is not generic shall have it's vtable defined only in the translation unit that provides the impl, and the trait is object safe. 
        - Otherwise, if the trait implementation is viable for some type *T*, a coercion unit that performs an unsized coercion involving a transformation from T to `dyn `*`Trait`*, or `dyn `*`Trait`*` + Trailing`, where `Trailing` is any combination of marker traits and lifetime bounds, shall instantiate the implementation, and emit the vtable in that translation unit. If multiple translation unit instantiate the same implementation, they shall consist of the same sequence of input tokens, and conditional compilation groups which shall be evaluated the same way or the, no diagnostic is required. All such vtables shall then be merged.
    - A method defined within a trait impl should mangled be as follows:
        `<special-name> := <method encoding>.impl<implementation type>[I<implementation template-args>+E]`
    - An associated constant of a trait impl shall be mangled the same way, as necessary (for example, when mangling temporary objects materialized while initializing the constant and extended to the `'static` lifetime).
        - In all cases, when a special-name is used, which is based on such an associated constant, the `.impl` part shall follow the name
    - In the above, if there is more than one candidate implementation for the block, the first shall be mangled as `<encoding>.impl<implementation type>Q_[I<implementation template-args>+E]`, and subsequent implementations shall be mangled as `<encoding>.impl<implementation type>Q<sequence number>_[I<implementation template-args>+E]`.
2. Compliance with the above requirements is not necessary for conformance with this specification, but is recommended.
3. No vendor which complies with the above specification shall use a vendor-specific suffix in a mangled name that starts with the terminal `impl`, except to mangle a name mentioned in (1).

## Examples for above Rules

All are nested in the crate foo

1. `fn name() -> &'static str`
  - Manged as `_ZN3foo4nameU6staticRCu5sliceIDuE`
2. `fn get<U,F: FnOnce()->U>(f: F) -> U` called with `get(crate::name)`
  - Instantiation mangled as `_ZN3foo3getIU6staticRCu5sliceIDuEDtL_ZNS_4nameU6staticRCu5sliceIDuET0_T_`

