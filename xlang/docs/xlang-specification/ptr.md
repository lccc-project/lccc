# Pointer Accesses and lvalues `[ptr]`

## Representation of Pointers `[ptr.repr]`

1. All pointer types with sized pointee types shall have the same size as an implementation-defined integer type. All pointer types shall have the same implementation-defined alignment. 

2. A pointer can be said to "represent" an address, as follows:
    * A pointer to an object, or past the end of an object represents the address of the first byte in the *object-representation* of that object, or the first byte after that *object-representation* respectively.
    * A pointer to a function represents the address of that function
    * A null pointer represents the address 0. 
    * An invalid pointer represents an unspecified address.
    * A pointer invalidated by a derive operation represents the same address that it represented before being invalidated. 

3. All pointers that represent the same address shall have the same *object-represention* (even if those pointers would compare unequal). 

4. Two pointers compare equal if the following conditions are met:
    * A null pointer compares equal with any other null pointer, and with no other pointer values.
    * Two pointers compare equal if they represent the same address

5. _Note 1 - these two rules, as well as clause 2 above, implies no pointer, other than a null pointer, represents the address 0 - End Note_

6. _Note 2 - two pointers which compare equal will have the same_ object-representation _. However, as mentioned above, the reserve is not necessarily guaranteed- End Note_

7. All other comparisons involving pointers shall form an unspecified total order which shall be according to at least the following rules:
    * A pointer to the *i*<sup>th</sup> element of an array compares less-than the pointer to the *j*<sup>th</sup> element of the same array, where *i* < *j*. 
    * A pointer to a member of a structure type shall compare less-than a pointer to a member of the same structure type that occurs later in declaration order, and either the structure does not have the tag `nonstandard_layout`, or the members have the same visibility
    * A null pointer shall compare less than any other valid pointer
    * The total order shall be consistent with the equality rules for pointers, described above.

## Invalidation `[ptr.invalidation]`

1. During Execution of a program, a pointer value is exactly one of the following:
    * A pointer to an object
    * A pointer past the end of an object
    * A pointer to a function
    * A null pointer
    * An invalid pointer.

2. A pointer that is not an invalid pointer may be described as being a valid pointer. A pointer that is not a null pointer may be described as being a `non-null` pointer. 

3. Each kind of pointer value describes what operations can be performed on that pointer:
    * Pointers to objects may be dereferenced
    * Pointers to objects and pointers passed-the-end of an object may have pointer arithmetic applied to them
    * Pointers to functions may be called
    * Valid Pointers may be compared for equality, and may be ordered according to the total order specified in `[ptr.repr]`.

4. A derive expression is an operation that, given one pointer value, produces a new equivalent pointer value, or performs some operation and yields a new pointer to a *reachable* object. This new pointer is called a *derivative pointer*. The original pointer value is called the *originating pointer*.


5. A derive expression is parameterized by a *pointer-type*, which specifies zero or more pointer validity attributes, which can each be one of the following:
    * `nonnull`
    * `invalid`
    * `null_or_invalid`
    * `dereferenceable(n)`
    * `dereference_write(n)`
    * `write_only(n)`
    * `null_or_dereferenceable(n)`
    * `null_or_dereference_write(n)`
    * `null_or_write_only(n)`
    * `aligned(n)`
    * `readonly`
    * `unique`
    * `bounded(b)`

6. Among these validity attributes, they can be classified into trivial, and non-trivial validity requirements. `nonnull` and `aligned(n)` are both trivial. All others are non-trivial. 

7. Certain types of validity attributes are incompatible. When such attributes are introduced by a derive expression, pointers with incompatible attributes are invalidated as follows:
    * When an expression introduces the `unique` attribute, all other pointer values, except the originating pointer, and any originating pointers from that sequence, to the same object, or any subobject are invalidated. 
    * When an expression introduces the `readonly` attribute, all pointers with the attributes `dereference_write` or `write_only`, except the originating pointer, and any originating pointers from that sequence, to the same object, or any subobject are invalidated, except for pointers to any `mutable` subobject, or subobject *transparently pointer-interconvertible* with such a subobject.
    * When an expression introduces the attributes `dereference_write` or `write_only`, all pointers with the attribute `readonly` are invalidated.
    * When an expression introduces `dereferenceable`, `dereference_write`, or `write_only`, all other pointers to the same object with the `unique` attribute are invalidated.
    * `non-null` pointers with the attributes `null_or_dereferenceable`, `null_or_dereference_write`, and `null_or_write_only` shall behave, for the purposes of these rules, as though they had the attributes `dereferenceable`, `dereference_write`, or `write_only`.

8. If an *orginating pointer* is invalidated, all of it's *derived pointers* are invalidated. If a derive expression is applied to an invalidated pointer, including the derive expression which caused the pointer to become invalidated, the behaviour is undefined if it introduces any non-trivial validity requirement, other than `invalid` or `null_or_invalid`.

9. An invalidated pointer is not a valid pointer. 
10. _Note 1- this implies that a null pointer can never be invalidated - End Note_. 

11. If an expression is evaluated which requires a valid pointer, such that the expression is unsequenced relative to an expression which invalidates that pointer, the behaviour is undefined. 

12. Any pointer passed to or returned from a function, read from or written to an object, is considered to be a derivative pointer of the explicit value, as though an intervening derive expression with the type of the pointer is applied. 

13. A destroy expression performs some operation, and invalidates the pointer after the completion of that operation. 
14. Expressions which require a valid pointer may not be reordered relevative to the operation performed by a destroy expression applied to the same pointer.
15. _Note 2 - this implies that the invalidation of the pointer is atomic w.r.t. the performed operation - End Note_
16. If a derive expression produces a pointer with the `invalid` attribute, the result is an invalid pointer. If a derive expression produces a pointer with the `null_or_invalid`, the result is an invalid pointer if it is not a null pointer.

## Reachability `[ptr.reach]`

1. A pointer can access any object for which the *object-representation* of that object overlaps a byte reachable from that pointer, unless the object is *unaddressible* (that is, no pointer can be formed to that object) and the object is not a subobject of any object *pointer-interconvertible* with the pointed to object _Note - in particular, the discriminant of niche-optimized unions is not reachable from a pointer to the payload - End Note_. (Pending resolution of <https://github.com/rust-lang/unsafe-code-guidelines/issues/84>) 

2. A byte *n* is reachable from a pointer p, if the pointer was not returned from the intrinsic `::__lccc::builtins::rust::limit`, or a *derivative* of such a pointer, and the byte is part of the *object representation* of the object pointed to by p, any object *pointer-interconvertible* with that object, or the immediately enclosing array thereof. 
3. If the pointer was returned from a call to the intrinsic `::__lccc::builtins::rust::limit`, with an extent n on a pointer of type `T`, it is treated as though the returned pointer points to an array with extent `n` and element-type `T`, for the purposes of determining reachability. 

4. A byte *n* is not reachable from a pointer p if it is part of the *object-representation* of an object pointed to by a `unique` pointer which is neither a derivative nor an orginator of p.

5. A byte *n* is not reachable for the purposes of writing from a pointer p if it is part of the *object-representation* of an object pointed to by a `readonly` pointer, or a pointer returned from `::__lccc::builtins::rust::kill_mutation`, which is not a derivative of p, or if it is part of the *object-representation* of an object with static storage duration that is declared `#const` and is not part of the *object-representation* of a mutable subobject thereof (pending resolution of <https://github.com/rust-lang/unsafe-code-guidelines/issues/236>).

6. If a valid pointer is produced via a derive expression that has a `dereferenceable(n)`, `dereference_write(n)`, or `write_only(n)` attribute, where n bytes from the address represented by the pointer value are not all reachable from that pointer, the behaviour is undefined. 
7. If a valid pointer is produced via a derive expression that has a `dereference_write(n)` or `write_only(n)` attribute, where `n` bytes from the address represented by the pointer are not all reachable from that pointer for the purposes of writing, the behaviour is undefined. 

9. If an `add` or `sub` expression results in a pointer that represents an address which is not reachable from the original expression, the behaviour is undefined. 
10. If a `sub` expression is applied to two pointer values, where the address represented by either pointer is not reachable from the other pointer, the behaviour is undefined. 

11. A byte is reachable from an lvalue obtained by indirecting or subscripting a pointer if that byte is reachable from that pointer.


## Incorrect-type accesses `[ptr.access]`

1. When an lvalue of type `T` accesses an object `o` of type `U`, where `T` and `U` are not the same type,the behaviour is undefined if the lvalue was obtained by indirecting a pointer tagged `strict_alias` and `T` and `U` are not compatible types. Otherwise, the *object-representation* of smallest object *pointer-interconvertible* with `o` that is at least the size of `T`, or the immediately enclosing array of any such object, are reinterpreted as type `U`, as though the *object-representation* of a synthetic object (this is the *materialized object-representation*). 
2. If no such object exists, the behaviour is undefined. If any byte in the *materialized object-representation* is unreachable from the lvalue, each such byte is treated as though it is the object representation of type `u8` with the value `uninit`. The behaviour is undefined if the access is an assignment. 
3. If all bytes accessed as part of the *materialized object-representation* of a scalar type is a padding byte, or part of the *object-representation* of a scalar object with the value `uninit`, the resulting value is `uninit`, unless the access has class `freeze` or `volatile`.
4. If any bytes accessed as part of the *materialized object-representation* of a scalar type is a padding byte, or part of the *object-representation* of a scalar object with the value `uninit`, the value of each such byte is unspecified. 
5. If the resulting value is not a valid value of the type, or the value is `uninit`, and the type of the value has a validity attribute, or is a pointer type with a non-trivial validity requirement, the result is `invalid`. 
 
6. If a value is written to the *materialization object-representation* of such a scalar, the value of any remaining bytes of overlapping scalar objects with value uninit are unspecified _Note - The value of that scalar would no longer be `uninit`, but can still be an invalid value - End Note_. 
7. If the resulting value of `T` is not a valid value of the type, the result is `invalid`. If any scalar object with value `uninit` is created, the result is `invalid` if the object is of a type with a validitity attribute, or a pointer type with a non-trivial validity requirement. 
8. _Note 1 - The behaviour of a program that produces or stores the value `invalid` is undefined. Objects may have an `invalid` value provided they are not loaded from - End Note_
 
9. When an lvalue of type `T` access raw storage, an complete object of type `T` is implicitly created, with the value `uninit`. If `T` or any field or base class of `T` has a validity attribute, or is a pointer type with a non-trivial validity requirement, the object instead has value `invalid`. 
10. The behaviour is undefined if any byte in the *object-representation* of the implicitly created object is unreachable from the lvalue. 

## Pointer Conversions `[ptr.convert]`

1. It shall be possible to convert, via a `convert reinterpret` expression, any pointer value to an integer type which is the same size as the pointer type. 
2. If the pointer value was obtained by converting a value of an equally sized integer type, possibly applying any sequence of `add` or `sub` operations on the converted pointer, the result shall be equal to the original value, after the equivalent `add` or `sub` is applied to that value. Otherwise, the result is an unspecified value of that type. 
3. If the chosen value is not valid for the type, the result is `invalid`.
4. Two pointers that *represent the same address* shall convert to values of that type which compare equal. Two pointers that do not *represent the same address* shall convert to values of that type which compare unequal. 
5. A null pointer shall convert to the value 0 of all integer types.

6. It shall be possible to convert, via a `convert reinterpret` expression, any value of an integer type to any pointer type of the same size. 
7. If the integer value was obtained by converting a pointer value, possibly applying any sequence of `add` or `sub` operations, such that the equivalent operations applied to the original pointer would have defined behaviour, the result shall be that original pointer with the equivalent operations applied, if the pointer types are the same, otherwise the result shall be the same as converting to the original pointer type, then converting to the target type by a `convert reinterpret` expression.  
8. Otherwise, the result is unspecified. If the chosen value is not valid for the type, or does not satisfy a non-trivial validity requirement of the pointer type, the result is `invalid`.

9. It shall be possible to convert a pointer value to any other integer type by a `convert reinterpret` expression, as though by converting to an unspecified integer type of the same size as the pointer type, then to the requested type, by two separate, consecutive, `convert reinterpret` expressions. 

10. It shall be possible to convert a value of any other integer type to a pointer type by a `convert reinterpret` expression, as though by converting to an unspecified integer type of the same size as the pointer type, then to the pointer type, by two separate, consecutive, `convert reinterpret` expressions.

11. It shall be possible to convert a value of any sized pointer type to any other sized pointer type. If there is a object of type `T`, where `T` is the pointee type of the converted-to pointer, that is *pointer-interconvertible* with the object pointed to by the converted pointer value, if any, the result is a pointer to that object. Otherwise, the result is the same pointer value. 

12. It shall be possible to convert a value of a pointer-to-slice type, to a pointer to it's element type. The resulting pointer shall be a pointer to the first element of the pointed-to-slice, if any, or a null pointer if the converted pointer value is a null pointer. 
13. Two objects, *a* and *b* are *pointer-interconvertible* if
    * They are the same object 
    * One object is of a standard-layout structure type, and the other is it's first non-static data member, or if it has no non-static data members, any of it's base classes.
    * One object is of a union type, and the second is any one of it's members.
    * One Object is of a transparent structure type, and the other is any member of that object
    * There exists an object *c*, such that *a* is *pointer-interconvertible* with *c* and *c* is *pointer-interconvertible* with *b*.
14. _Note 1 -_ pointer-interconvertibility _is reflexive, symmetric, and transitive - End Note_ 
15. Pointers to *pointer-interconvertible* objects represent the same address. 
    

## Trivial Pointer Validity `[value.validity.ptr]`

1. The trivial validity attributes for pointer types shall be the validity attributes `nonnull` and `aligned(n)` for some `n` that is a power of two.

2. A pointer that is `dereferenceable`, `dereference_write`, `write_only`, or `invalid` is considered to have the validity attribute `nonnull`. 

3. A null pointer is not a valid value of a pointer type with the validity attribute `nonnull`. 
4. A pointer is said to be *aligned to* n, if n is a power of two, and the address represented by the pointer is divisible by n.
5. A pointer which is not *aligned to* n is not a valid value of a pointer type with the validity attribute `aligned(n)`


