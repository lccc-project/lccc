# Pointers [ptr]

## Pointer Validity [ptr.validity]

1. Every pointer type may be parameterized by a number of attributes that impact validity of pointer values, and may be categorized into two groups, trivial validity and non-trivial validity.
2. The following attributes affect trivial validity:
    - `aligned`
    - `nonnull` 
    - `dereferenceable`
    - `dereference_write`
    - `write_only`
    - `invalid`
3. The following attributes affect non-trivial validity:
    - `dereferenceable`
    - `dereference_write`
    - `write_only`
    - `null_or_dereferenceable`
    - `null_or_dereference_write`
    - `null_or_write_only`
    - `invalid`
    - `null_or_invalid`
    - `unique`
    - `readonly`
    - `readshallow`
4. The following attributes have no affect on validity, but may additionally be present on a pointer type:
    - `near`
    - `far`
    - `ref`
    - `rvalue_ref`
    - `const`
    - `volatile`
5. [Note: In the case of `near`, `far`, the attribute serves to distinguish on segmented architectures between pointers that solely consist of an offset, and pointers that contain a segment + offset in some machine-specific manner. In the other cases, those attributes merely serve to distinguish source-level differences in pointer types]
6. An attribute that affects non-trivial validity is a non-trivial validity attribute. An attribute that affects trivial validity and is not a non-trivial validity attribute is a non-trivial validity attribute.
7. [Note: While `dereferenceable`, `dereference_write`, `write_only`, and `invalid` affect trivial validity, none are trivial validity attributes. The effect on trivial validity for these attributes is the same as that for `nonnull`]

## Root Pointers [ptr.root]

1. Every valid pointer belongs to a graph originating from a root pointer. 
2. A root pointer is obtained from the following operations:
* A constant that obtains the address of a global definition,
* A constant that obtains the address of a label,
* A constant that obtains a null pointer,
* A string literal constant of a pointer type,
* An `addr_of` expression, applied to an lvalue that was not obtained from a pointer
3. A null pointer is a root pointer with address zero. It is obtained from a null pointer constant, a global_address constant applied to a weak declaration where no definition is provided, and operations that produce a null pointer. Every null pointer is identical, and any valid pointer with address zero is a null pointer.
4. [Note: If a valid pointer to address zero is obtained in any other way but a global_address constant applied to a weak declaration or a null pointer constant, the behaviour is undefined]
5. Every other root pointer points to one of the following:
* An object,
* One byte past the end of an object,
* A function, or
* A target.
6. A root pointer to an object is invalidated when the object is deallocated, and in all other cases that a pointer might be invalidated.
7. An uninitialized value of a pointer type is an invalid pointer. The address represented by an uninitialized value of a pointer type is indeterminate.

## Derive Operations [ptr.derive]

1. A derive operation is an operation that asserts non-trivial validity of a pointer type, possibly invalidating other incompatible pointers.
2. A derive operation occurs when a program executes the `derive` instruction, or an implicit derive operation occurs (see below).
3. If a derive operation is validly applied to an invalid pointer, a null pointer, a pointer to a function, or a pointer to a target, the result is that same pointer. Otherwise, the derive operation forms an edge in the pointer graph the operand belongs to, and yields a new pointer value which is connected to the same graph through this edge, which connects from the operand. 
4. When a derive operation occurs, if the attribute `invalid` is applied by the operation, the behaviour is undefined if the operand pointer is a null pointer. Otherwise, the result is an invalid pointer that represents the address of the result. This does not form a new graph edge. 
    - When a derive operation occurs, if the attribute `null_or_invalid` is applied by the operation, the result is a null pointer if the operand is a null pointer. Otherwise, the result is an invalid pointer. This does not form a new graph edge.
5. Otherwise, if the derive operation introduces `dereferenceable`, `dereference_write`, or `write_only`, the behaviour is undefined if the pointer is not a pointer to an object, or a pointer one byte past the end of the object. 
    - If the derive operation introduces `null_or_dereferenceable`, `null_or_dereference_write`, or `null_or_write_only`, the result is a null pointer if the operand is a null pointer. Otherwise, the behaviour is undefined if the pointer is not a pointer to an object.
6. If the result is a pointer to an object, then other pointers are invalidated as follows. An invalidated pointer has it's corresponding vertex removed from its pointer graph, and pointers derived from it are also invalidated:
    - Any pointer, that is not a parent of the result, to the same object, or to any subobject, other than a mutable subobject, with the `unique` attribute, are invalidated.
    - If the derive operation introduces `unique`, any pointer, that is not a parent of the result, to the same object or to any subobject is invalidated.
    - If the derive operation introduces `readonly`, any pointer, that is not a parent of the result, to the same object that has a valid range attribute of `dereference_write` or `write_only` (including `null_or_dereference_write` and `null_or_write_only`) is invalidated.
    - If the derive operation introduces `dereference_write`, `null_or_dereference_write`, `write_only`, or `null_or_write_only`, any pointer to the same object or any superobject with the `readonly` attribute or any pointer with the `readshallow` attribute to the same object that is a parent of the result is invalidated, unless the result pointer is to a mutable subobject of that object, or to some subobject of such a mutable subobject.
7. If the derive operation introduces `dereferenceable(n)`, `null_or_dereferenceable(n)`, `dereference_write(n)`, or `null_or_dereference_write(n)`, then the behaviour is undefined if the resultant pointer is invalidated by this operation. The behaviour is further undefined if the first `n` bytes from the address represented by the resultant pointer is not reachable for reads (`[ptr.reachability]`), or the pointer is `null` (in the case of `null_or_dereferenceable` or `null_or_dereference_write`).
8. If the derive operation introduces `dereference_write(n)`, `null_or_dereference_write(n)`, `write_only(n)`, or `null_or_write_only(n)`, then the behaviour is undefined if the resultant pointer is invalidated by this operation. The behaviour is further undefined if the first `n` bytes from the address represented by the resultant pointer is not reachable for writes (`[ptr.reachability]`), or the pointer is `null` (in the case of `null_or_dereference_write` or `null_or_write_only`).
9. An implicit derive operation occurs in all of the following cases:
    - Each parameter passed to a function undergoes an implicit derive operation if the parameter type either in the callsite signature, or in the definition signature has any non-trivial validity attribute.
    - The value returned from a function undergoes an implicit derive operation if the return type either in the callsite signature, or in the definition signature has any non-trivial validity attribute
    - Whenever a pointer value is assigned to an lvalue of a pointer type that has any non-trivial validity attribute.
    - Whenever a pointer value is read from an lvalue of a pointer type that has any non-trivial validity attribute.
    - Whenever a pointer value is produced from an expression, other than a `call` expression, that has exactly one pointer operand and pointer result, if one of the following conditions is satisfied:
        - The result is a valid pointer that differs from the pointer operand,
        - The type of the pointer operand has a non-trivial validity attribute.
    - Whenever the address of an lvalue is taken, if that lvalue was produced by indirecting a pointer

## Reachability [ptr.reachability]

1. A byte is reachable from a pointer `p` if it is part of the object representation of an object `o`, such that `p` points to or one-past-the-end of an object `a` that is *pointer-interconvertable* with `o` or `a` is a *sibling subobject* of such an object.
2. A byte is not reachable for the purposes of reading from a pointer `p` if it belongs to the object representation of an object `o` such that there exists a pointer `q` that points to `o`, `p` is not derived from `q`, and `q` is `unique`. Otherwise that byte is reachable for the purposes of reading if it is reachable from `p`.
3. A byte is not reachable for the purposes of writing from a pointer `p`, if either `p` or some pointer that `p` is derived from is `readshallow` or that byte belongs to the object-representation of an object `o`, such that either `o` is a const complete object or there is some pointer `q` that points to `o` where `q` is `readonly`, and the byte does not belong to the object-representation of a mutable subobject `a` of `o`. Otherwise, that byte is reachable for the purposes of writing if it is reachable for the purposes of reading from `p`.
