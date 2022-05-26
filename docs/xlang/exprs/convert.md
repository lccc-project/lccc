# Type Conversions [expr.convert]

`expr := convert <strength> <type>`

Type checking: `[..,T]`=>`[..,<type>]`  
Operands: `[..,v]`=>`[..,r]`

## Reinterpret Conversion `[expr.convert.reinterpret]

`strength := reinterpret`

1. `convert reinterpret` can be used to perform any of the following conversions that do not change remove pointer definition attributes:
* Integer type to Integer type
* Integer type to character type
* Character type to Integer type
* Character Type to Character Type
* Integer Type to Pointer type
* Character Type to Pointer type
* Pointer type to Integer type
* Pointer type to Character Type

2. When a conversion occurs between integer types, between character types, or from an integer type to a character type as though by `convert strong`

3. When a converion occurs between pointer types, the address of the incoming pointer value is considered. If there is an addressible object of the pointee type that starts at the address of the incoming pointer, or ends just before the address of the incoming pointer, the result is a pointer to that object or one-past-the-end of that object if every byte reachable from the result is reachable from the incoming pointer. If the result is a pointer to a different object than the incoming pointer, or the result type has any non-trivial validity invariants, a derive operation occurs.

4. When a conversion occurs between a pointer and an integer type or character type, first the pointer is converted into its address in the addr-type of the pointer type, then the address is converted (possibly by truncation) to the destination type. The addr-type of a pointer type is the unsigned integer type that is the same width as the pointer type. If the conversion is to an integer or charater type that is at least as wide as the addr-type, the conversion may be losslessly reversed by (5).
