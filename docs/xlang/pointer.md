# Pointers [ptr]

## Root Pointers

1. Every valid pointer belongs to a graph originating from a root pointer. 
2. A root pointer is obtained from the following operations:
* A constant that obtains the address of a global definition,
* A constant that obtains the address of a label,
* A constant that obtains a null pointer,
* A string literal constant of a pointer type,
* An `addr_of` expression, applied to an lvalue that was not obtained from a pointer
3. A null pointer is a root pointer with address zero. It is obtained from a null pointer constant, or a global_address constant applied to a weak declaration where no definition is provided. Every null pointer is identical, and any valid pointer with address zero is a null pointer
4. [Note: If a valid pointer to address zero is obtained in any other way but a global_address constant applied to a weak declaration or a null pointer constant, the behaviour is undefined]
5. Every other root pointer points to one of the following:
* An object,
* A function, or
* A target.
