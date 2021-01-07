# XLang Introduction [intro]

1. This document describes formally, an intermediate representation called xir, for frontend compilers of other programming languages. It specifies a text based and binary format for the intermediate representation, as well as an API which plugins may interact with to visit entities in defined in xir. 
2. Plugins are independent computer programs operated by the xlang program, or a similar program written with the same purpose, which operates on the constructs defined within the specification. To ensure that compiler frontends may properly implement any programming language, this document defines specific constructs which may be transformed by plugins to any extent, as permitted by the following `[intro.abstract]` and `[intro.compliance]` sections. 
3. An *implementation* refers collectively to the xlang program, or similar program, and all plugins being operated by that program, and any mechanism to evaluate the result as a computer program. Implementation may also refer individual plugins. 
4. Plugins which operate directly on constructs of this specification, transforming them for further operation, are called transformers.
5. _Note 1 - Transformers may be further categorized into optimizers, which transform the constructs based on known equivalences for the purposes of optimization, and reduces, which transform higher level constructs into lower-level ones - End Note_
6. Plugins which accept the constructs of this specification, and transform them into a different programming language, are called code generators. 

## Abstract Machine [intro.abstract]

1. The Semantic Descriptions in this document define a paremeterized, non-deterministic abstract machine. This document defines specifically the abstract machine enclosing all constructs. Plugins which operate on the constructs here need only operate on those which it supports, and may not result in a program that exactly implements or emulates this abstract machine. Rather, plugins need only preserve the *observable behaviour* of the constructs it visits as described below. 
2. Certain constructs of this specification are documented as *implementation-defined*. These represent parameters of the abstract machine. The implementation shall choose the value of each of these parameters, and document that choice. Only the actual behaviour of the construct need be documented. Plugins which do not alter this behaviour need not document such. *Implementation-defined* behaviour includes whether a particular *conditionally-supported* construct is supported.
3. _Note 1 - In particular, transformer plugins need not document implementation-defined behaviour pertaining to constructs which are not affected by such transformations. - End Note_
4. _Note 2 - Code generators, which do not maintain any constructs of the specification in their present form, likely need to document all implementation-defined behaviour of any constructs which it supports - End Note_
5. Certain other constructs of this specification are documented as *unspecified*. These represent non-deterministic aspects of the specification. Implementations may choose how these constructs operate. This document may limit the choices available.
6. _Note 3 - It is not necessary that implementations documentation the choice for unspecified behaviour - End Note_
7. Certain other constructs of this specification are documented as *undefined*. There are no semantic requirements for such constructs imposed on implementations.
8. _Note 4 - If a particular evaluation of a program results in undefined behaviour, the results of that evaluate are not constrained, even with respect to any operations before the first undefined operation - End Note_
9. During execution of a well-formed program, an implementation shall ensure that the observable behaviour of the result corresponds to at least one execution of the current paremeterized abstract machine, provided that execution does not result in undefined behaviour. 
10. _Note 5 - An implementation need not preserve the observable behaviour of an execution that results in undefined behaviour - End Note_
11. The obvervable behaviour of a program shall consistent of the following
    - The number, order, and value stored (if any) of all accesses with the class volatile shall be preserved exactly by the implementation.
    - _Note 6 - An implementation need not produce a program that distinguishes between volatile an non-volatile accesses, provided the accesses qualified as volatile by this specification remain precisely as described by this specification - End Note_
    - Any writes performed to a file shall be reflected on the file system by the time the program terminates.
    - Any writes to an interactive device shall be reflected in that device by the time the program terminates, or a read occurs from that interactive device. What constitutes an interactive device is implementation-defined.
    - _Note 7 - While this specification defines no constructs that performs I/O on files or interactive devices, other specifications may define such constructs. The behaviour of such constructs shall be maintained as defined - End Note_
    - Beyond the above, library functions defined by other specifications may contain additional observable behaviour. The implementation shall maintain this observable behaviour as that specification requires it, to the extent that the operation of surrounding constructs need only consider the requirements of the xlang abstract machine. 
    - _Note 8 - For example, a fictional specification may define a function foo::bar, which accepts a type for which `int(32)` is appropriate, and performs some operation designated as observable behaviour. An implementation shall maintain the call to `foo::bar`, as well as the resulting semantics of that call, but the computation for it's parameter need only respect the constructs of this specification, even if it would be an incorrect transformation according to the specification that defines `foo::bar` - End Note_

## Conformance [intro.conformance]

1. An implementation complies with this specification if it emulates the required behaviour according to `[intro.abstract]` of any program consisting of only constructs it supports, and provides documentation for all *implementation-defined* behaviour. 
2. A number of constructs are required to be supported by all implementations. An implementation shall support constructs consisting of the following
    - 16384 declarations in an input file, which include only object, function, struct, and union declarations. 
    - 32768 local variables declared in a function, including parameters.
    - 256 nested blocks
    - 16 lvalue expression stack items
    - 16 rvalue expression stack items.
    - 16 total expression stack items at any target
    - 2 expression stack items returned from any block, except a top level block.
    - 1 expression stack item returned from a top level block
    - A constant expression which contains only the following
        - A global address
        - A label address, if the identified target has no stack items
        - A null pointer value
        - An undefined value
        - An integer literal of up to 16 bytes. 
        - A floating-point literal that can fit in an IEEE 754 double-precision binary floating-point type.
        - An array constant that is either a repeat expression, or contains at most 32 values, if the type is valid
        - A boolean literal
        - A string literal with a length of at most 32768 characters, and 65536 bytes, if the type of the literal is valid
        - A size_of or align_of constant.
    - 512 expressions in a block, which each contain only the following
        - A constant expression
        - A function call, if the call target has a function pointer type, that is equivalent to the function type at the call.
        - A member expression
        - A conversion from an lvalue to an rvalue.
        - A weak or strong conversion, which is one of the following
            - An integer conversion
            - A floating point conversion
            - A boolean conversion
        - A reinterpret conversion
        - A unary operator expression, other than bit_neg, applied to an integer type, floating-point type, or a vector type. 
        - A bit_neg expression, applied to an integer type, or a vector of an integer type
        - A binary operator expression, other than a bitwise operation, a subscript, or a three-way comparison, applied to two expressions of the same integer, floating-point, or vector type. 
        - A binary bitwise operation, applied to two expressions of the same integer type or a vector of an integer type
        - A binary add, sub, or subscript, operation applied to an expression of a pointer type, except a pointer-to-function type, or a pointer to an incomplete or unsized type, and an expression of an integer type.
        - A binary sub operation applied to two expressions of the same pointer type, except a pointer-to-function type, or a pointer to an incomplete or unsized type.
        - A destroy or derive expression, with up to 16 total destroy, derive, or (if supported) lock, expressions nested. 
        - An indirection, applied to an expression of a pointer type
        - A sequence, or a fence expression, that has an access class other than normal, which does not contain freeze or nontemporal.
        - An assignment, of an expression of any complete type, to an lvalue of the same type.
        - A compound assignment, where the equivalent binary operation is supported, and the access class does not contain nontemporal.
        - An lvalue operation, other than a cmp_excg or a swap, applied to a lvalue expression of an integer, floating-point, or pointer type, other than a pointer-to-function type, or a pointer to an incomplete or unsized type.
        - A swap operation, applied to two lvalue expressions of the same *copyable* type, with an access class that is not normal, and does not contain nontemporal. 
        - A cmp_excg operation, applied to two lvalue expressions of the same *copyable* type, and an expression of that same type, with an atomic access class that does not contain nontemporal. 
        - A block exit
        - A nested block
        - An aggregate constructor, or a tuple expression
    - 1024 targets in a block
    - 4096 total targets in a block and each of it's nested blocks.
    - 1024 tags in a function
    - 16777216 tags total in a file. 
    - Any number of branches, or switches, applied to an expression of an integer type.
    - A type which consists of only the following, in any combination, where the size of the type is a representable value of an implementation-defined integer type.  
        - An integer type, with a width at most 128.
        - A floating-point type, with a width at most 128.
        - A decimal floating-point type, with a width at most 64.
        - A vector type of one of the above, other than a decimal floating-point type, that has a total size of 64.
        - A pointer type
        - An array type
        - A function type, with up to 256 parameters
        - A structure or union type
        - A product type, with at most 256 elements.
    - A structure or union declaration with at most 512 members.
    - An identifier in a declaration, which consists of an optional root component, followed by a single non-special component that has at most 4096 characters.   

