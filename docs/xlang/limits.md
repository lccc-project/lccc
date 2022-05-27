# Implementation Limits [limits]

1. Every implementation should be capable of properly translating any well-formed program that does not violate any of the following limits, provided memory constraints are obeyed at runtime:
* 16777216 Declarations at module scope
* 256 nested scopes
* 1024 components in a declaration name
* 256 bytes in a basic name component
* 1024 bytes in a special name component
* 64 nested generic declarations
* 128 nested, in any combination, inline-aggregate types, product types, and function types
* 128 nested pointer types
* 16 nested, in any combination, `destroy` and `derive` expressions
* 32768 values on the operand stack
* 16384 rvalues on the operand stack
* 512 local variables within a function
* 512 items in the declared incoming stack of a target
* 1024 targets
* 1024 fields in the initializer-spec of an `aggregate` instruction
* 2048 concurrently live objects of non-zero size
* 16386 concurrently live objects
* 16777216 total objects created during the runtime of the program
* 16777216 instructions evaluated by a constant expression
* 2048 call instructions evaluated by a constant expression
* 512 objects created by the evaluation of a constant expression
* 128 constraints given in the `clobbers` specification of an assembly expression
* 32 targets listed in the `goto` specification of an assembly expression
* 1024 operands listed in an assembly expression
* 2048 operand specifier in the asm-str of an assembly expression
* 1024 nested expansions of a generic declaration
* 1024 recursive expansions of a generic declaration caused by a single substitution
* 4096 bytes in a byte string constant
* 4096 UTF-8 encoded bytes in a string constant
* 128 significant bits in an integer constant used in an expression
* 128 significant bits in an integer constant used in a type
* 4096 significant bits in an integer constant used in a static initializer
* 