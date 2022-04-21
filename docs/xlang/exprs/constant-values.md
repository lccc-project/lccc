# Constant Values

Syntax: `expr := const <value>`

Type Checking: `[..]`->`[..,T]`

1. Certain expressions produce a value known at compile time, known as a constant expression. These expressions are precomputed by translator, and may be used in positions that require a known value (such as in type signatures).


### Undefined Value

#### Invalid

Syntax: `value := undef invalid <type>`

Type Checking: `[..]`->`[..,T]`

1. Produces the canonical invalid value of `T`. The behaviour of a program that produces this value of any type is undefined.

2. In addition to the `undef invalid` value, this value is produced whenever a value that violates a scalar validity or pointer trivial validity attribute is produced, in the following circumstances. Note that all of these circumstances lead the a program with undefined behaviour:
* An `as_rvalue` expression that loads such an invalid value of the type of the lvalue operand,
* An `assign` expression that stores such an invalid value of the type of the lvalue operand (invalid is stored),
* A `call` expression, for which one of the argument is such an invalid value according to the corresponding formal parameter of the call-type,
* An `exit` expression, which returns such an invalid value according to the return type of the call site or of the definition of the function being called.

#### Uninit

Syntax: `value: undef uninit <type>`

Type Checking: `[..]`->`[..,T]`