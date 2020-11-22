# Expressions [expr]

## Expression Stack [expr.stack]

1. All expressions defined by this specification operate by pulling it's operands from a fictious stack, known as the expression stack, and by pushing it's results to the same stack.
2. The items present on the stack may be divided into two categories: lvalue, and rvalue. An rvalue item represents an actual value of a particular type, whereas an lvalue represents a place where values may be stored.
3. This document will indicate the required operand types by `[..,T1,T2,...,TN]` where `T1`, `T2`, ..., `TN` represent *N* types on the expression stack, and `..` indicates all items below `T1` on the expresison stack. If `T`*`k`* (`1<=k<=N`) starts with `lvalue` followed by the name of a type, it represents an lvalue item, otherwise it represents an rvalue item. A program shall not require an expression stack contain items which are not present at the head of the stack. 
4. This document will indicate operands by `[..,op1,op2,...opN]`, where `op1`, `op2`, ..., `opN` represent *N* items on the expression stack. 
5. A type variable, such as `T`, `U`, or `V` may appear in place of a type name above. Each use of such a type variable appears in the "Type checking" portion of an expression represent some type. All occurances of a particular type variable refer to the same type

## Constant Values [expr.const]

1. Syntax:

```
"const" <value>
```

2. Type checking: `[..]`->`[..,T]`

3. Operands: `[..]`->`[..,val]`

4. Certain expressions may simply result in a constant value, known at compile time. Such expressions are known as constants.

5. The result of a constant expression is an rvalue, of a type corresponding to the constant.

6. Multiple evaluations of a constant value produce the same result. 

## Function Call [expr.call]

1. Syntax:
```
"call" <fn-type>
```

2. Type checking, where `fn-type` is `fn(A1,A2,...,AN)->R`
    - Form (1): `[..,*fn(A1,A2,...,AN)->R,A1,A2,...,AN]`->`[..,R]`
    - Form (2): `[..,lvalue fn(A1,A2,...,AN)->R,A1,A2,...,AN]`->`[..,R]`
    - Form (3): `[..,F,A1,A2,...,AN]`->`[..,R]`

3. Operands: `[..,f,arg1,arg2,...argN]`->`[..,ret]`.

4. Form (1): Calls the function pointed to by `f` with each argument. If any argument is an uninitialized value of a type that has a validity-requirement `[value.validity]`, the behaviour is undefined. If `f` is an uninitialized value, or does not point to a function that has the signature `fn(A'1,A'2,...,A'N)->R` where each type in `A'1`, `A'2`, ..., `A'N`, `R'` is compatible with the corresponding type in `A1`, `A2`, ..., `AN`, `R`, the behaviour is undefined. 

5. Form (2): Calls the function designated by `f` with each argument. If any argument is an uninitialized value of a type that has a validity-requirement `[value.validity]`, the behaviour is undefined. If `f` is an uninitialized value, or does not point to a function that has the signature `fn(A'1,A'2,...,A'N)->R` where each type in `A'1`, `A'2`, ..., `A'N`, `R'` is compatible with the corresponding type in `A1`, `A2`, ..., `AN`, `R`, the behaviour is undefined. 

6. Form (3): 
    - if `F` is a pointer-to-function type, the program is ill-formed.
    - Otherwise, if the identifier `F::#"operator()"`, possibly with some generic arguments, can be named, and has type `fn(F,A1,A2,...,AN)->R`, the expression is evaluated as though a constant value, which is the global address of that function is present on the expression stack, before `[F,A1,A2,...,AN]`, and that function is called by `call fn(F,A1,A2,...,AN)->R`. The behaviour is undefined if the resulting expression would use this same form this of expression.
    - Otherwise, if `F` is a pointer to some complete type `G`, which has the attribute `ref`, and the identifier `G::#"operator()"`, possibly with some generic arguments, can be named, and has type `fn(F,A1,A2,...,AN)->R`, the expression is evaluated as though a constant value, which is the global address of that function is present on the expression stack, before `[F,A1,A2,...,AN]`, and that function is called by `call fn(F,A1,A2,...,AN)->R`. The behaviour is undefined if the resulting expression would use this same form this of expression.
    - Otherwise, the program is ill-formed. 

### Member Access [expr.member]

1. Syntax:
    - Form (1):
    ```
    "member" "struct" <identifier> 
    ```
    - Form (2):
    ```
    "member" "indirect" <identifier>
    ```

2. Type Checking:
    - Form (1):`[..,lvalue T]`->`[..,lvalue U]`
    - Form (2): `[..,T]`->`[..,*U]`

3. Operands: `[..,t]` -> `[..,m]`

4. Form (1): Accesses the named member of the struct `T`. Results in an lvalue designating member named by *identifier* in the object designated by `t`. The program is ill-formed if `T` is not a structure, union, or product type, or if *identifier* is not a nameable member of `T`. 

5. Form (2): If `T` is a pointer type to a structure that has a nameable member *identifier*, then has the same effect as the sequence of expressions, `indirect`, `member struct <identifier>`, `address_of`, except no indirection takes place. 
6. Form (2): Otherwise, if `T::#"operator->"()` is nameable, then that function is called `[expr.call]` with the signature `fn(T)->K`, where `K` is a complete object type such that the call is well-formed (if multiple such types exist, the program is ill-formed. No diagnostic is required), and then the same `member indirect` expression is applied to the result, recursively. 
7. Form (2): Otherwise, if `T` is a pointer type `*ref S`, and `S::#"operator->"()` is nameable, then that function is called `[expr.call]` with the signature `fn(T)->K` where `K` is a complete object type such that the call is well-formed (if multiple such types exist, the program is ill-formed. No diagnostic is required), and then the same `member indirect` expression is applied to the result, recursively. 
8. Otherwise, the program is ill-formed. 

### Local Variables [expr.local]

1. Syntax:
```
"local" "_" <number>
```

2. Type Checking: `[..]`=>`[..,lvalue T]`

3. Pushes an lvalue designating the local variable `_`*`number`*, which has type `T`. If the enclosing function does not declare the local variable, the program is ill-formed. If the storage of the named local variable is not active, the behaviour is undefined. 

### Value Class Conversion [expr.as_rvalue]

1. Syntax:
```
"as_rvalue" <access-class>
```

2. Type Checking `[..,lvalue T]`=>`[..,T]`

3. Operands: `[..,l]`=>`[..,t]`

4. Converts an lvalue into an rvalue. The resulting value is the value of the object designated by `l`. The behaviour is undefined if no such object exists (but see `[ptr.access]` for operations inside the *object-representation* of another object). 

5. The operation of `as_rvalue` is according to `access-class`. If `access-class` contains `atomic`, `atomic acquire`, `atomic acqrel`, or `atomic seqcst`, the access is an atomic operation (if it contains `atomic release`, the program is ill-formed). If `access-class` contains `atomic acquire`, `atomic acqrel`, or `atomic seqcst`, the access is an acquire operation. If `access-class` contains `volatile`, the access is a volatile access. If `access-class` contains `volatile` or `freeze`, and the value read is `uninitialized`, the resulting value is an unspecified value of the type `T`, which is neither `uninitialized` or `invalid` (but may be an invalid value of the type `T`).

6. If the resulting value is `invalid` or not a valid value of the type `T`, the behaviour is undefined. If `T` has any validity requirements (`[value.validity]`), and the resulting value is `uninitialized`, the behaviour is undefined. 

### Conversions [expr.convert]

1. Syntax:
```
"convert" ("strong"/"weak") <type> 
```

2. Type Checking: `[..,T]`=>`[..,type]`

3. Operands: `[..,t]`=>`[..,r]`

3. Performs an implicit (weak), or explicit (strong) type conversion. 

4. If `T` and *type* are both integer types, the result is the same value of `type` equal to `t`, if it is representable as a value of `type`. Otherwise, the result is such a value with is representable as `type` congruent to `t` module `2`<sup>*`N`*</sup>, where N is the width of `T`. If the value is not a valid value of `type`, the behaviour is undefined. 

5. If `T` and *type* are both floating-point types, or `T` is an integer type, and *type* is a floating-point type, the result is the value `t` represented as *type*. If the result cannot be represented exactly as a value of *type*, it is rounded according to the current rounding mode. If the result, after rounding, is larger than the maximum representable value, or smaller than the minimum representable value, the result is positive or negative infinity respectively. 

6. If `T` is a floating-point type, and *type* is an integer type, the result is the value `t`, truncated to a value of the *type*. If the result is larger than the maximum value of the integer type, or `t` is Infinity, the result is the maximum value of the type. If the result is smaller than the minimum value of the type, or `t` is -Infinity, the result is the minimum value. If the `t` is `NaN`, the result is 0. 

7. If `T` and *type* are both vector types with the same vector size, the result is the same as converting each element of the vector according to the above rules.

#### Reinterpret Conversion [expr.reinterpret]

1. Syntax:
```
"convert" "reinterpret" <type>
```

2. Type Checking: `[..,T]`->`[..,type]`

3. Operands `[..,t]`->`[..,r]`

4. Performs a conversion by reinterpreting the value `t` as a value of *type*.

5. If `T` and *type* are both integer types, the result is the same as performing a `strong` conversion from `T` to *type*. 

6. If either `T` and *type* are both pointer types, or one is a pointer type, and the other is an integer type, `t` is converted to the result according to the rules described by `[ptr.convert]`.

### Unary Operators [expr.unary]

1. Syntax:

```
("neg" / "bneg" / "pos" / "umn")
```
