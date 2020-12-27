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

8. If the incoming value is uninit, the result is uninit

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

7. If `t` is uninit, the result is uninit. 

### Branch [expr.branch]

1. Syntax:
```
condition := always / equal / less / greater / less_or_equal / greater_or_equal / not_equal / never / zero / nonzero / negative / positive / negative_or_zero / positive_or_zero
expr /= "branch" [<condition>] "@"<number>
```

2. Type checking: Given the target `@`*`n`* for a branch to `@`*`n`* recieves `[T...]`, and each `Ti` in `T...` may be an `lvalue` or an `rvalue` operand, and `N` is an integer. 
```
If the branch is taken: [..,T...,int(N)] => [T...]
If the branch is not taken: [..,int(N)] => [..]
If the branch is taken: [..,T...,uint(N)] => [T...]
If the branch is not taken: [..,uint(N)] => [..]
```

3. Operands:
```
[..,t...,v] => [t...] or [..,t...]
```

4. Branches to the target if `v` satisfies the condition. 

5. If the condition is satisfied, the top `n` items from the expression stack are preserved by the branch, where `n` is the number of the recieving items in the `stack-item-list` of the target, and other operands are disguarded. If any disgarded item has *non-trivial destruction*, whether or not the destructor is performed on the value is unspecified. After the branch, the next expression to be evaluated is the expression immediately following the target. 

5. If  the condition is not satisfied, the next expression to be evaluated is the expression immediately following the branch expression. The operand stack is not modified (after `v` is removed from it).

6. The conditions are satisfied as follows:
    * The condition `always` (which is default if omitted) is always satisfied (the branch is always taken)
    * The condition `equal` or `zero` is satisfied if `v` has the value `0`.
    * The condition `less` or `negative` is satisfied if `v` has a value less than `0` (note: this condition is never satisfied if `v` is an unsigned integer type)
    * The condition `greater` or `positive` is satisfied if `v` has a value greater than `0`.
    * The condition `less_or_equal` or `negative_or_zero` is satisfied if `v` has a value less than or equal to `0`
    * The condition `greater_or_equal` or `positive_or_zero` is satisfied if `v` has a value greater than or equal to `0` (note: this coniditon is always satisfied for unsigned integer types)
    * The condition `never` is not satisfied (note that it still performs typechecking of the target)

7. If the execution of a branch expression crosses the end of a tag, all pointers bounded by that tag are invalidated. 

8. If the execution of a branch expression enters the storage region for a local variable, the storage for the object begins, and is initialized with the value `uninit`.

9. If the execution of a branch expression leaves the storage region for a local variable, the storage for the local ends, and all pointers to that object are invalidated. If any such local variable has *non trivial destruction*, the destructor operation is performed on that local variable prior to the branch being taken. 

10. _Note 1 -  When performing type checking, expressions following the branch directly are typechecked against all items on the expression stack. Expressions following the target are typechecked against the expression stack specification at the target - End Note_

