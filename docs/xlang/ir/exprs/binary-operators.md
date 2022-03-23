# XLang Binary Operator Expressions

## Binary Operators

### Overflow Behaviour

Each Binary Operator applied to primitive (scalar or pointer) types can be applied with an optional overflow behaviour - this indicates the result of primitive operations on overflow (and also affects the typechecking of the operator).

The overflow behaviours are as follows:
* Wrap: Overflow causes wrapping Modulo 2^N (for integers/chars), or overflows to Infinity/NaN for floats.
* Checked: Same as Wrap, but a second result, indicating whether overflow occurs, is pushed.
* Trap: Overflow causes the program to terminate abnormally (unless some unspecified signal is handled or ignored) in a backend-specific way
* Unchecked: Overflow produces `uninit`.

With the exception of `Checked`, the overflow behaviours do not alter the stack transition of binary operators. For `Checked`, the stack transition is `[..,T,T]`->`[..,T,uint(1)]`. 

`Checked` and `Trap` cannot be applied to arithmetic where either operand is a pointer type. 

Trapping and Unchecked operations that do not overflow are permitted in a constant evaluation. Wrapping and Checked operations are always permitted in constant evaluation.

Overflow on integers and character types are at signed (+/-2^(N-1)) or unsigned (+2^N) bounderies depending on the signedness of the type. Overflow on floats is on the maximum and minimum values to Infinity, or when an operation on non-NaN elements produces NaN. 

#### Uninit Behaviour for Checked and Trap

Trapping and Checked operations behave specially when either operand is `uninit`. In the case of checked, both results are `uninit`, even if the operation statically cannot overflow (IE. unsigned division by `nonzero` integer). In the case of trap, the behaviour is undefined.

This special behaviour is not observed with wrapping or unchecked operations: both simply yield uninit.


### Add/Sub

Syntax: `add [overflow-behaviour]`, `sub [overflow-behaviour]`

Stack Transitions:
1. `[..,T,T]`->`[..,T]` (wrap, trap, or unchecked)
2. `[..,T,T]`->`[..,T,uint(1)]` (checked)
3. `[..,P,T]`->`[..,P]` or `[..,T,P]`->`[..,P]` if `P` is a pointer type and `T` is an integer type
4. `[..,P,P]`->`[..,uint(<sizebits>)]`  if `P` is a pointer type. (sub only)

Operands:
1. `[..,a,b]`->`[..,r]` (wrap, trap, or unchecked)
2. `[..,a,b]`->`[..,r,v]` (checked)


Behaviour:
- Forms 1 and 2: Computes the result of `a+b` (add) or `a-b` (sub).
- Form 3: Computes the result of `a+b` or `a-b` on pointers. For `unchecked`, both the pointer operand and the result must be inbounds of the same array, and must be mutually reachable from each other.
