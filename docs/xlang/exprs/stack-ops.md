# Stack Operations

1. Certain expressions, called stack operations, perform mostly untyped modification of the operand stack.

## pop

`expr := pop [<n:int>]`

Typechecking: `[..,T0,T1,...,Tn]`=>`[..]`

Operands: `[..,t0,t1,...,tn]`=>`[..]`

1. Removes the top n values from the stack. The removed values are discarded.

2. If no value is given n=1

## dup

`expr := dup [<n:int>]`

Typechecking: `[..,T0,T1,...,Tn]`=>`[..,T0,T1,...,Tn,T0,T1,...,Tn]`

Operands: `[..,t0,t1,...,tn]`=>`[..,t0,t1,...,tn,t0,t1,...,tn]`

1. Pops the top n values from the stack, then pushes those values in order twice. Each popped value appears in order, and then the first value popped is followed by the second set of pushed values in order.

2. If no value is given n=1


## pivot 

`expr := pivot [<n:int> [<m:int>]]`

Typechecking: `[..,S0,S1,...,Sm,T0,T1,...,Tn]`=>`[..,T0,T1,...,Tn,S0,S1,...,Sm]`

Operands: `[..,s0,s1,...,sm,t0,t1,...,tn]`=>`[..,t0,t1,...,tn,s0,s1,...,sm]`

1. Pops n values followed by m values, then pushes the first set of values popped followed by the second set.

2. The default value for n is 1 and m is n.

## select

`expr := select <n:int>`

Typechecking: `[..,T,T,...,T,I]`=>`[..,T]`

Operands: `[..,tn,...,t2,t1,i]`=>`[..,ti]`

1. Pops `i` from the stack, then `n` values, pushing the `ith` value popped.

2. `I` shall have an integer type. If `i` is negative, greater than `n`, or uninit, the behaviour is undefined.