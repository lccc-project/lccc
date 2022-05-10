# Assembly Expression

Syntax: `expr := asm <option>* "asm-str" <special-constriants>* [<constraint>...] [<late>? <constraint> => <type>...]`

Type Checking: `[..,inputs...]`=>`[..,outputs...]`

1. Enables performing machine-specific operations by generating specific assembly.

## Assembly options

1. Each Inline Assembly statement may be parameterized by zero or more options, given as follows. These modify the behaviour of the operation

```
options := pure
```

2. Indicates that the assembly expression contains no operations that affect any memory (including memory pointed to by a pointer operand), and that the same asm-str with the same inputs yields the same outputs..

```
options := transparent
```

3. Indicates that the program does not modify any code executed by the assembly expression. This permits optimizers to analyze and assume the behaviour of content of the `asm-str` beyond the options, inputs, outputs, and clobbers specified.

```
options := nostack
```

4. Indicates that the assembly expression does not modify any memory covered by the stack, except through pointer operands.

```
option := nomem
```

5. Indicates that the assembly expression does not modify any memory, except through pointer operands (this includes leaked addresses, and static variables).

```
options := noexit
```

6. Indicates that the assembly expression does not complete. Note that this has no effect on type checking - expressions following the assembly expression are still type checked.

```
options := syntax(<name>)
```

7. Indicates that the asm-str is written in the given named syntax (either an identifier or a string literal). What syntaxes are available and the default syntax are target-specific

```
options := class(<class>)
```

8. Indicates that the asm-str operates according to `class`. The atomic access class incidates that the assembly statement performs an atomic or synchronization operation consistent with `class`. `volatile` indicates that the assembly expression performs volatile accesses to memory or has other side effects. `nontemporal` and `freeze` have machine-specific semantics if they appear.
9. The `volatile` access class cannot appear in a `pure` assembly expression. The behaviour of such an assembly expression is undefined. 
10. If no atomic access class appears in `class` or no `class` is provided, then it is machine-specific whether the assembly expression is permitted to perform an atomic operation, but such an atomic operation does not have any synchronization effects. If the assembly-statement contains an atomic class stronger than relaxed, and does not perform any memory operations, it acts as a `fence` with that access class. If the order is relaxed, the behaviour of such an assembly expression is undefined. Whether or not the `fail relaxed` modifier has any effect is machine-specific.
11. [Note: The assembly expression may perform operations that have stronger synchronization semantics in the target system than `class` specifies. This does not alter the synchronization effects on the XIR abstract machine. For example, a `mov` x86 instruction may still cause a data race if the assembly expression isn't indicated as atomic.]

## Outputs

`special-constraints := goto(<target>...)`

1. the `goto` specification may appear following the asm-str and preceeding the input and output specifications. This lists targets that the asm-expr may (but is not guaranteed to) branch to. If the asm-expr branches to any target other than one listed in this, or it transfers control into any asm-expr that exits without transfering control back to the current asm-expr, the behaviour is undefined.

2. `special-constraints: clobbers(<constraint>...)`

2. The `clobbers` specification may appear following the asm-str and preceeding the input and output specifications. See constraints for details about clobbers.

## Constraints

`constraint := <string-literal-without-escapes> | <identifier> | (<constraint>)`

1. Each assembly statement can have zero or more input, output, and clobber constraints.
2. Each input, output, and clobber contains a constraint which is either a string literal or identifier. Each constraint is machine-specific, but the constraints "cc" and "memory" may appear in the clobbers list to incidate that the assembly block clobbers either all memory or the condition code variable (which may store, for example, the cached results of previous comparisons). Neither "memory" nor "cc" may appear as a constraint in the input or output specification.
3. The use of escape sequences in constraint names given as string literals is unspecified. If a program contains a string as a constraint name that does not identically match either "cc" or "memory" but is equivalent after converting escape sequences, the program is ill-formed, no diagnostic required. If the string after evaluating escapes is not valid UTF-8, the program is ill-formed, no diagnostic required. 
3. Each input constraint corresponds to a value, in order, from the head of the stack before the asm-str, with the nth last input constraint corresponding to the nth value popped. The valid types for each constraint name is machine-specific, but each input type shall be a scalar type or a pointer type. 
4. Each output constraint corresponds to a value, in order, pushed to the head of the stack after the asm-str, with the nth output constraint corresponding to the nth value pushed. The same types valid for input constraints are valid for the same named output constraint.
5. Each clobbers constraint correponds to some location that is modified by the assembly expression. The implementation cannot rely on the value of the location designated by the constraint.
6. The memory clobber indicates that the program cannot rely on the value of any object given to the assembly expression in a writable pointer, accessible from a leaked writable pointer, or accessible via any mutable static reachable from the current module. It is incompatible with the `nomem` and `pure` options - the behaviour of such an assembly expression is undefined. 
7. Certain constraints refer to specific machine registers. If a particular machine register is named multiple times as an input, or multiple times as an output, the behaviour is undefined. Additionally, certain machine registers may overlap with certain other machine registers. If two overlapping registers are both specified as an input or both specified as an output, the behaviour is undefined

`output := late? <constraint> => <type>`

7. An output operand has both a constraint name and a type. The constraint indicates the location the corresponding value can be located at, and the type gives the type of that value. Additionally, an output may specify the `late` qualifier. If this qualifier is given, then the implementation may assume that the constraint is only modified to produce the specified output after all inputs are read by the assembly expression.
8. A program that specifies an unparenthesised output constraint with the name `late` given as an identifier is ill-formed. The identifier `late` given in an output constraint is the `late` specifier, rather than a constraint name. 

### Assembly String

`asm-str := \"([^\"{}]\\|<limited-escape-sequence>|<operand-specifier>|"{{"|"}}")*\"`

1. The asm-str is a machine-specific string to be interpreted by the codegen in a codegen-specific manner according to the options specified. The form of the asm-str is unspecified, except that operand specifiers (specified) below are replaced with the corresponding input or output operand. 

Syntax: `operand-specifier := "{"<integer>?{i,o,t}<class-specifier>?"}"`
`class-specifier := ":"[a-zA-Z0-9]*`

2. Each operand specifier contains 3 parts: an optional position, an input or output specifier, and an optional class specifier. The position is the zero indexed position in the corresponding (input or output) operand list and, if omitted, is filled with one more than the position last used for an operand specifier from the same list without a position indicator. An operand specifier with a position specifier that ends with `i` is an input operand specifier and refers to an operand from the input list. An operand specifier with a position specifier that ends with `o` is an output operand specifier and refers to an operand from the output list. The class-specifier indicates modifications to the operands and are machine-specific.  An operand specifier with a position specifier that ends in `t` is a target specifier, and refers to a target given in the `goto` specification, if any. If any class specifier appears, the program is ill-formed, no diagnostic required.

3. If a malformed operand-specifier appears within the asm-str, the program is ill-formed, no diagnostic required.

`limited-escape-sequence := <escape-sequence>`

4. The escape sequences allowed for asm-str shall only produce valid UTF-8. If an escape sequence expands either `{` or `}`, the program is ill-formed, no diagnostic required.