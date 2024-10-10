# Access Classes [expr.class]

`access-class := <class-option>*`

1. Every memory operation may occur according to an access class. This access class directs how the entire operation is performed, as well as potential synchronization effects and obeservable side effects. This can also impact the value loaded/stored.

## Atomic [expr.atomic]

`class-option := atomic <ordering> <fail-ordering>?`

1. An access-class may contain an atomic class, to indicate that the entire memory operation is atomic, and to indicate the synchronization effects of the memory operation. 

2. Atomic operations do not cause data races (`[memory.race]`) when executed concurrently to other atomic operations. 

3. Every atomic operation that modifies an object M, referred to as an atomic store, executes in an unspecified total order unique to that memory location, known as the modification order of M. 

4. Every atomic operation that reads an object M, referred to as an atomic load A, reads some value that is stored by either some atomic store in the modification order of M, or some value stored by a non atomic operation B that modifies the object, if B *happens-before* A and there exists no store C such that C *happens-before* A.

5. Certain atomic operations both read and modify an object M, known as a read-modify-write. A read-modify-write operation A takes its value from the last atomic store in the modification order that preceeds it, B, or some non-atomic store C such that B *happens-before* C and C is *visible* to A. If there is no atomic store that preceeds the read-modify-write, then it takes its value from some non-atomic store C such that C is *visible* to A.

5. If an atomic store A to an object M *happens-before* an atomic store B to M, then A shall appear before B in the modification order of M.

6. [Note: This is referred to as write-write coherence]

7. If an atomic store A to an object M *happens-before* an atomic load B from M, then B shall take its value from A, from an atomic store C that appears later than A in the modification order of M, or from a non-atomic store D, such that A *happens-before* D and D is *visible* to B.

8. [Note: This is referred to as write-read coherence.]

9. If an atomic load A from an object M *happens-before* an atomic store B to M, then A shall take its value from some atomic store C that appears before B in the modification order of M, or from a non-atomic store D such that C *happens-before* D and D *happens-before* A.

10. [Note: This is referred to as read-write coherence.]

11. If an atomic load A from M that takes its value from an atomic store B *happens-before* an atomic load C from M, then C takes its value from B, some atomic store D that appears later than C in the modification order of M, or some non-atomic store E, such that A *happens-before* E and E *happens-before* B.

12. [Note: This is referred to as read-read coherence.]

13. A store is visible to a load if the store *happens-before* the load, or the atomic load takes its value from the atomic store, or some store to the same object, that appears later in the modification order.

14. The implementation should ensure that atomic stores on one thread of execution become visible to a load from the same object on another thread of execution in finite time. 

15. [Note: As long as the above rules are maintained, relaxed atomic loads may be reordered freely, and merged into adjacent relaxed loads.]

## Non-temporal [expr.nontemporal]

`class-option := nontemporal`

1. An access class may contain the `nontemporal` specifier. 

2. An access may not specify both the `nontemporal` access class and an atomic access class.

3. [Note: Fences and sequence instructions may specify both simultaneously]

4. `nontemporal` accesses do not guarantee visibility order with respect to accesses to other memory locations.

5. [Note: Nontemporal accesses are intended to hint to the processor that the program is unlikely to use the data loaded/stored in the future]

