# XLang Memory Model [memory]

1. XLang Programs execute according to a partial order, defined in the following sections. This order guarantees that single-threaded programs that do not handle signals execute in as-written order according to the xlang specification. 

## Execution Ordering [memory.execution]

1. There are a number of partial order relations that establish the order of execution of an xlang program. These are defined here. Each partial order is Assymetric.

2. The *weakly sequenced-before* relation describes the expresion order of a single thread. For each pair of expressions E and F, which are executed within a particular thread, such that either E and F are both not executed by a signal handler, or are both executed by the same signal handler, either E and F are the same expression, E is *weakly sequenced-before* F, or F is *weakly sequenced-before* E The *weakly sequenced-before* relation is transitive.  Thus, for three expressions A, B, and C, if A is *weakly sequenced-before* B and B is *weakly sequenced-before* C, then A is *weakly sequenced-before* C.
3. A *weakly sequenced-before* relation is precisely defined as program order as evaluated (considering branches, function calls, and block exits). 
4. The *sequenced-before* relation describes the memory access order of expressions on a single thread. The *sequenced-before* relation is transitive. Thus, for three expressions A, B, and C, if A is *sequenced-before* B and B is *sequenced-before* C, then A is *sequenced-before* C.
5. The *sequenced-before* relation is established as follows, between two expressions A and B if:
    - Neither A nor B are accesses with the `nontemporal` access class, and A is *weakly sequenced-before* B,
    - A and B are both access to the same memory location, and A is *weakly sequenced-before* B,
    - A is an access with the `nontemporal` access class, and B is a fence or sequence expression with the `nontemporal` access class, and A is *weakly sequenced-before* B, or
    - B is an access with the `nontemporal` access class, and A is a fence or sequence expression with the `nontemporal` access class, and A is *weakly sequeneced-before* B.
6. [Note: Absent nontemporal accesses, *weakly sequenced-before* is equivalent to *sequenced-before*. *weakly sequenced-before* describes the order of behaviour of an xlang program, *sequenced-before* describes the order that memory accesses become visible within a thread of execution]
7. The *happens-before* relation describes the order of expressions accross the entire program, including between two threads and between a thread and a signal handler. The *happens-before* relation is transitive. Thus, for three expressions A, B, and C, if A is *sequenced-before* B and B is *sequenced-before* C, then A is *sequenced-before* C.
8. An Expression A *happens-before* an expression B if:
    - A is *sequenced-before* B,
    - A *synchronizes-with* B. 
9. [Note: Absent signal handlers, on a single-threaded program, *happens-before* and *sequenced-before* are identical]

## Object Model [memory.object]

1. Values in memory are stored in objects. Objects have the following 4 properties:
* A type,
* A lifetime,
* A storage duration, and
* Storage within that duration.
2. Objects can either be subobjects of some other object or complete objects. An object `o` is a subobject of some other object `p` if:
* `o` is a base class of `p`,
* `o` is a field of `p`,
* `o` is an element of the array of `p`, or
* There exists an object `q`, such that `o` is a subobject of `q`, and `q` is a subobject of `p`.
3. An object `o` is a superobject of some other object `p`, if `p` is a subobject of `o`
3. Every addressible object has an address. For a complete object, that address is the address of the storage it occupies. Otherwise, it is some address within the same storage. Two objects may share the same address only if one provides storage for the other, or one is the object representation of the other. The address of an object is a multiple of the alignment requirement of that object's type.
4. An object may provide storage for another object if the second object is a subobject of the first, or the first is an integer or character type without a validity attribute that has a width of exactly 8, or an array thereof.
5. Every addressible object has an *object-representation*, which is an array of type `uint(8)` that is equal to the size of that object's type, and that begins at the address of that object. The object representation of subobjects overlap with that of their superobjects. For a `copyable` type, an object of that type may be exactly duplicated by copying the entire object or value representation of that object to suitably sized and aligned storage. The object representation of an object exists for the entirety of it's storage duration.
6. Any object-representation may be divided into that object's *value-representation* and padding. The *value-representation* is the entire portion of the object representation that determines the value of that object, and the rest of the object representation is padding. Padding bytes may occur between fields of an aggregate type, or following the last field of an aggregate type [Note: Any scalar type has no padding bytes.]
7. Two objects may be pointer-interconvertible with each other. In such a case, a pointer to one may be used as a suitably typed pointer to another, and both objects have the same address. An object `o` is pointer-interconvertible with an object `p` if:
* One is of a *standard-layout* aggregate type and the other is the first field of non-zero size or non-one alignment of that aggregate type in declaration order, or any preceeding field, or if the type has no fields, then any of it's base classes, 
* One is of a union type, and the other is any variant of that union, 
* One is a transparent structure type, and the other is any field of that structure type, or
* There exist an object `q`, such that `o` is *pointer-interconvertible* with `q` and `q` is *pointer-interconvertible* with `p`.
8. A most derived object is either a complete object, or a sub object that is not a base class subobject.
9. Two subobjects are sibling objects if both are direct, non-virtual base classes, of the same superobject, both are fields of the same structure type, both are elements of the same array, or both are virtual base class subobjects of the same most derived object.
10. An object `o` is a *reachable superobject* of some subobject `p` of `o`, if `p` is *pointer-interconvertible* with `o`, or `p` is a sibling object of some object `q` that is *pointer-interconvertible* with `o`, or `p` is a base class subobject of `o`.