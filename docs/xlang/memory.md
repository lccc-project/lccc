# XLang Memory Model [memory]

1. XLang Programs execute according to a partial order, defined in the following sections. This order guarantees that single-threaded programs that do not handle signals execute in as-written order according to the xlang specification. 

2. There are 3 binary relations defined by the xlang specification, given below. Each of these relations are Asymetric, and are transitive other than "weekly happens-before"
    * The sequenced-before relation describes the order of single-threaded programs. For two expressions on the same thread of execution, E and F, either E is sequenced-before F, F is sequenced-before E, E and F are the same expression, or at least one of E and F executes from an asynchronous signal handler and the other does not execute from the same asynchronous signal handler.
        * [Note: In a single-threaded program, without signal handlers, this relation is equivalent to the weekly happens-before and happens-before relation]
    * The weekly happens-before relation is one of two relations that describes the order of a multi-threaded program. If two expression E and F exist, such that E *happens-before* F, then E *weekly happens-before* F. 
    * The happens-before relation is the second relation that describe the order of a multithreaded program. If two expression E and F exist, such that E is *sequenced-before* F, then E *happens-before* F. The happens-before relation is established between threads by synchronization edges.
    * [Note: in a single-threaded program, all three of these relations are equivalent]

3. Certain expression *synchronize-with* other expression. These expressions guarantee that a happens-before relation exists between these expressions in some order (which order is unspecified, unless otherwise guaranteed). 


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