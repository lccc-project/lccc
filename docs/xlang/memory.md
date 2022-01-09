# XLang Memory Model [memory]

1. XLang Programs execute according to a partial order, defined in the following sections. This order guarantees that single-threaded programs that do not handle signals execute in as-written order according to the xlang specification. 

2. There are 3 binary relations defined by the xlang specification, given below. Each of these relations are Asymetric, and are transitive other than "weekly happens-before"
    * The sequenced-before relation describes the order of single-threaded programs. For two expressions on the same thread of execution, E and F, either E is sequenced-before F, F is sequenced-before E, or E and F are the same expression 
        * [Note: this includes an expression that executes in a signal handler on the same thread of execution, see [memory.signal].]
        * [Note: In a single-thread program, this relation is equivalent to the weekly happens-before and happens-before relation]
    * The weekly happens-before relation is one of two relations that describes the order of a multi-threaded program. If two expression E and F exist, such that E *happens-before* F, then E *weekly happens-before* F. 
    * The happens-before relation is the second relation that describe the order of a multithreaded program. If two expression E and F exist, such that E is *sequenced-before* F, then E *happens-before* F. The happens-before relation is established between threads by synchronization edges.
    * [Note: in a single-threaded program, all three of these relations are equivalent]

3. Certain expression *synchronize-with* other expression. These expressions guarantee that a happens-before relation exists between these expressions in some order (which order is unspecified, unless otherwise guaranteed). 

## 