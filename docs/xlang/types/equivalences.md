# Type Equivalence Relations [types.equivalences]

1. For the purposes of the xlang specification, a number of *type equivalences* are defined, which allow programs to relate certain types in particular circumstances. These equivalences are equivalence relations, that is, they are reflexive (Some type `T` satisfies each relation with `T`), they are symmetric (if some pair of types `T` and` U` satisfy the relation, then `U` and `T` also satisfy the relation), and transitve (if some pair of types `T` and `U` satisfy the relation, and some pair of types `U` and `V` satisfy the relation with the same `U`, then `T` and `V` satisfy the relation). 

2. There is a type equivalence called *compatibility* (and types satisfying this equivalance are called *comptaible types*), which is defined on two types `T` and `U`, and is satisfied if:
* `T` and `U` are the same type,
* `T` is a type, and `U` is a tagged type with a base type of `T`,
* `T` and `U` are both pointers `*K` and `*V` with the same pointer-width, such that `K` and `V` are *compatible types*,
* `T` and `U` are both *standard-layout* structure types with the same number of fields, and for each field in `T` and `U` in order:
    * if the field in `T` has type `K` and the field in `U` has type `V`, `K` and `V` are *compatible types*, and
   *  if the field in `T` is a bitfield with length `L`, then the field in `U` is a bitfield with length `L`, otherwise the field in `U` is not a bitfield,
* `T` and `U` are both union types, such that for each field `f` in `T` there exists a field `g` in `U`, and likewise for each field `f` in `U` there exists a field `g` in `T`, such that:
    * If `f` has type `K` and `g` has type `V`, `K` and `V` are *compatible types*, and
    * If `f` is a bitfield of length `L`, then `g` is a bitfield of length `L`, otherwise `g` is not a bitfield,
* `T` and `U` are both product types with the same number of elements, and, for each pair of elements `K` and `V` in order, `K` and `V` are *compatible types*,
* `T` is an enumeration type and `U` is its underlying type,
* `T` and `U` are both scalar types of the same kind, with the same width and if one has a vector size, the other has the same vector size,
* `T` and `U` are both aligned types with the same explicit alignment requiremnt, and the underlying types are *compatible*,
* `T` and `U` are both function types with the same tag and the same number of parameters, such that:
    * If the return type of `T` is `K` and of `U` is `V`, `K` and `V` are compatible,
    * If `T` is variadic, then `U` is variadic, otherwise `U` is not variadic,
    * For each pair of parameters in order `K` and `V`, `K` and `V` are compatible types, 
* `T` is a pointer to `void()` and `U` is a poiner to a character type of width 8, such that `T` and `U` has the same pointer width,
* `T` and `U` are both arary types with extent `N`, and underlying types `K` and `V`, such that `K` and `V` are compatible types,
* `T` and `U` are both slice types with underlying types `K` and `V`, such that `K` and `V` are compatible types.