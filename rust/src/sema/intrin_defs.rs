def_intrinsics! {
    unsafe intrin __builtin_unreachable() -> !;
    intrin __builtin_abort() -> !;
    intrin impl_id() -> &str;
    unsafe intrin __builtin_allocate<type>(Var<0>) -> *mut u8;
    unsafe intrin __builtin_deallocate<type>(Var<0>, *mut u8) -> ();
    intrin type_id<type>() -> (*const u8, usize);
    intrin type_name<type>() -> &str;
    intrin destroy_at<type>(*mut Var<0>) -> ();
    intrin discriminant<type, type>(&Var<0>) -> Var<1>;
    unsafe intrin transmute<type, type>(Var<0>) -> Var<1>;
    intrin black_box<type>(Var<0>) -> Var<1>;

    unsafe intrin construct_in_place<type, type, type>(*mut Var<0>, Var<1>, Var<2>) -> ();

    unsafe intrin __builtin_read<type>(*const Var<0>) -> Var<0>;
    unsafe intrin __builtin_read_freeze<type>(*const Var<0>) -> Var<0>;
    unsafe intrin __builtin_read_volatile<type>(*const Var<0>) -> Var<0>;
    unsafe intrin __builtin_write<type>(*mut Var<0>,Var<0>) -> ();
    unsafe intrin __builtin_write_volatile<type>(*mut Var<0>, Var<0>) -> ();

    intrin __builtin_size_of<type>() -> usize;
    intrin __builtin_align_of<type>() -> usize;

    intrin __builtin_size_of_val<type>(*const Var<0>) -> usize;
    intrin __builtin_align_of_val<type>(*const Var<0>) -> usize;

    intrin __builtin_likely(bool) -> bool;
    intrin __builtin_unlikely(bool) -> bool;
}
