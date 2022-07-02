pub use xlang_abi as __abi;

#[macro_export]
macro_rules! simple_path{
    (:: $($id:ident)::*) => {
        $crate::Path{
            components: $crate::macros::__abi::vec![$crate::PathComponent::Root, $($crate::PathComponent::Text(::core::stringify!($id))),*]
        }
    };

    ($($id:ident)::*) => {
        $crate::Path{
            components: $crate::macros::__abi::vec![$($crate::PathComponent::Text(::core::stringify!($id).into())),*]
        }
    }
}
