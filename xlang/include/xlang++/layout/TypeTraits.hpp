#ifndef XLANG_LAYOUT_TYPE_TRAITS_HPP_2021_08_22_19_37_43
#define XLANG_LAYOUT_TYPE_TRAITS_HPP_2021_08_22_19_37_43

#include <type_traits>

namespace lccc{
    template <typename T>
    struct type_identity
    {
        using type = T;
    };

    template <typename T>
    using type_identity_t = typename type_identity<T>::type;

    template <typename T>
    struct remove_cvref : std::remove_cv<std::remove_reference_t<T>>
    {
    };

    template <typename T>
    using remove_cvref_t = typename remove_cvref<T>::type;

    template<typename T,typename U>
    struct copy_cv : type_identity<T>{};

    template<typename T,typename U> struct copy_cv<T,const U> : type_identity<const T>{};
    template<typename T,typename U> struct copy_cv<T,volatile U> : type_identity<volatile T>{};
    template<typename T,typename U> struct copy_cv<T,const volatile U> : type_identity<const volatile T>{};

    template<typename T,typename U> using copy_cv_t = typename copy_cv<T,U>::type;
}
#endif