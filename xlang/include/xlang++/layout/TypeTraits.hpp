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
}
#endif