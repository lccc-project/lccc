#ifndef XLANG_LAYOUT_PRIMITIVES_HPP_2021_08_22_17_42_38
#define XLANG_LAYOUT_PRIMITIVES_HPP_2021_08_22_17_42_38

#include <cstddef>
#include <cstdint>
#include <memory>
#include <xlang++/Properties.h>

namespace lccc{
    using std::uint8_t;
    using std::uint16_t;
    using std::uint32_t;
    using std::uint64_t;
    using std::int64_t;
    using std::int8_t;
    using std::int16_t;
    using std::int32_t;

    using std::size_t;
    using std::ptrdiff_t;

    template<typename T,typename=decltype(std::pointer_traits<T>::to_address(std::declval<T>()))>
        auto* to_address(T val){
            return std::pointer_traits<T>::to_address(std::move(val));
        }
    
    template<typename T>
        auto* to_address(T val){
            return lccc::to_address(val.operator->());
        }

    template<typename T> T* to_address(T* val){
        static_assert(!std::is_function_v<T>);
        return val;
    }

    static_assert(sizeof(std::size_t)==4||sizeof(std::size_t)==8);
    constexpr std::size_t xlang_hash_prime = sizeof(std::size_t)==4?16777619:sizeof(std::size_t)==8?1099511628211:0;

    extern"C"{
        XLANG_API void* xlang_allocate(std::size_t) noexcept;
        XLANG_API void* xlang_allocate_aligned(std::size_t,std::size_t) noexcept;
        XLANG_API void xlang_deallocate(void* v)noexcept;
        XLANG_API void xlang_deallocate_aligned(void* v,std::size_t)noexcept;
        /// Hashes a scalar value
        /// Preconditions: v must point to an object which is at least z bytes
        XLANG_API std::size_t xlang_hash_scalar(const void* v,std::size_t z);
        XLANG_API extern const char xlang_hash_seed;
    }
}

#endif /* XLANG_LAYOUT_PRIMITIVES_HPP_2021_08_22_17_42_38 */