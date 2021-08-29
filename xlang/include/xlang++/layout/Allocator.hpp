#ifndef XLANG_LAYOUT_ALLOCATOR_HPP_2021_08_28_20_43_07
#define XLANG_LAYOUT_ALLOCATOR_HPP_2021_08_28_20_43_07

#include <xlang++/layout/Primitives.hpp>

namespace lccc {
    struct bad_alloc{};

    template<typename T> struct allocator{
        using value_type = T; 
        T* allocate(std::size_t n){
            T* t;
            if constexpr(alignof(T)<=alignof(std::max_align_t))
                t = static_cast<T*>(xlang_allocate(sizeof(T)*n));
            else
                t = static_cast<T*>(xlang_allocate_aligned(sizeof(T)*n,alignof(T)));
            if(t)
                return t;
            else
                throw lccc::bad_alloc{};
        }
        void deallocate(T* t,std::size_t) noexcept{
            if constexpr(alignof(T)<=alignof(std::max_align_t))
                xlang_deallocate(t);
            else
                xlang_deallocate_aligned(t,alignof(T));
        }
    };
}

#endif /* XLANG_LAYOUT_ALLOCATOR_HPP_2021_08_28_20_43_07 */