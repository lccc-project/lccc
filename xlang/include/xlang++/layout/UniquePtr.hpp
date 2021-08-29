#ifndef XLANG_LAYOUT_UNIQUEPTR_HPP_2021_08_22_19_43_49
#define XLANG_LAYOUT_UNIQUEPTR_HPP_2021_08_22_19_43_49

#include <xlang++/layout/TypeTraits.hpp>
#include <utility>

namespace lccc{
    template <typename T>
    struct unique_ptr
    {
    private:
        T *_m_ptr;
        void (*_m_delete)(void *);
        static void do_delete(void *v)
        {
            delete (T *)v;
        }

    public:
        unique_ptr(T *ptr, void (*deleter)(void *) = do_delete) : _m_ptr{ptr}, _m_delete{deleter} {}
        unique_ptr() : _m_ptr{}, _m_delete{do_delete} {}
        unique_ptr(unique_ptr &&u) noexcept : _m_ptr{std::exchange(u._m_ptr, nullptr)}, _m_delete{u._m_delete} {}

        template <typename U, typename = std::enable_if_t<std::is_convertible_v<U *, T *>>>
        unique_ptr(unique_ptr<U> &&ptr) : _m_ptr{ptr.release()}, _m_delete(do_delete) {}
        unique_ptr(const unique_ptr &) = delete;
        ~unique_ptr()
        {
            if (_m_ptr)
                _m_delete(_m_ptr);
        }

        void swap(unique_ptr &other) noexcept
        {
            std::swap(this->_m_ptr, other._m_ptr);
            std::swap(this->_m_delete, other._m_delete);
        }

        friend void swap(unique_ptr &a, unique_ptr &b) noexcept
        {
            a.swap(b);
        }

        unique_ptr &operator=(unique_ptr u) noexcept
        {
            swap(u);
            return *this;
        }

        T &operator*()
        {
            return *this->_m_ptr;
        }
        const T &operator*() const
        {
            return *this->_m_ptr;
        }

        T *operator->()
        {
            return this->_m_ptr;
        }

        const T *operator->() const
        {
            return this->_m_ptr;
        }

        void reset(T *n = nullptr)
        {
            if ((n = std::exchange(this->_m_ptr, n)))
                this->_m_delete(n);
        }

        T* release(){
            return std::exchange(this->_m_ptr,nullptr);
        }
        
        T* get(){
            return this->_m_ptr;
        }

        const T* get()const{
            return this->_m_ptr;
        }
    };

    template<typename T,typename... Args,typename=std::enable_if_t<std::is_constructible_v<T,Args&&...>>> lccc::unique_ptr<T> make_unique(Args&&... args){
        return {new T(std::forward<Args>(args)...)};
    }

}
#endif /* XLANG_LAYOUT_UNIQUEPTR_HPP_2021_08_22_19_43_49 */