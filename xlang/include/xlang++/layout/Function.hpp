#ifndef XLANG_LAYOUT_FUNCTION_HPP_2021_19_30_04
#define XLANG_LAYOUT_FUNCTION_HPP_2021_19_30_04

namespace lccc{
    struct bad_function_call
    {
    };

    template <typename Fn>
    struct function;

    /// A type Erased invocable object.
    /// Compatible with the layout presented in Rust RFC 2955, and by MIA.
    /// Contains one virtual member, auto call(Self*,Args&&...)->R
    template <typename R, typename... Args>
    struct function<R(Args...)>
    {
    private:
        struct vtable
        {
        public:
            std::size_t size;
            std::size_t align;
            void (*destroy)(void *);
            void (*dealloc)(void *);
            R(*call)(void *, Args...);
        };

        template <typename T>
        static const vtable *vtable_for()
        {
            static constexpr vtable vtbl = {
                sizeof(T),
                alignof(T),
                [](void *v) { static_cast<T *>(v)->~T(); },
                [](void *v) { ::operator delete(v); },
                [](void *v, Args... args) {
                    return std::invoke(*static_cast<T *>(v), std::forward<Args>(args)...);
                }};
            return &vtbl;
        }

        void *allocation;
        const vtable *vtbl;

    public:
        function() : allocation{}, vtbl{} {}
        function(std::nullptr_t) : function{} {}

        template <typename T, typename = std::enable_if_t<std::is_invocable_r_v<R, T, Args &&...>>>
        function(T &&t) : allocation{new T{t}}, vtbl{vtable_for<T>()} {}

        function(function &&f) noexcept : allocation{std::exchange(f.allocation, nullptr)}, vtbl{f.vtbl} {}

        function(const function &) = delete;

        void swap(function &f)
        {
            using std::swap;
            swap(allocation, f.allocation);
            swap(vtbl, f.vtbl);
        }

        friend void swap(function &f1, function &f2)
        {
            f1.swap(f2);
        }

        function &operator=(function &&f) noexcept
        {
            this->swap(f);
            return *this;
        }

        ~function()
        {
            if (allocation)
            {
                // invariant: if allocation is non-null, vtbl is as well.
                this->vtbl->destroy(allocation);
                this->vtbl->dealloc(allocation);
            }
        }

        R operator()(Args... args)
        {
            if (!allocation)
                throw bad_function_call{}; // Shut up clang-tidy, I can't use stdlib types that cross module bounderies
            return this->vtbl->call(allocation, std::forward<Args>(args)...);
        }
    };
}

#endif 
