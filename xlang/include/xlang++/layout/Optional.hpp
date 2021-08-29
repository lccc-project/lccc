#ifndef XLANG_LAYOUT_OPTIONAL_HPP_2021_08_28_20_47_23
#define XLANG_LAYOUT_OPTIONAL_HPP_2021_08_28_20_47_23
#include <optional>
#include <xlang++/layout/TypeTraits.hpp>
#include <functional>
#include <iterator>
#include <type_traits>

namespace lccc {
    namespace _detail{
        template<typename T> struct _option_storage{
            bool _engaged;
            alignas(T) unsigned char _m_storage[sizeof(T)];
            
            template<typename... Ts> void construct_from(Ts&&... ts){
                ::new(_m_storage) T(std::forward<Ts>(ts)...);
            }

            T* get_storage()noexcept{
                return reinterpret_cast<T*>(_m_storage);
            }

            const T* get_storage()const noexcept{
                return reinterpret_cast<const T*>(_m_storage);
            }
            // SAFETY: this->_m_storage shall provide storage for an object of type T for which the lifetime has begun, and not ended.
            void set() noexcept{
                
                this->_engaged = true;
            }

            bool engaged() const noexcept{
                return _engaged;
            }

            T& get_unchecked() noexcept{
                return *get_storage();
            }

            const T& get_unchecked() const noexcept{
                return *get_storage();
            }
            void clear(){
                if(std::exchange(_engaged,false))
                    reinterpret_cast<T*>(_m_storage)->~T();
            }
        };

        template<typename T,bool=std::is_trivially_destructible_v<T>> struct _option_destructor : _option_storage<T>{
            _option_destructor()noexcept=default;
            ~_option_destructor() noexcept(std::is_nothrow_destructible_v<T>){
                this->clear();
            }
        };

        template<typename T> struct _option_destructor<T,true> : _option_storage<T>{};


        template<typename T,bool=std::is_trivially_move_constructible_v<T>,bool=std::is_trivially_move_assignable_v<T>>
            struct _option_move : _option_destructor<T>{
                _option_move()noexcept=default;
                _option_move(_option_move&& m) {
                    if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }
                }

                _option_move& operator=(_option_move&& m) noexcept{
                    if(this->engaged()&&m.engaged())
                        this->get_unchecked() = std::move(m.get_unchecked());
                    else if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }else{
                        this->clear();
                    }
                    return *this;

                }
            };

        template<typename T> struct _option_move<T,true,false> : _option_destructor<T>{
            _option_move()noexcept=default;
            _option_move(_option_move&&) noexcept=default;

            _option_move& operator=(_option_move&& m) noexcept{
                    if(this->engaged()&&m.engaged())
                        this->get_unchecked() = std::move(m.get_unchecked());
                    else if(m.engaged()){
                        ::new(this->get_storage()) T{std::move(m.get_unchecked())};
                        this->set();
                    }else{
                        this->clear();
                    }
                    return *this;

                }
        };

        template<typename T> struct _option_move<T,true,true>: _option_destructor<T>{};

        template<typename T,bool has_copy_ctor=std::is_copy_constructible_v<T>,bool trivial_copy_ctor=std::is_trivially_copy_constructible_v<T>,bool is_copy_assignable=std::is_copy_assignable_v<T>,bool is_trivial_copy_assignment=std::is_trivially_copy_assignable_v<T>>
        struct _option_copy : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy&)=delete;
            _option_copy& operator=(const _option_copy&)=delete;
        };


        template<typename T> struct _option_copy<T,true,false,false,false> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy& o){
                if(o.engaged()){
                    ::new(this->get_storage()) T{o.get_unchecked()};
                    this->set();
                }
            }
            _option_copy& operator=(const _option_copy&)=delete;
        };

        template<typename T> struct _option_copy<T,true,true,false,false> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy&)=default;
            _option_copy& operator=(const _option_copy&)=delete;
        };

        template<typename T,bool trivially_copy_assignable> struct _option_copy<T,true,false,true,trivially_copy_assignable> : _option_move<T>{
            _option_copy()=default;
            _option_copy(const _option_copy& o){
                if(o.engaged()){
                    ::new(this->get_storage()) T{o.get_unchecked()};
                    this->set();
                }
            }

            _option_copy& operator=(_option_copy&& m) noexcept{
                if(this->engaged()&&m.engaged())
                    this->get_unchecked() = m.get_unchecked();
                else if(m.engaged()){
                    ::new(this->get_storage()) T{m.get_unchecked()};
                    this->set();
                    
                }else{
                    this->clear();
                }
                return *this;

            }
        };

        template<typename T> struct _option_copy<T,true,true,true,true> : _option_move<T>{};
    }

    struct nullopt_t{
        constexpr explicit nullopt_t()noexcept=default;
    };

    constexpr inline nullopt_t nullopt{};

    template<typename T> struct optional;

    struct _nonesuch{
        _nonesuch(const _nonesuch&)=delete;
        _nonesuch(_nonesuch&&)=delete;
        _nonesuch()=delete;
        ~_nonesuch()=delete;
    };

    template<typename T> struct _flatten_option {
        using type = _nonesuch;
    };
    template<typename T> struct _flatten_option<lccc::optional<T>> {
        using type = T;
    };

    template<typename T> struct _flatten_option<std::optional<T>> {
        using type = T;
    };

    template<typename T> struct _flatten_option<const T>{
        using type = std::add_const_t<typename _flatten_option<T>::type>;
    };

    template<typename T> struct _flatten_option<T&>{
        using type = std::add_const_t<typename _flatten_option<T>::type>;
    };


    
    template<typename T> struct optional: private _detail::_option_copy<T>{
    public:
        static_assert(!std::is_same_v<lccc::remove_cvref_t<T>,nullopt_t>);
        optional()noexcept=default;
        optional(const optional&)=default;
        optional(optional&&)noexcept=default;
        optional& operator=(const optional&)=default;
        optional& operator=(optional&&)noexcept=default;
        ~optional()noexcept=default;

        optional(const nullopt_t&) noexcept : optional{}{} 
        
        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
            optional(const T& t) {
                ::new(this->get_storage()) T{t};
                this->set();
            }
        
        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
            optional(T&& t) {
                ::new(this->get_storage()) T{std::move(t)};
                this->set();
            }

        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
            optional(const std::optional<T>& t) {
                if(t){
                    ::new(this->get_storage()) T{*t};
                    this->set();
                }
            }

        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
            optional(std::optional<T>&& t) {
                if(t){
                    ::new(this->get_storage()) T{*t};
                    this->set();
                }
            }



        template<typename F,std::enable_if_t<std::is_invocable_v<F&&,const T&>>* =nullptr>
            void if_present(F&& f){
                if(this->engaged())
                    std::invoke(std::forward<F>(f),this->get_unchecked());
            }

        using _detail::_option_storage<T>::get_unchecked;



        explicit operator bool()const{
            return this->engaged();
        }

        T& operator*(){
            return this->get_unchecked();
        }

        const T& operator*()const{
            return this->get_unchecked();
        }

        T* operator->()noexcept{
            return &this->get_unchecked();
        }

        const T* operator->() const noexcept{
            return &this->get_unchecked();
        }

        template<typename F> lccc::optional<std::invoke_result_t<F&&,const T&>>
            map(F&& f)  const{
                if(*this)
                    return {std::invoke(f,**this)};
                else
                    return nullopt;
            }

        template<typename F> lccc::optional<std::invoke_result_t<F&&,T&&>>
            map(F&& f) &&{
                if(*this)
                    return {std::invoke(f,std::move(**this))};
                else
                    return nullopt;
            }

        template<typename _T = typename _flatten_option<T>::type,std::enable_if_t<std::is_copy_constructible_v<_T>>* =nullptr>
            lccc::optional<_T> flatten() const{
                if(*this)
                    return **this;
                else
                    return nullopt;
            }

        template<typename _T = typename _flatten_option<T>::type,std::enable_if_t<std::is_move_constructible_v<_T>>* =nullptr>
            lccc::optional<_T> flatten() &&{
                if(*this)
                    return std::move(**this);
                else
                    return nullopt;
            }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&,const T&>>* =nullptr>
            lccc::optional<T> and_then(F&& f) const{
                if(*this)
                    return std::invoke(f,this->get_unchecked());
                else
                    return nullopt;
            }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&,T&&>>* =nullptr>
            lccc::optional<T> and_then(F&& f) &&{
                if(*this)
                    return std::invoke(f,std::move(this->get_unchecked()));
                else
                    return nullopt;
            }

       template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&>>* =nullptr,std::enable_if_t<std::is_copy_constructible_v<T>>* =nullptr>
             lccc::optional<T> or_else(F&& f) const&{
                 if(*this)
                    return *this;
                else
                    return {std::invoke(f)};
             }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T>,F&&>>* =nullptr,std::enable_if_t<std::is_move_constructible_v<T>>* =nullptr>
             lccc::optional<T> or_else(F&& f) &&{
                 if(*this)
                    return std::move(**this);
                else
                    return {std::invoke(f)};
             }


        template<std::enable_if_t<std::is_move_constructible_v<T>>* =nullptr>
            lccc::optional<T> take(){
                if(*this){
                    lccc::optional<T> opt{**this};
                    this->clear();
                    return opt;
                }else{
                    return nullopt;
                }
            }

        lccc::optional<T&> as_ref() noexcept{
            if(*this)
                return {**this};
            else
                return nullopt;
        }

        lccc::optional<const T&> as_ref() const noexcept {
            if(*this)
                return {**this};
            else
                return nullopt;
        }

    };

    struct _private_token{
        explicit constexpr _private_token()=default;
    };

    ///
    /// Specialization of optional on reference types
    /// optional<T&> is trivially-copyable and a standard layout type
    /// 
    template<typename T> struct optional<T&>{
    private:
        static_assert(!std::is_same_v<lccc::remove_cvref_t<T>,nullopt_t>);
        T* _m_inner;

        

    public:
        constexpr optional(T* _ptr, _private_token) noexcept : _m_inner{_ptr}{}
        constexpr optional(const lccc::nullopt_t& =lccc::nullopt) noexcept : _m_inner{}{}


        constexpr optional(const optional&)=default;
        constexpr optional(optional&&)noexcept=default;
        constexpr optional& operator=(const optional&)=default;
        constexpr optional& operator=(optional&&)noexcept=default;
        ~optional()noexcept=default;

        // rvalue overload is deleted
        constexpr optional(const T&&)=delete; 
        constexpr optional(T& _t) : _m_inner{&_t}{}

        template<typename U,typename=std::enable_if_t<std::is_convertible_v<U*,T*>>>
            constexpr optional(lccc::optional<U&> opt) : _m_inner(opt.as_ptr()){}

        template<typename U,typename=std::enable_if_t<std::is_convertible_v<U*,T*>>>
            constexpr optional(U& val) : _m_inner(&val){}
        
        // Copy/Move Constructor/Assignment are trivial

        T& operator*(){
            return *_m_inner;
        }
        const T& operator*() const{
            return *_m_inner;
        }

        T* operator->() noexcept{
            return _m_inner;
        }

        const T* operator->()const noexcept{
            return _m_inner;
        }

        T* as_ptr() noexcept{
            return _m_inner;
        }

        const T* as_ptr() const noexcept {
            return _m_inner;
        }

        explicit operator bool()const noexcept{
            return _m_inner;
        }

        lccc::optional<const T&> as_const() const{
            if(*this)
                return {**this};
            else
                return nullopt;
        }

        template<typename F, std::invoke_result_t<F&&,T&>* =nullptr>
            void if_present(F&& f){
                if(*this)
                    std::invoke(std::forward<F>(f),**this);
            }

        template<typename F, std::invoke_result_t<F&&,const T&>* =nullptr>
            void if_present(F&& f) const{
                if(*this)
                    std::invoke(std::forward<F>(f),const_cast<const T&>(**this));
            }

        template<typename F> lccc::optional<std::invoke_result_t<F&&,T&>> map(F&& f){
            if(*this)
                return {std::invoke(std::forward<F>(f),**this)};
            else
                return nullopt;
        }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T&>,F&&>>* =nullptr>
            lccc::optional<T&> or_else(F&& f){
                if(*this)
                    return *this;
                else
                    return std::invoke(std::forward<F>(f));
            }

        template<typename F,std::enable_if_t<std::is_invocable_r_v<lccc::optional<T&>,F&&,T&>>* =nullptr>
            lccc::optional<T&> and_then(F&& f){
                if(*this)
                    return std::invoke(std::forward<F>(**this));
                else
                    return nullopt;
            }

        lccc::optional<T&> take(){
            if(*this){
                auto ptr = std::exchange(_m_inner,nullptr);
                return {*ptr};
            }else{
                return nullopt;
            }
        }
        template<typename U,std::enable_if_t<std::is_base_of_v<T,U>&&std::is_convertible_v<U*,lccc::copy_cv_t<U,T>*>>* = nullptr>
            lccc::optional<U&> downcast() const noexcept{
            return {dynamic_cast<U*>(_m_inner),_private_token{}};
        }
    };

    

    template<typename T,typename U> auto operator==(const lccc::optional<T>& a,const lccc::optional<U>& b) noexcept(noexcept(*a==*b)) -> std::common_type_t<bool,decltype(*a==*b)>{
        if(a&&b)
            return *a==*b;
        else 
            return !a&&!b;
    }

    template<typename T,typename U> auto operator==(const std::optional<T>& a,const lccc::optional<U>& b) noexcept(noexcept(*a==*b)) -> std::common_type_t<bool,decltype(*a==*b)>{
        if(a&&b)
            return *a==*b;
        else 
            return !a&&!b;
    }

    template<typename T,typename U> auto operator==(const lccc::optional<T>& a,const std::optional<U>& b) noexcept(noexcept(*a==*b)) -> std::common_type_t<bool,decltype(*a==*b)>{
        if(a&&b)
            return *a==*b;
        else 
            return !a&&!b;
    }
}

#endif 