#ifndef XLANG_VARIANT_HPP_2021_08_22_19_47_36
#define XLANG_VARIANT_HPP_2021_08_22_19_47_36

#include <xlang++/layout/Primitives.hpp>
#include <xlang++/layout/TypeTraits.hpp>
#include <type_traits>
#include <functional>
#include <variant>

namespace lccc{

    namespace _detail{

        template<typename KeyType,typename... Ts> struct variant_storage{
            std::aligned_union_t<1,lccc::pair<KeyType,Ts>...> _m_storage;

            KeyType discriminant() const noexcept{
                KeyType val = *std::launder(reinterpret_cast<const KeyType*>(&this->_m_storage));
                xlang_assert(static_cast<std::size_t>(val)<sizeof...(Ts),"The discriminant of a variant must be in range for the type list");
                return val;
            }

            template<typename T> T& get_element_unchecked() noexcept{
                return std::launder(reinterpret_cast<lccc::pair<KeyType,T>*>(&this->_m_storage))->second;
            }

            template<typename T> const T& get_element_unchecked() const noexcept{
                return std::launder(reinterpret_cast<const lccc::pair<KeyType,T>*>(&this->_m_storage))->second;
            }
        };
        template<typename KeyType,typename T> void do_in_place_copy(void* to_storage,const void* from_storage){
            ::new(to_storage) lccc::pair<KeyType,T>(*std::launder(static_cast<const lccc::pair<KeyType,T>*>(from_storage)));
        }



        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is> void in_place_copy_sel(KeyType key,void* to_storage,const void* from_storage,std::index_sequence<I1,Is...> seq){
            if(static_cast<std::size_t>(key)==I1)
                do_in_place_copy<KeyType,T1>(to_storage,from_storage);
            else{
                if constexpr(sizeof...(Is)==0)
                    xlang_assert(false,"Reached end of type list for copy construction");
                else
                    in_place_copy_sel<KeyType,Ts...>(key,to_storage,from_storage,std::index_sequence<Is...>{});
            }    
        } 
        
        template<typename KeyType,typename... Ts> void in_place_copy(KeyType key,variant_storage<KeyType,Ts...>* to_storage,const variant_storage<KeyType,Ts...>* from_storage){
            in_place_copy_sel(key,&to_storage->_m_storage,from_storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,typename T> void do_in_place_move(void* to_storage,void* from_storage){
            ::new(to_storage) lccc::pair<KeyType,T>(std::move(*std::launder(static_cast<lccc::pair<KeyType,T>*>(from_storage))));
        }



        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is> void in_place_move_sel(KeyType key,void* to_storage,const void* from_storage,std::index_sequence<I1,Is...> seq){
            if(static_cast<std::size_t>(key)==I1)
                do_in_place_move<KeyType,T1>(to_storage,from_storage);
            else{
                if constexpr(sizeof...(Is)==0)
                    std::terminate();
                else
                    in_place_move_sel<KeyType,Ts...>(key,to_storage,from_storage,std::index_sequence<Is...>{});
            }    
        } 
        
        template<typename KeyType,typename... Ts> void in_place_move(KeyType key,variant_storage<KeyType,Ts...>* to_storage,const variant_storage<KeyType,Ts...>* from_storage){
            in_place_move_sel(key,&to_storage->_m_storage,from_storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,typename T> void do_in_place_destroy(void* storage){
            std::launder(static_cast<lccc::pair<KeyType,T>*>(storage))->~pair<KeyType,T>();
        }

        template<typename KeyType,typename T1,typename... Ts,std::size_t I1,std::size_t... Is>
            void in_place_destroy_sel(KeyType key,void* storage,std::index_sequence<I1,Is...>){
                if(static_cast<std::size_t>(key)==I1)
                    do_in_place_destroy<KeyType,T1>(storage);
                else{
                    if constexpr(sizeof...(Is)==0)
                        std::terminate();
                    else
                        in_place_destroy_sel<KeyType,Ts...>(key,storage,std::index_sequence<Is...>{});
                }
            }

        template<typename KeyType,typename... Ts> void in_place_destroy(KeyType key,variant_storage<KeyType,Ts...>* storage){
            in_place_destroy_sel(key,storage,std::index_sequence_for<Ts...>{});
        }

        template<typename KeyType,bool trivial_destructor,typename... Ts> struct variant_destructor : variant_storage<KeyType,Ts...>{
            ~variant_destructor()=default;
        };

        template<typename KeyType,typename... Ts> struct variant_destructor<KeyType,false,Ts...> : variant_storage<KeyType,Ts...>{
            ~variant_destructor() noexcept((std::is_nothrow_destructible_v<Ts> && ...)){
                KeyType k = *std::launder(static_cast<KeyType*>(this));
                in_place_destroy(k,this);
            }
        };
        

        template<typename KeyType,bool has_default_ctor,typename... Ts>
            struct variant_default_ctor : variant_destructor<KeyType,std::conjunction_v<std::is_trivially_destructible<Ts>...>,Ts...>{
                variant_default_ctor() =delete;
            };
        template<typename KeyType,typename T1,typename... Ts> struct variant_default_ctor<KeyType,true,T1,Ts...> : variant_destructor<KeyType,std::conjunction_v<std::is_trivially_destructible<T1>,std::is_trivially_destructible<Ts>...>,T1,Ts...>{
            variant_default_ctor() noexcept{
                ::new(&this->_m_storage) lccc::pair<KeyType,T1>{}; // get zero-initialization
            }
        };

        template<typename KeyType,bool has_copy_ctor,bool trivial_copy_ctor,typename T1,typename... Ts>
            struct variant_copy_ctor : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                variant_copy_ctor(const variant_copy_ctor&)=delete;
                variant_copy_ctor& operator=(const variant_copy_ctor&)=delete;
            };
        template<typename KeyType,typename T1,typename... Ts>
            struct variant_copy_ctor<KeyType,true,true,T1,Ts...> : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                constexpr variant_copy_ctor(const variant_copy_ctor&)=default;
                constexpr variant_copy_ctor& operator=(const variant_copy_ctor&)=default;
            };
        
        template<typename KeyType,typename T1,typename... Ts>
            struct variant_copy_ctor<KeyType,true,false,T1,Ts...> : variant_default_ctor<KeyType,std::is_default_constructible_v<T1>,T1,Ts...>{
                variant_copy_ctor(const variant_copy_ctor& c1) noexcept {
                    KeyType k = *static_cast<const KeyType*>(static_cast<const void*>(&c1));
                    in_place_copy(this,&c1);
                }
                variant_copy_ctor& operator=(const variant_copy_ctor& c1) noexcept{
                    if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                        in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                    in_place_copy(this->discriminant(),this,&c1);
                    return *this;
                }
            };

        template<typename KeyType,bool has_move_ctor,bool trivial_move_ctor,typename... Ts>
            struct variant_move_ctor : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...> {
                variant_move_ctor(variant_move_ctor&&)=delete;
                variant_move_ctor& operator=(variant_move_ctor&&)=delete;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,true,Ts...> : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                constexpr variant_move_ctor(variant_move_ctor&&)=default;
                constexpr variant_move_ctor& operator=(variant_move_ctor&&)=default;
            };
        template<typename KeyType,typename... Ts>
            struct variant_move_ctor<KeyType,true,false,Ts...> : variant_copy_ctor<KeyType,(std::is_copy_constructible_v<Ts> && ...),(std::is_trivially_copy_constructible_v<Ts> && ...),Ts...>  {
                variant_move_ctor(variant_move_ctor&& m1) noexcept{
                    KeyType k = m1.discriminant();
                    in_place_move(k,this,&m1);
                }
                variant_move_ctor& operator=(variant_move_ctor&& m1) noexcept{
                    if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                        in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                    in_place_move(this->discriminant(),this,&m1);
                    return *this;
                }
            };
        
        template<typename KeyType,typename... Ts>
            struct variant_impl : variant_move_ctor<KeyType,(std::is_move_constructible_v<Ts> && ...),(std::is_trivially_move_constructible_v<Ts> && ...),Ts...>{};
    }

    template<typename KeyType,typename... Ts> struct variant;
}

namespace std{
    template<std::size_t I,typename KeyType,typename T1,typename... Ts> struct variant_alternative<I,lccc::variant<KeyType,T1,Ts...>> : variant_alternative<(I-1),lccc::variant<KeyType,Ts...>>{};
    template<typename KeyType,typename T1,typename... Ts> struct variant_alternative<0,lccc::variant<KeyType,T1,Ts...>>{
        using type = T1;
    };

    template<typename KeyType,typename... Ts> struct variant_size<lccc::variant<KeyType,Ts...>> : std::integral_constant<std::size_t,sizeof...(Ts)>{};
}

namespace lccc{


    struct monostate{};

    template<typename KeyType,KeyType Key> struct in_place_key_t{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::in_place_key_t must be an integral or enumeration type");
    };

    template<auto Key> constexpr inline in_place_key_t<decltype(Key),Key> in_place_key{};

    /// A partial implementation of std::variant, with custom key types and a guaranteed layout
    /// 
    /// The layout of each variant member V_i, where 0<=i<sizeof...(Ts) is the struct{KeyType discrim;T_i}, 
    ///  where i is the ith corresponding type in Ts...
    /// The layout of `variant<KeyType,Ts...>` is the layout of the union{Vs... vs;} where Vs is the list of types described above.
    /// If KeyType is a fixed width integer type `std::uintN_t` or `std::intN_t`, or an enum type with an underlying type which is such a type,
    ///  this layout is equivalent to the rust declaration `#[repr(uN)] enum Variant{ Cs(Ts)...}`, where `Cs` is an pack of anonymous constructors for each value of KeyType
    ///  present for Ts... 
    /// If `sizeof...(Ts)-1` does not fit in KeyType, or it's underlying type if it is an enum type, the behaviour is undefined.
    /// The behaviour is undefined if a variant used with a value of KeyType that does not correspond with an element of Ts...
    template<typename KeyType,typename... Ts> struct variant : private _detail::variant_impl<KeyType,Ts...>{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::variant must be an integral or enumeration type");
        static_assert((std::is_object_v<Ts>&&...),"Each type in Ts... shall be a complete object type");

        using _detail::variant_impl<KeyType,Ts...>::variant_impl;

        template<KeyType Key,typename... Us,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>,
            typename = std::enable_if_t<std::is_constructible_v<Ty,Us&&...>>>
            explicit variant(in_place_key_t<KeyType,Key>,Us&&... args) noexcept {
                ::new(&this->_m_storage) lccc::pair<KeyType,Ty>(Key,Ty(std::forward<Us>(args)...));
            }

        template<KeyType Key,typename... Us,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>,
            typename = std::enable_if_t<std::is_constructible_v<Ty,Us&&...>>>
            void emplace(Us&&... args) noexcept {
                if constexpr(!(std::is_trivially_destructible_v<Ts> && ...))
                    _detail::in_place_destroy<KeyType,Ts...>(this->discriminant(),this);
                ::new(this->_m_storage) lccc::pair<KeyType,Ty>(Key,Ty(std::forward<Us>(args)...));
            }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> Ty& get_unchecked()noexcept{
            xlang_assert(Key==this->discriminant(),"Failed get_unchecked, key!=discriminant");
            return this->template get_element_unchecked<Ty>();
        }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> const Ty& get_unchecked()const noexcept{
            xlang_assert(Key==this->discriminant(),"Failed get_unchecked, key!=discriminant");
            return this->template get_element_unchecked<Ty>();
        }
        
        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> Ty* get_pointer()noexcept{
            if(Key!=this->discriminant())
                return nullptr;
            else
                return &this->template get_element_unchecked<Ty>();
        }

        template<KeyType Key,typename Ty = std::variant_alternative_t<static_cast<std::size_t>(Key),variant>> const Ty* get_pointer()const noexcept{
            if(Key!=this->discriminant())
                return nullptr;
            else
                return &this->template get_element_unchecked<Ty>();
        }

        ///
        /// Returns the discriminant field of the type
        using _detail::variant_storage<KeyType,Ts...>::discriminant;

    };
    /// Specialization of lccc::variant for no variants
    /// An empty lccc::variant acts like the rust `!` type - it cannot be created, and can be implicitly-converted to any type
    /// Having a value of this type is undefined behaviour. 
    /// It is trivially copyable and 
    template<typename KeyType> struct variant<KeyType>{
        static_assert(std::is_enum_v<KeyType>||std::is_integral_v<KeyType>,"KeyType for lccc::variant must be an integral or enumeration type");
        variant()=delete;
        variant(const variant& v)=default;

        variant(variant&& v)=default;

        variant& operator=(const variant& v)=default;

        variant& operator=(variant&& v) = default;

        template<typename T> operator T()const noexcept{
            unreachable_unchecked();
            return *reinterpret_cast<T*>(this);
        }

        KeyType discriminant() const noexcept{
            unreachable_unchecked();
            return *reinterpret_cast<KeyType*>(this);
        }
    };

    using empty = variant<std::size_t>;

    namespace _detail{
        template<typename F,typename KeyType,typename R,typename T0,typename... Ts, std::size_t I0,std::size_t... Is> 
            R do_visit_sel(KeyType key, F&& f, void* storage, std::index_sequence<I0,Is...>) noexcept((std::is_nothrow_invocable_v<F&&,const T0&> && ... && std::is_nothrow_invocable_v<F&&,const Ts&>)){
                if(static_cast<KeyType>(I0)==key)
                    return std::invoke(std::forward<F>(),static_cast<T0&&>(*static_cast<lccc::pair<KeyType,std::remove_cv_t<std::remove_reference_t<T0>>>*>(storage)));
                else{
                    if constexpr(sizeof...(Is)==0)
                        xlang_assert(false,"Invalid key present in Variant");
                    else
                        return do_visit_sel<F,KeyType,R,Ts...>(key,std::forward<F>(f),storage,std::index_sequence<Is...>{});
                }
            }
    }

    template<typename F,typename KeyType,typename... Ts,std::enable_if_t<std::conjunction_v<std::is_invocable<F,const Ts&>...>>* =nullptr>
        decltype(auto) visit(const lccc::variant<KeyType,Ts...>& var,F&& f) noexcept(std::conjunction_v<std::is_nothrow_invocable<F,const Ts&>...>){
            if constexpr(sizeof...(Ts)==0)
                unreachable_unchecked();
            else
                return _detail::do_visit_sel<F,KeyType,std::common_type_t<std::invoke_result_t<F&&,const Ts&>...>,const Ts&...>(var->discriminant(),const_cast<void*>(static_cast<const void*>(&var)),std::index_sequence_for<Ts...>{});
        }

} // namespace lccc


#endif /* XLANG_VARIANT_HPP_2021_08_22_19_47_36 */