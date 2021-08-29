#ifndef XLANG_LAYOUT_HASH_HPP_2021_08_28_20_49_25
#define XLANG_LAYOUT_HASH_HPP_2021_08_28_20_49_25
#include <xlang++/layout/Primitives.hpp>
#include <xlang++/layout/Optional.hpp>
#include <xlang++/layout/Pair.hpp>
#include <xlang++/layout/StringView.hpp>
#include <xlang++/layout/String.hpp>
#include <xlang++/layout/Variant.hpp>
#include <xlang++/layout/TypeTraits.hpp>
#include <functional>

namespace lccc {

    template<typename T,typename=void>
         struct hash{
    public:
        hash()=delete;
        hash(const hash&)=delete;
        hash(hash&&)=delete;
        hash& operator=(const hash&)=delete;
        hash& operator=(hash&&)=delete;
    };

    template<typename T> struct hash<T,std::enable_if_t<std::is_integral_v<T>||std::is_floating_point_v<T>||std::is_member_pointer_v<T>||std::is_pointer_v<T>||std::is_enum_v<T>>>{
        std::size_t operator()(const T& t) const{
            return xlang_hash_scalar(&t,sizeof(T));
        }
    };

    template<typename T> struct hash<T&> : public lccc::hash<std::remove_const_t<T>>{}; // allows hashing optional<T&>

    template<typename T,std::size_t N> struct hash<T[N]>{
    private:
        hash<T> _inner;
    public:
        template<std::invoke_result_t<const hash<T>&,const T&>* =nullptr> std::size_t operator()(const T (&arr)[N]) const{
            std::size_t hash = xlang_hash_scalar(&xlang_hash_seed,1);
            for(const T& t : arr){
                hash^=std::invoke(_inner,t);
                hash*=xlang_hash_prime;
            }
            return hash;
        }
    };

    template<typename T> struct hash<lccc::optional<T>>{
    private:
        hash<T> _inner;
    public:
        template<std::invoke_result_t<hash<const T>&,const T&>* =nullptr> std::size_t operator()(const lccc::optional<T>& opt) const{
            if(opt)
                return std::invoke(_inner,opt);
            else
                return xlang_hash_scalar(&xlang_hash_seed,1);
        }
    };

    template<> struct hash<lccc::nullopt_t>{
        std::size_t operator()(const lccc::nullopt_t&) const{
            return xlang_hash_scalar(&xlang_hash_seed,1);
        }
    };


    template<> struct hash<lccc::monostate>{
        std::size_t operator()(const lccc::monostate&) const{
            return xlang_hash_scalar(&xlang_hash_seed,1);
        }
    };

    template<typename KeyType,typename... Ts> struct hash<lccc::variant<KeyType,Ts...>>{
    private:
        lccc::hash<KeyType> _khash;
    public:

        template<std::void_t<decltype(std::declval<const lccc::hash<KeyType>&>()()),decltype(std::declval<const lccc::hash<Ts>&>()(std::declval<const Ts>()))...>* =nullptr>
            std::size_t operator()(const lccc::variant<KeyType,Ts...>& v) const{
                size_t hash = xlang_hash_scalar(&xlang_hash_seed,1);
                hash *= std::invoke(_khash,v.discriminant());
                hash ^= xlang_hash_prime;
                hash *= lccc::visit(v,[](const auto& a) -> std::size_t{
                    return lccc::hash<lccc::remove_cvref_t<decltype(a)>>{}(a);
                });
            }
    };

    
    template<typename T> struct equal_to{
    public:
        constexpr auto operator()(const T& a,const T& b) -> decltype(a==b){
            return a==b;
        }
    };
}

#endif