/**
 * xlang++/layout/UnorderedMap.hpp
 * This file is part of libxlang, a part of the lccc project
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * Like all libraries as part of the lccc project,
 *  libxlang is additionally dual licensed under the terms of the MIT and Apache v2 license. 
 * When dealing in this software, you may, at your option, do so under only those terms,
 *  or only under the terms of the GNU Lesser General Public License, or under both sets of terms. 
 */

#ifndef LCCC_LAYOUT_UNORDERED_MAP_HPP_2021_08_28_20_53_29
#define LCCC_LAYOUT_UNORDERED_MAP_HPP_2021_08_28_20_53_29

#include <xlang++/layout/Hash.hpp>
#include <xlang++/layout/Primitives.hpp>
#include <xlang++/layout/Allocator.hpp>

namespace lccc
{

    std::size_t _next_hashtable_capacity(std::size_t s);

    template<typename K,typename V,typename Allocator=lccc::allocator<lccc::pair<const K,V>>, typename Hash=lccc::hash<K>,typename Pred=lccc::equal_to<K>> struct unordered_map{
    public:
        using key_type = K;
        using value_type = V;
        using element_type = lccc::pair<const K,V>;
        using allocator_type = Allocator;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = typename std::allocator_traits<Allocator>::pointer;
        using const_pointer = typename std::allocator_traits<Allocator>::const_pointer;
        using size_type = typename std::allocator_traits<Allocator>::size_type;
        using difference_type = typename std::make_signed_t<size_type>;
    private:
        
        struct _hash_bucket{
            size_type _m_count;
            std::aligned_union_t<1,lccc::pair<const K,V>> _m_entries[16];
        };
        using _bucket_allocator = typename std::allocator_traits<Allocator>::template rebind_alloc<_hash_bucket>;
        using _bucket_pointer = typename std::allocator_traits<_bucket_allocator>::pointer;


        _bucket_pointer _m_array;
        size_type _m_buckets;
        _bucket_allocator _m_alloc;
        Hash _m_hash;
        Pred _m_pred;

        

        template<typename ElementT_> struct _iterator{
        private:
            _bucket_pointer _m_bucket;
            size_type _m_idx;

            _iterator(_bucket_pointer _ptr,size_type _idx=0) : _m_bucket{_ptr}, _m_idx{_idx}{}
        public:
            using value_type = ElementT_;
            using reference = value_type&;
            using const_reference = const value_type&;
            using pointer = ElementT_*;
            using const_pointer =  const ElementT_*;
            using iterator_category = std::bidirectional_iterator_tag;

            _iterator()noexcept=default;

            _iterator& operator++()noexcept{
                _m_idx++;
                while(_m_bucket->_m_count==_m_idx){
                    _m_bucket++;
                    _m_idx=0;
                }
                return *this;
            }

            _iterator operator++(int) noexcept{
                auto ret{*this};
                (++*this);
                return ret;
            }

            _iterator& operator--() noexcept{
                while(_m_idx==0){
                    _m_bucket--;
                    _m_idx = _m_bucket->_m_count;
                }
                _m_idx--;
                return *this;
            }

            _iterator operator--(int)noexcept{
                auto ret{*this};
                (--*this);
                return ret;
            }
            pointer operator->()const noexcept{
                return reinterpret_cast<pointer>(_m_bucket->_m_entries+_m_idx);
            }
            reference operator*()const noexcept{
                return *this->operator->();
            }
        };

        void _rehash(){
            size_type _cap = _next_hashtable_capacity(_m_buckets);
            
        }
    public:
        using iterator = _iterator<element_type>;
        using const_iterator = _iterator<const element_type>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;


        unordered_map(const Allocator& _alloc=Allocator(),const Hash& _hash=Hash(),const Pred& _pred=Pred()) : _m_array{}, _m_buckets{17}, _m_alloc{_alloc}, _m_hash{_hash}, _m_pred{_pred}{
            _m_array = std::allocator_traits<_bucket_allocator>::allocate(_m_alloc,_m_buckets);
            for(size_type n = 0;n<_m_buckets;n++)
            {
                std::allocator_traits<_bucket_allocator>::construct(_m_alloc,_m_buckets+n);
                _m_array[n]._m_count = 0;
            }
        }

        template<typename U,std::invoke_result_t<const Hash&,const U&>* =nullptr, std::invoke_result_t<const Pred&,const K&,const U&>* =nullptr>
            lccc::optional<reference> get(const U& key){
                auto hash = const_cast<const Hash&>(_m_hash)(key)%_m_buckets; // non-const operator() may be different or deleted, force use of const overload
                for(size_type n =0;n<_m_array[hash]._m_count;n++)
                    if(const_cast<const Pred&>(_m_pred)(*reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n]),key))
                        return *reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n]);
                
                return nullopt;
            }

        template<typename U,std::invoke_result_t<const Hash&,const U&>* =nullptr, std::invoke_result_t<const Pred&,const K&,const U&>* =nullptr>
            lccc::optional<const_reference> get(const U& key) const {
                auto hash = const_cast<const Hash&>(_m_hash)(key)%_m_buckets; // non-const operator() may be different or deleted, force use of const overload
                for(size_type n =0;n<_m_array[hash]._m_count;n++)
                    if(const_cast<const Pred&>(_m_pred)(reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n])->first,key))
                        return *reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n]);
                
                return nullopt;
            }


        const allocator_type& get_allocator()const{
            return this->_m_alloc;
        }

        lccc::pair<lccc::optional<value_type>,iterator> insert(element_type e){
            for(;;){
                auto hash = const_cast<const Hash&>(_m_hash)(e.first)%_m_buckets;
                size_type n = 0;
                for(;n<_m_array[hash]._m_count;n++){
                    if(const_cast<const Pred&>(_m_pred)(reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n])->first,e.first))
                        return {lccc::optional{std::exchange(reinterpret_cast<element_type*>(&_m_array[hash]._m_entries[n])->first,e.second)},iterator{&_m_array[hash],n}};
                }
                if(n!=16){
                    ::new(&_m_array[hash]._m_entries[n]) element_type{std::move(e)};
                    _m_array[hash]->_m_count++;
                    return {lccc::optional<value_type>{},iterator{&_m_array[hash],n}};
                }
                this->rehash();
            }
        }
    };
    
}

#endif //LCCC_LAYOUT_H
