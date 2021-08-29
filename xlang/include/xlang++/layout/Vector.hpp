#ifndef XLANG_LAYOUT_VECTOR_HPP_2021_08_28_20_44_15
#define XLANG_LAYOUT_VECTOR_HPP_2021_08_28_20_44_15

#include <exception>
#include <xlang++/layout/Allocator.hpp>
#include <memory>

namespace lccc{


    namespace _detail{
        struct terminate_on_unwind{
            int excepts;
            terminate_on_unwind() : excepts{std::uncaught_exceptions()}{}
            ~terminate_on_unwind(){
                if(std::uncaught_exceptions()!=excepts)
                    std::terminate();
            }
        };
    }

    template<typename T,typename Alloc=lccc::allocator<T>> struct vector{
    public:
        static_assert(std::is_same_v<T,typename Alloc::value_type>);
        using element_type = T;
        using value_type = T;
        using pointer = typename std::allocator_traits<Alloc>::pointer;
        using const_pointer = typename std::allocator_traits<Alloc>::const_pointer;
        using reference = T&;
        using const_reference = const T&;
        using size_type = typename std::allocator_traits<Alloc>::size_type;
        using difference_type = std::make_signed_t<size_type>;
        using iterator = pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    private:
        pointer _m_begin;
        size_type _m_len;
        size_type _m_capacity;
        Alloc _m_alloc;

        void reallocate(size_type ncap){
            pointer nbegin = std::allocator_traits<Alloc>::allocate(_m_alloc,ncap);
            for(size_t n = 0;n<_m_len;n++){
                try{
                    
                    std::allocator_traits<Alloc>::construct(_m_alloc,nbegin+n,std::move_if_noexcept(_m_begin[n]));
                }catch(...){
                    {
                        _detail::terminate_on_unwind _guard{};
                        for(;n>0;n--)
                            std::allocator_traits<Alloc>::destroy(_m_alloc,nbegin+n-1);
                    }
                    throw;
                }
            }
            _m_capacity = ncap;
            std::swap(nbegin,_m_begin);
            for(size_t n = _m_len;n>0;n--)
                std::allocator_traits<Alloc>::destroy(_m_alloc,nbegin[n-1]);
            std::allocator_traits<Alloc>::deallocate(_m_alloc,nbegin);
        }

    public:
        vector(const Alloc& alloc=Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc{alloc}{
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,16);
        }

        ~vector(){
            if(_m_begin){
                for(;_m_len>0;_m_len--)
                    std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len-1);
                std::allocator_traits<Alloc>::deallocate(_m_alloc,_m_begin);
            }
        }

        vector(const vector& v) : _m_alloc{std::allocator_traits<Alloc>::select_on_container_copy_construction(v.get_allocator())}, _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,v._m_begin[_m_len]);
        }

        vector(vector&& v) : _m_begin{std::exchange(v,nullptr)}, _m_len{v._m_len}, _m_capacity{v._m_capacity}, _m_alloc{std::move(v._m_alloc)}{}

        template<typename Alloc2> vector(const vector<T,Alloc2>& v,const Alloc& alloc) : _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity}, _m_alloc{alloc} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,v._m_begin[_m_len]);
        }

        template<typename Alloc2> vector(vector<T,Alloc2>&& v,const Alloc& alloc) : _m_begin{}, _m_len{0}, _m_capacity{v._m_capacity}, _m_alloc{alloc} {
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(_m_len=0;_m_len<v._m_len;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::move_if_noexcept(v._m_begin[_m_len]));
        }

        template<typename InputIt,typename=std::enable_if_t<!std::is_integral_v<InputIt>>,typename=typename std::iterator_traits<InputIt>::iterator_category>
            explicit vector(InputIt i1,InputIt i2,const Alloc& alloc = Alloc()) : _m_begin{}, _m_len{}, _m_capacity{16}, _m_alloc{alloc} {
                if constexpr(std::is_base_of_v<std::forward_iterator_tag,typename std::iterator_traits<InputIt>::iterator_category>){
                    _m_capacity = std::distance(i1,i2);
                    _m_capacity--;
                    _m_capacity |= _m_capacity>>1;
                    _m_capacity |= _m_capacity>>2;
                    _m_capacity |= _m_capacity>>4;
                    _m_capacity |= _m_capacity>>8;
                    _m_capacity |= _m_capacity>>16;
                    if constexpr(sizeof(size_type)>4)
                        _m_capacity |= _m_capacity>>32;
                    _m_capacity++;
                    _m_capacity=std::max(_m_capacity,16);
                    _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
                    for(;i1!=i2;i1++,_m_len++)
                        std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,*i1);
                }else{
                    _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
                    for(;i1!=i2;i1++)
                        push_back(*i1);
                }
            }

        explicit vector(std::initializer_list<T> il,const Alloc& alloc=Alloc()) : vector(begin(il),end(il),alloc){}

        explicit vector(size_type num,const T& t=T(),const Alloc& alloc=Alloc()) : _m_begin{}, _m_capacity{num}, _m_len{}, _m_alloc{alloc}{
            _m_capacity--;
            _m_capacity |= _m_capacity>>1;
            _m_capacity |= _m_capacity>>2;
            _m_capacity |= _m_capacity>>4;
            _m_capacity |= _m_capacity>>8;
            _m_capacity |= _m_capacity>>16;
            if constexpr(sizeof(size_type)>4)
                _m_capacity |= _m_capacity>>32;
            _m_capacity++;
            _m_capacity=std::max(_m_capacity,16);
            _m_begin = std::allocator_traits<Alloc>::allocate(_m_alloc,_m_capacity);
            for(;_m_len<num;_m_len++)
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,t);
        }

        const Alloc& get_allocator() const noexcept{
            return _m_alloc;
        }

        vector& operator=(const vector& v) {
            using std::swap;
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value)
                _m_alloc = v._m_alloc;
            vector nvec(v,_m_alloc);
            swap(_m_begin,nvec._m_begin);
            swap(_m_capacity,nvec._m_capacity);
            swap(_m_len,nvec._m_len);
            return *this;
        }

        vector& operator=(vector&& v){
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value){
                if(_m_begin){
                    pointer pbegin = std::exchange(_m_begin,nullptr);
                    // If we throw, we would rather leak then double-destroy.
                    for(;_m_len>0;_m_len--)
                        std::allocator_traits<Alloc>::destroy(_m_alloc,pbegin+_m_len-1);
                    std::allocator_traits<Alloc>::deallocate(_m_alloc,pbegin);
                }
                _m_alloc = std::move(v._m_alloc);
                _m_begin = std::exchange(v._m_begin,nullptr);
                _m_capacity = v._m_capacity;
                _m_len = v._m_len;
            }else{
                using std::swap;
                vector nvec(std::move(v),_m_alloc);
                swap(_m_begin,nvec._m_begin);
                swap(_m_capacity,nvec._m_capacity);
                swap(_m_len,nvec._m_len);
                return *this;
            }
        }

        void swap(vector& rhs) noexcept{
            using std::swap;
            if constexpr(std::allocator_traits<Alloc>::propagate_on_container_swap::value)
                swap(_m_alloc,rhs._m_alloc);
            swap(_m_begin,rhs._m_begin);
            swap(_m_capacity,rhs._m_capacity);
            swap(_m_len,rhs._m_len);
        }

        friend void swap(vector& a,vector& b) noexcept{
            a.swap(b);
        }

        template<typename=std::enable_if_t<std::is_copy_constructible_v<T>>>
        void push_back(const T& t){
            if(_m_len==_m_capacity)
                reallocate(_m_capacity<<1);
            std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,t);
            _m_len++;
        }
        template<typename=std::enable_if_t<std::is_move_constructible_v<T>>>
        void push_back(T&& t){
            if(_m_len==_m_capacity)
                reallocate(_m_capacity<<1);
            std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::move(t));
            _m_len++;
        }

        template<typename... Args,typename=std::enable_if_t<std::is_constructible_v<T,Args&&...>>>
            void emplace_back(Args&&... args){
                if(_m_len==_m_capacity)
                    reallocate(_m_capacity<<1);
                std::allocator_traits<Alloc>::construct(_m_alloc,_m_begin+_m_len,std::forward<Args>(args)...);
                _m_len++;
            }

        void pop_back() noexcept{
            std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+(_m_len--));
        }

        void clear() noexcept{
            for(;_m_len>0;_m_len--)
                std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len-1);
        }

        iterator erase(const_iterator it){
            difference_type pos = it-_m_begin;
            _m_len--;
            for(size_type n = pos;n<_m_len;n++)
                _m_begin[n] = std::move(_m_begin[n+1]);
            std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len);
            return _m_begin+pos;
        }

        iterator erase(const_iterator begin,const_iterator end) {
            difference_type pos = begin-_m_begin;
            difference_type len = end-begin;
            _m_len-=len;
            for(size_type n = pos;n<_m_len;n++)
                _m_begin[n] = std::move(_m_begin[n+len]);
            for(size_type t = _m_len;t<_m_len+len;t++)
                return std::allocator_traits<Alloc>::destroy(_m_alloc,_m_begin+_m_len);
            return _m_begin+pos;
        }

        bool empty() const noexcept{
            return _m_len;
        }

        reference back() noexcept{
            return _m_begin[_m_len-1];
        }
        reference front() noexcept{
            return _m_begin[0];
        }

        const_reference back() const noexcept{
            return _m_begin[_m_len-1];
        }
        const_reference front() const noexcept{
            return _m_begin[0];
        }


        pointer data() noexcept{
            return _m_begin;
        }

        friend vector<T,Alloc>::pointer data(vector<T,Alloc>& v) noexcept{
            return v.data();
        }

        const_pointer data()const noexcept{
            return _m_begin;
        }

        friend vector<T,Alloc>::pointer data(const vector<T,Alloc>& v) noexcept{
            return v.data();
        }

        size_type size() const noexcept{
            return _m_len;
        }

        friend vector<T,Alloc>::size_type size(const vector<T,Alloc>& v) noexcept{
            return v.size();
        }

        size_type capacity()const noexcept{
            return _m_capacity;
        }

        void reserve(size_type ncap) const noexcept{
            if(_m_capacity<ncap){
                ncap--;
                ncap |= ncap>>1;
                ncap |= ncap>>2;
                ncap |= ncap>>4;
                ncap |= ncap>>8;
                ncap |= ncap>>16;
                if constexpr(sizeof(size_type)>4)
                    ncap |= ncap>>32;
                ncap++;
                reallocate(ncap);
            }
        }

        void shrink_to_fit(){
            size_type ncap = _m_len;
            ncap--;
            ncap |= ncap>>1;
            ncap |= ncap>>2;
            ncap |= ncap>>4;
            ncap |= ncap>>8;
            ncap |= ncap>>16;
            if constexpr(sizeof(size_type)>4)
                ncap |= ncap>>32;
            ncap++;
            reallocate(std::max(ncap,16));
        }

        iterator begin()noexcept{
            return _m_begin;
        }

        const_iterator begin()const noexcept{
            return _m_begin;
        }

        iterator cbegin()const noexcept{
            return _m_begin;
        }

        iterator end()noexcept{
            return _m_begin+_m_len;
        }

        const_iterator end()const noexcept{
            return _m_begin+_m_len;
        }

        const_iterator cend()const noexcept{
            return _m_begin+_m_len;
        }

        friend typename vector<T,Alloc>::iterator begin(vector<T,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename vector<T,Alloc>::const_iterator begin(const vector<T,Alloc>& v) noexcept{
            return v.begin();
        }

        friend typename vector<T,Alloc>::const_iterator cbegin(const vector<T,Alloc>& v) noexcept{
            return v.cbegin();
        }

        friend typename vector<T,Alloc>::iterator end(vector<T,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename vector<T,Alloc>::const_iterator end(const vector<T,Alloc>& v) noexcept{
            return v.end();
        }

        friend typename vector<T,Alloc>::const_iterator cend(const vector<T,Alloc>& v) noexcept{
            return v.cend();
        }

        reverse_iterator rbegin()noexcept{
            return reverse_iterator{end()};
        }

        const_reverse_iterator rbegin()const noexcept{
            return const_reverse_iterator{end()};
        }

        const_reverse_iterator crbegin()const noexcept{
            return const_reverse_iterator{cend()};
        }
        
        reverse_iterator rend()noexcept{
            return reverse_iterator{begin()};
        }

        const_reverse_iterator rend()const noexcept{
            return const_reverse_iterator{begin()};
        }

        const_reverse_iterator crend()const noexcept{
            return const_reverse_iterator{cbegin()};
        }

        friend typename vector<T,Alloc>::reverse_iterator rbegin(vector<T,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator rbegin(const vector<T,Alloc>& v) noexcept{
            return v.rbegin();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator crbegin(const vector<T,Alloc>& v) noexcept{
            return v.crbegin();
        }

        friend typename vector<T,Alloc>::reverse_iterator rend(vector<T,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator rend(const vector<T,Alloc>& v) noexcept{
            return v.rend();
        }

        friend typename vector<T,Alloc>::const_reverse_iterator crend(const vector<T,Alloc>& v) noexcept{
            return v.crend();
        }

        reference operator[](size_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](size_type sz) const noexcept{
            return _m_begin[sz];
        }

        reference operator[](difference_type sz) noexcept{
            return _m_begin[sz];
        }

        const_reference operator[](difference_type sz) const noexcept{
            return _m_begin[sz];
        }
    };
}

#endif 