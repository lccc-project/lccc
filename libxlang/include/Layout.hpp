//
// Created by chorm on 2020-05-12.
//

#ifndef LCNIX_LAYOUT_HPP
#define LCNIX_LAYOUT_HPP

#include <memory>
#include <cstdint>
#include <bitset>
#include <vector>
#include <variant>

namespace lccc::xlang::layout{
    struct Layout;
    struct ScalarLayout{
    private:
        std::uint16_t size;
        std::uint16_t align;
        bool floatT:1;
        bool nonzero:1;
        bool signedT:1;
    public:
        static ScalarLayout integerLayout(std::uint32_t size,const char* arch);
        static ScalarLayout floatLayout(std::uint32_t size,const char* arch);
        constexpr ScalarLayout(std::uint16_t size,std::uint16_t align):size{size},align{align},floatT{},nonzero{},signedT{}{}
        constexpr ScalarLayout asSigned()const{
            ScalarLayout copy{*this};
            copy.signedT = true;
            return copy;
        }
        constexpr ScalarLayout asNonZero()const{
            ScalarLayout copy{*this};
            copy.nonzero = true;
            return copy;
        }
        constexpr ScalarLayout aligned(std::uint16_t alignment)const{
            ScalarLayout copy{*this};
            copy.align = alignment;
            copy.size +=(alignment-(copy.size%alignment))%alignment;
            return copy;
        }
        constexpr std::uint64_t layout_alignment()const{
            return this->align;
        }
        constexpr std::uint64_t layout_size()const{
            return this->size;
        }
    };
    struct SumLayout{
    private:
        std::vector<std::unique_ptr<Layout>> elements;
        std::uint64_t size;
        std::uint64_t align;
        bool transparent:1;
    public:
        std::uint64_t layout_alignment()const{
            return this->align;
        }
        std::uint64_t layout_size()const{
            return this->size;
        }
    };
    struct ProductLayout{
    private:
        std::vector<std::unique_ptr<Layout>> elements;
        std::uint64_t size;
        std::uint64_t align;
        bool transparent:1;
        bool preserveOrder:1;
    public:
        std::uint64_t layout_alignment()const{
            return this->align;
        }
        std::uint64_t layout_size()const{
            return this->size;
        }
    };

    struct Layout{
    private:
        std::variant<ScalarLayout,SumLayout,ProductLayout> item;
    public:
#pragma clang diagnostic push
#pragma ide diagnostic ignored "cppcoreguidelines-pro-type-member-init"
        Layout(ScalarLayout scalar):item{scalar}{}
        Layout(SumLayout sum):item{std::move(sum)}{}
        Layout(ProductLayout prod):item{std::move(prod)}{}
#pragma clang diagnostic pop
        std::uint64_t layout_alignment()const{
            return std::visit(this->item,[](auto&& a){return a.layout_alignment();});
        }
        std::uint64_t layout_size()const{
            return std::visit(this->item,[](auto&& a){return a.layout_size();});
        }
    };
}

#endif //LCNIX_LAYOUT_HPP
