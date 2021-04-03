#ifndef LCCC_CC_AST_HPP
#define LCCC_CC_AST_HPP

#include <memory>
#include <vector>

namespace lccc::cc {

class a_item {
  public:
    virtual ~a_item() = 0;
};

class ast {
  private:
    std::vector<std::unique_ptr<a_item>> items;
};

}

#endif // LCCC_CC_AST_HPP
