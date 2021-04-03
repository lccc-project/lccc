#ifndef LCCC_CC_PROGRAM_HPP
#define LCCC_CC_PROGRAM_HPP

namespace lccc::cc {

class p_item {
  public:
    virtual ~p_item() = 0;
};

class program {
  private:
    std::vector<std::unique_ptr<p_item>> items;
};

}

#endif
