#ifndef LCCC_CC_PROGRAM_HPP
#define LCCC_CC_PROGRAM_HPP

#include <memory>
#include <string_view>
#include <vector>

namespace lccc::cc {

enum class p_type_id {
    None,
    Void,
    I32
};

class p_type {
  private:
    p_type_id m_id;
};

class p_item {
  public:
    virtual ~p_item() = 0;
};

class p_statement {
  public:
    virtual ~p_statement() = 0;
};

class p_function : public p_item {
  private:
    p_type m_return_type;
    std::string_view m_name;
    std::vector<p_type> m_params;
    std::vector<std::unique_ptr<p_statement>> m_statments;
};

class program {
  private:
    std::vector<std::unique_ptr<p_item>> m_items;
  public:
    program(std::vector<std::unique_ptr<p_item>> items): m_items(std::move(items)) {}
    std::vector<std::unique_ptr<p_item>>& items();
};

}

#endif
