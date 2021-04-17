#include "semantic.hpp"

namespace lccc::cc {

program analyze(ast tree) {
    std::vector<std::unique_ptr<p_item>> vec{};
    vec.push_back(std::make_unique<p_function>());
    return program(std::move(vec));
}

}
