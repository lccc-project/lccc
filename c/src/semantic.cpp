#include "semantic.hpp"

namespace lccc::cc {

program analyze(ast tree) {
    return program(std::vector<std::unique_ptr<p_item>>(std::make_unique<p_function>()));
}

}
