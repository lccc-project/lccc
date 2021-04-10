#include "program.hpp"

namespace lccc::cc {

std::vector<std::unique_ptr<p_item>>& program::items() {
    return m_items;
}

}
