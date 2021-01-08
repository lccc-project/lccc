#ifndef LCCC_LCRUST_FRONTEND_UNSTABLE_HPP_2021_01_07_19_50_11
#define LCCC_LCRUST_FRONTEND_UNSTABLE_HPP_2021_01_07_19_50_11

#include <set>
#include <stack>
#include <string>

namespace lccc::lcrust{
    /// 
    /// A type that represents unstable features enabled in a rust crate.
    struct Unstable{
    private:
        std::set<std::string> active_features;
        std::stack<std::set<std::string>> stacked_features;
    public:
        
        bool is_feature_enabled(const std::string& name) const noexcept;
        void enable_feature(std::string name);

        void push_feature_state();
        void pop_feature_state();
    };
}

#endif /*LCCC_LCRUST_FRONTEND_UNSTABLE_HPP_2021_01_07_19_50_11*/