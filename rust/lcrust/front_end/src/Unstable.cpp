#include <Unstable.hpp>
#include <utility>

namespace lccc::lcrust{
    bool Unstable::is_feature_enabled(const std::string& name)const noexcept{
        return this->active_features.count(name);
    }

    void Unstable::enable_feature(std::string name){
        this->active_features.insert(std::move(name));
    }

    void Unstable::push_feature_state(){
        this->stacked_features.push(std::move(this->active_features));
        this->active_features.clear();
    }

    void Unstable::pop_feature_state(){
        if(this->stacked_features.empty())
            this->active_features.clear();
        else
        {
            this->active_features = this->stacked_features.top();
            this->stacked_features.pop();
        }
        
    }
}