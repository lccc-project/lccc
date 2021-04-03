#include <string>
#include <xlang++/Visit.hpp>

#include "parse.hpp"
#include "preprocess.hpp"
#include "semantic.hpp"

namespace lccc::cc {

class CVisitor : public lccc::xlang::FileVisitor {
  private:
    program program;
  public:
    virtual void visitInputFile(FILE *file) override;
};

void CVisitor::visitInputFile(FILE *file) {
    std::string processed = preprocess(file); // Phase 1-6
    ast tree = parse(processed); // Phase 7 part 1
    program = analyze(std::move(tree)); // Phase 7 part 2
}

extern "C" {
    XLANG_EXPORT lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_plugin_main(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args) {
        return lccc::make_unique<CVisitor>();
    }
}

}

