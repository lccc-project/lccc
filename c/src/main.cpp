#include <xlang++/Visit.hpp>

class CVisitor : public lccc::xlang::FileVisitor {
  public:
    virtual void visitInputFile(FILE *file) override;
};

void CVisitor::visitInputFile(FILE *file) {

}

extern "C" {
    XLANG_EXPORT lccc::unique_ptr<lccc::xlang::FileVisitor> xlang_plugin_main(lccc::xlang::FileVisitor *parent, lccc::span<lccc::string_view> args) {
        return lccc::make_unique<CVisitor>();
    }
}
