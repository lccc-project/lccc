#ifndef LCRUST_FRONTEND_PROCMACRO_HPP_2021_06_22_15_47_36
#define LCRUST_FRONTEND_PROCMACRO_HPP_2021_06_22_15_47_36

#include <xlang++/Layout.h>

namespace lccc::lcrust{
    struct ProcMacro{
    private:
        lccc::unique_ptr<struct ProcMacroHandle> hdl;

    };

    struct ProcMacroDiagnostic{

    };
}

#endif /* LCRUST_FRONTEND_PROCMACRO_HPP_2021_06_22_15_47_36 */