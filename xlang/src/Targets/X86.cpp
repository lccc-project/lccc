
#include "X86.hpp"

namespace lccc::xlang
{
    
    const lccc::string_view x86_target_features[]{
        "80387"_sv,
        "softfp"_sv,
        "mmx"_sv,
        "sse"_sv,
        "sse2"_sv,
        "sse3"_sv,
        "ssse3"_sv,
        "sse4"_sv,
        "sse4a"_sv,
        "sse4.1"_sv,
        "sse4.2"_sv,
        "avx"_sv,
        "avx2"_sv,
        "avx512f"_sv,
        "avx512pf"_sv,
        "avx512er"_sv,
        "avx512vl"_sv,
        "avx512bw"_sv,
        "avx512dq"_sv,
        "avx512ifma"_sv,
        "avx512vbmi"_sv,
        "sha"_sv,
        "aes"_sv,
        "pclmul"_sv,
        "clflushopt"_sv,
        "clwb"_sv,
        "fsgsbase"_sv,
        "ptwrite"_sv,
        "rdrand"_sv,
        "rdseed"_sv,
        "f16c"_sv,
        "fma"_sv,
        "pconfig"_sv,
        "wbnoinvd"_sv,
        "fma4"_sv,
        "prfchw"_sv,
        "bmi"_sv,
        "bmi2"_sv,
        "cx16"_sv,
    };

    const lccc::string_view i386_target_features[]{
        "80387"_sv,
        "softfp"_sv,
    };

    const MachineProperties i386_machine{
        i386_target_features
    };

    const ArchProperties x86_architecture{
        0x1f,
        {},
        x86_target_features,
        {},
        &i386_machine,
    };
     
}
