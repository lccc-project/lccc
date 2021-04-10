add_xlang_plugin(frontend-c
    src/main.cpp
    src/parse.cpp src/parse.hpp
    src/preprocess.cpp src/preprocess.hpp
    src/program.cpp src/program.hpp
    src/semantic.cpp src/semantic.hpp
)
install_xlang_plugin(frontend-c)
