project(insieme_analysis_haskell_libHS NONE)
cmake_minimum_required(VERSION 3.2)

add_custom_target(
    insieme_analysis_haskell
    ALL

    # install adequate ghc / cabal lib
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=$ENV{INSIEME_LIBS_HOME}/stack-latest/stack-root"
        "LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        "LD_LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        $ENV{INSIEME_LIBS_HOME}/stack-latest/bin/stack setup

    # install dependencies and build package
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=$ENV{INSIEME_LIBS_HOME}/stack-latest/stack-root"
        "LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        "LD_LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        $ENV{INSIEME_LIBS_HOME}/stack-latest/bin/stack build

    # copy library (package key in name) to binary dir
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=$ENV{INSIEME_LIBS_HOME}/stack-latest/stack-root"
        "LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        "LD_LIBRARY_PATH=$ENV{INSIEME_LIBS_HOME}/gmp-latest/lib:$ENV{INSIEME_LIBS_HOME}/zlib-latest/lib"
        $ENV{INSIEME_LIBS_HOME}/stack-latest/bin/stack runhaskell --package Glob --package directory utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# exports
set(LIBHSRTS_DIR $ENV{INSIEME_LIBS_HOME}/stack-latest/stack-root/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(insieme_analysis_haskell ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
