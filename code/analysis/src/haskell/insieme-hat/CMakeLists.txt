project(insieme_analysis_haskell NONE)
cmake_minimum_required(VERSION 3.2)

insieme_find_package(NAME GMP)
insieme_find_package(NAME ZLIB)

set( stack_home $ENV{INSIEME_LIBS_HOME}/stack-latest CACHE PATH "Stack Home Directory" )
set( stack_root ${stack_home}/stack-root_$ENV{USER} )

add_custom_target(
    insieme_analysis_haskell

    # create STACK_ROOT folder to prevent hideous error
    COMMAND ${CMAKE_COMMAND} -E make_directory ${stack_root}

    # install adequate ghc / cabal lib
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack setup

    # binarys like alex and c2hs have to be installed manually
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack build alex
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack build c2hs

    # install dependencies and build package
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack build --ghc-options='-lHSrts-ghc7.10.3'

    # copy library (package key in name) to binary dir
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack runhaskell --package Glob --package directory utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# exports
# for some strange reason stack sometimes chooses ghc-gmp4
set(LIBHSRTS_DIR  ${stack_root}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(LIBHSRTS_DIR2 ${stack_root}/programs/x86_64-linux/ghc-gmp4-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(insieme_analysis_haskell ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
