project(insieme_analysis_haskell NONE)
cmake_minimum_required(VERSION 3.2)

insieme_find_package(NAME GMP)
insieme_find_package(NAME ZLIB)

set(stack_home $ENV{INSIEME_LIBS_HOME}/stack-latest CACHE PATH "Stack Home Directory")
set(stack_root ${stack_home}/stack-root_$ENV{USER})

# check for (patched) haskell stack
if(NOT (${CMAKE_GENERATOR} MATCHES "Visual Studio"))
    if(NOT EXISTS ${stack_home}/bin/stack)
        MESSAGE(FATAL_ERROR "Coulnd't find patched Haskell stack")
    endif()
endif()

add_custom_target(
    insieme_analysis_haskell

    # create STACK_ROOT folder to prevent hideous error
    COMMAND ${CMAKE_COMMAND} -E make_directory ${stack_root}

    # install adequate ghc / cabal lib
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack --ghc-variant standard setup

    # binarys like alex and c2hs have to be installed manually
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack --ghc-variant standard build alex
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack --ghc-variant standard build c2hs

    # install dependencies and build package
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack --ghc-variant standard build

    # copy library (package key in name) to binary dir
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        "LD_LIBRARY_PATH=${GMP_ROOT}/lib:${ZLIB_ROOT}/lib"
        ${stack_home}/bin/stack --ghc-variant standard runhaskell --package Glob --package directory utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# runtime library exports
set(LIBHSRTS_DIR ${stack_root}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(LIBHSRTS ${stack_root}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts/libHSrts-ghc7.10.3.so PARENT_SCOPE)

set(insieme_analysis_haskell ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
