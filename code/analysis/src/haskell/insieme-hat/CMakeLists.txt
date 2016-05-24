project(insieme_analysis_haskell NONE)
cmake_minimum_required(VERSION 3.2)

set( stack_home $ENV{INSIEME_LIBS_HOME}/stack-latest CACHE PATH "Stack Home Directory" ) 		# TODO: use e.g. insieme_find_package
set( stack_root ${stack_home}/stack-root_$ENV{USER} )

set( gmp_lib_dir $ENV{INSIEME_LIBS_HOME}/gmp-latest/lib CACHE PATH "GMP library directory" )		# TODO: use e.g. insieme_find_package
set( z_lib_dir $ENV{INSIEME_LIBS_HOME}/zlib-latest/lib CACHE PATH "ZLIB library directory" )		# TODO: use e.g. insieme_find_package

add_custom_target(
    insieme_analysis_haskell
    ALL

    # install adequate ghc / cabal lib
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        "LD_LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        ${stack_home}/bin/stack --allow-different-user setup

    # install dependencies and build package
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        "LD_LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        ${stack_home}/bin/stack --allow-different-user build --force-dirty --ghc-options='-lHSrts-ghc7.10.3'

    COMMAND chmod -R g+w ${stack_root}

    # copy library (package key in name) to binary dir
    COMMAND ${CMAKE_COMMAND} -E env
        "STACK_ROOT=${stack_root}"
        "LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        "LD_LIBRARY_PATH=${gmp_lib_dir}:${z_lib_dir}"
        ${stack_home}/bin/stack --allow-different-user runhaskell --package Glob --package directory utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# exports
set(LIBHSRTS_DIR ${stack_root}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(insieme_analysis_haskell ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
