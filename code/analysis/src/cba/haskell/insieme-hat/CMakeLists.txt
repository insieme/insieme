find_package(GMP ${GMP_VERSION} REQUIRED)
get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)

find_package(ZLIB ${ZLIB_VERSION} REQUIRED)
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

# TODO find_package(STACK REQUIRED)
if(NOT STACK_ROOT AND DEFINED $ENV{STACK_ROOT})
	set(STACK_ROOT "$ENV{STACK_ROOT}" CACHE PATH "Haskell Stack base directory location")
endif()
if(NOT STACK_ROOT)
	message(FATAL_ERROR "Haskell Stack NOT found")
endif()

set(stack_data ${STACK_ROOT}/stack-root_$ENV{USER})

# calling stack
set(stack
    ${CMAKE_COMMAND} -E env
    "STACK_ROOT=${stack_data}"
    "LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
    "LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
    ${STACK_ROOT}/bin/stack --ghc-variant standard
)

add_custom_target(
    analysis_haskell

    # create STACK_ROOT folder to prevent hideous error
    COMMAND ${CMAKE_COMMAND} -E make_directory ${stack_data}

    # install adequate ghc / cabal lib
    COMMAND ${stack} setup

    # binarys like alex and c2hs have to be installed manually, do not merge
    # these lines
    COMMAND ${stack} build alex
    COMMAND ${stack} build c2hs

    # install dependencies and build package
    COMMAND ${stack} build

    # copy library (package key in name) to binary dir
    COMMAND ${stack} runhaskell utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

if(BUILD_DOCS)
    add_custom_command(
        TARGET analysis_haskell
        POST_BUILD
        COMMAND ${stack} haddock
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()

# runtime library exports
set(LIBHSRTS_DIR ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(LIBHSRTS ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts/libHSrts-ghc7.10.3.so PARENT_SCOPE)

set(analysis_haskell ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
