find_package(GMP ${GMP_VERSION})
if(NOT GMP_FOUND)
	message(WARNING "GMP NOT found")
endif()
get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)

find_package(ZLIB ${ZLIB_VERSION})
if(NOT ZLIB_FOUND)
	message(WARNING "ZLIB NOT found")
endif()
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

# TODO find_package(STACK REQUIRED)
if(NOT STACK_ROOT AND DEFINED $ENV{STACK_ROOT})
	set(STACK_ROOT "$ENV{STACK_ROOT}" CACHE PATH "Haskell Stack base directory location")
endif()
if(NOT STACK_ROOT)
	message(WARNING "Haskell Stack NOT found")
endif()

file(GLOB_RECURSE insieme_hat_srcs insieme-hat.cabal src/*.hs)

set(stack_data ${STACK_ROOT}/stack-root_$ENV{USER})

# calling stack
set(stack
    ${CMAKE_COMMAND} -E env
    "STACK_ROOT=${stack_data}"
    "LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
    "LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
    ${STACK_ROOT}/bin/stack --ghc-variant standard
)

add_custom_command(
	OUTPUT libHSinsieme-hat.so

	# create STACK_ROOT folder to prevent hideous error
	COMMAND ${CMAKE_COMMAND} -E make_directory ${stack_data}

	# install adequate ghc / cabal lib
	COMMAND ${stack} setup

	# binarys like alex and c2hs have to be installed manually, do not
	# merge these lines
	COMMAND ${stack} build alex
	COMMAND ${stack} build c2hs

	# install dependencies and build package
	COMMAND ${stack} build

	# copy library (package key in name) to binary dir
	COMMAND ${stack} runhaskell utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

	DEPENDS ${insieme_hat_srcs}

	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMENT "Building Insieme HAT"
)

add_custom_target(insieme_hat DEPENDS libHSinsieme-hat.so)

if(MSVC)
	set_target_properties(insieme_hat PROPERTIES FOLDER analysis)
endif()

if(BUILD_DOCS)
    add_custom_command(
        TARGET insieme_hat
        POST_BUILD
        COMMAND ${stack} haddock
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()

# runtime library exports
set(LIBHSRTS_DIR ${stack_data}/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts PARENT_SCOPE)
set(LIBHSRTS ${stack_data}/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts/libHSrts-ghc8.0.2.so PARENT_SCOPE)

set(insieme_hat ${CMAKE_CURRENT_BINARY_DIR}/libHSinsieme-hat.so PARENT_SCOPE)
