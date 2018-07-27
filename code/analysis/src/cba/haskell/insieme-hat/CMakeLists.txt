file(GLOB_RECURSE INSIEME_HAT_SRCS insieme-hat.cabal *.hs *.chs)

set(haskell_env sh ${CMAKE_BINARY_DIR}/haskell-env.sh)

set(cabal ${haskell_env} ${CABAL_EXECUTABLE})
set(runhaskell ${haskell_env} runhaskell)
set(cabal_file ${CMAKE_CURRENT_SOURCE_DIR}/insieme-hat.cabal)

get_target_property(INSIEME_CORE_SOURCE_DIR core SOURCE_DIR)


list(APPEND HAT_PACKAGE_PATHS_LIST "${CMAKE_CURRENT_SOURCE_DIR}")

# Allows to specify a different compiler to use for compiling and linking haskell.
# Normally you wouldn't want to do this, but in case you are using a wrapper like distcc (or maybe also ccache), you might
# need to set this variable to the actual compiler you are using, since e.g. distcc doesn't support the @file argument syntax.
if(NOT HASKELL_C_COMPILER)
	set(HASKELL_C_COMPILER "${CMAKE_C_COMPILER}")
endif()
message(STATUS "Using ${HASKELL_C_COMPILER} for compiling Haskell")


## Paths to Haskell packages to include in the cabal.project file.
set(HAT_COMPILER_PATH "${THIRD_PARTY_DIR}/ghc/bin/ghc")
string(REPLACE ";" " " HAT_PACKAGE_PATHS "${HAT_PACKAGE_PATHS_LIST}")
configure_file(cabal.project.in cabal.project)
configure_file(cabal.project.in ${CMAKE_BINARY_DIR}/hat-profiling-project/cabal.project)
configure_file(cabal.project.local.in ${CMAKE_BINARY_DIR}/hat-profiling-project/cabal.project.local)

configure_file(haskell-env.sh.in ${CMAKE_BINARY_DIR}/haskell-env.sh @ONLY)

# mostly for get_target_property in allscale-hat
add_custom_target(insieme_hat DEPENDS libinsieme-hat.so)

if(IS_DIRECTORY "${CMAKE_BINARY_DIR}/hat-vanilla-project"
    AND NOT IS_SYMLINK "${CMAKE_BINARY_DIR}/hat-vanilla-project")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E rename "${CMAKE_BINARY_DIR}/hat-vanilla-project" "${CMAKE_BINARY_DIR}/_hat-vanilla-project_old")
endif()

add_custom_command(
        OUTPUT libinsieme-hat.so

	# Fix rebuilding of .chs files
	COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_SOURCE_DIR}/src/Insieme/Inspire/IR/NodeType.chs"

        # needed by copyLib
        COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_CURRENT_BINARY_DIR}" "${CMAKE_BINARY_DIR}/hat-vanilla-project"

	# install dependencies and build package
	COMMAND ${cabal} update
	COMMAND ${cabal} new-build --project-file=${CMAKE_CURRENT_BINARY_DIR}/cabal.project flib:insieme-hat

	# copy library to binary dir
	COMMAND ${runhaskell} ${CMAKE_CURRENT_SOURCE_DIR}/scripts/copyLib.hs ${cabal_file} "%flib%opt:insieme-hat" ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}

        DEPENDS ${INSIEME_HAT_SRCS}
        DEPENDS ${INSIEME_CORE_SOURCE_DIR}/include/insieme/core/ir_nodes.def
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cabal.project.in ${CMAKE_CURRENT_SOURCE_DIR}/cabal.project.local.in

	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}

	COMMENT "Building Insieme HAT"
)

if(BUILD_TESTS)
	add_custom_target(insieme_hat_build_tests
		ALL
		DEPENDS analysis
		COMMAND ${cabal} new-build -j --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle --project-file=${CMAKE_CURRENT_BINARY_DIR}/cabal.project -v1 insieme-hat:tests
		WORKING_DIRECTORY ${CMAKE_CURRENT_BUILD_DIR}
	)

	add_test(
		NAME insieme_hat_run_tests
		COMMAND ${haskell_env} ./scripts/hat-exec ${CMAKE_BINARY_DIR} %test%opt:test
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)

	set_tests_properties(insieme_hat_run_tests PROPERTIES DEPENDS insieme_hat_build_tests)
endif()

if(MSVC)
	set_target_properties(insieme_hat PROPERTIES FOLDER analysis)
endif()

if(BUILD_DOCS)
	add_custom_command(
		TARGET insieme_hat
		POST_BUILD
		COMMAND ${haskell_env} ./scripts/hat-haddock ${cabal_file} "%library%opt" ${CMAKE_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
endif()

# cleanup
set_directory_properties(
	PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
	"${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so;${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle"
)

# force cabal home cleanup
add_custom_target(clean_insieme_hat
	COMMAND ${CMAKE_COMMAND} -E remove "${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle/"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/cabal-home/.cabal"
)

set(insieme_hat ${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so PARENT_SCOPE)
