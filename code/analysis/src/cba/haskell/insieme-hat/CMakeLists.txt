get_filename_component(CABAL_PATH_ENTRY "${CABAL_EXECUTABLE}" DIRECTORY)
set(CABAL_PATH_ENTRY "${CABAL_PATH_ENTRY}:")

get_filename_component(GMP_LIBRARIES_DIR "${GMP_LIBRARIES}" PATH)
get_filename_component(ZLIB_LIBRARIES_DIR "${ZLIB_LIBRARIES}" PATH)

set(haskell_env
  ${CMAKE_COMMAND} -E env
  "PATH=${CABAL_PATH_ENTRY}${THIRD_PARTY_DIR}/ghc/bin/:$ENV{PATH}"
  "LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}:$ENV{LIBRARY_PATH}"
  "LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}:$ENV{LD_LIBRARY_PATH}"
  "HOME=${CMAKE_CURRENT_BINARY_DIR}/cabal-home"
)


set(cabal ${haskell_env} ${CABAL_EXECUTABLE})
set(runhaskell ${haskell_env} runhaskell)
set(cabal_file ${CMAKE_CURRENT_SOURCE_DIR}/insieme-hat.cabal)

list(APPEND HAT_PACKAGE_PATHS_LIST "${CMAKE_CURRENT_SOURCE_DIR}")

# Prepare command list for creating cabal superproject dir
unset(HAT_PACKAGES_SYMLINK_COMMANDS)
unset(HAT_PACKAGE_PATHS_REL_LIST)
foreach(pkg ${HAT_PACKAGE_PATHS_LIST})
	string(REGEX REPLACE "[/\\]$" "" pkg_trailing "${pkg}")
	get_filename_component(pkg_dirname "${pkg_trailing}" NAME)
	list(APPEND HAT_PACKAGES_SYMLINK_COMMANDS
		COMMAND ${CMAKE_COMMAND} -E create_symlink "${pkg}" "${CMAKE_BINARY_DIR}/hat-vanilla-project/${pkg_dirname}"
		COMMAND ${CMAKE_COMMAND} -E create_symlink "${pkg}" "${CMAKE_BINARY_DIR}/hat-profiling-project/${pkg_dirname}")
	list(APPEND HAT_PACKAGE_PATHS_REL_LIST "./${pkg_dirname}")
endforeach()


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

string(REPLACE ";" " " HAT_PACKAGE_PATHS "${HAT_PACKAGE_PATHS_REL_LIST}")
configure_file(cabal.project.in ${CMAKE_BINARY_DIR}/hat-vanilla-project/cabal.project)
configure_file(cabal.project.in ${CMAKE_BINARY_DIR}/hat-profiling-project/cabal.project)
configure_file(cabal.project.local.in ${CMAKE_BINARY_DIR}/hat-profiling-project/cabal.project.local)

string (REPLACE ";" " " haskell_env_cmd "${haskell_env}")
configure_file(haskell-env.sh.in ${CMAKE_BINARY_DIR}/haskell-env.sh)

add_custom_target(insieme_hat
	# Fix rebuilding of .chs files
	COMMAND ${CMAKE_COMMAND} -E touch "${CMAKE_CURRENT_SOURCE_DIR}/src/Insieme/Inspire/IR/NodeType.chs"

	# install dependencies and build package
	COMMAND ${cabal} update
	# -j1 because of cabal#4144
	COMMAND ${cabal} new-build -j --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle --project-file=${CMAKE_CURRENT_BINARY_DIR}/cabal.project -v1 flib:insieme-hat
	# copy library to binary dir
	COMMAND ${runhaskell} ${CMAKE_CURRENT_SOURCE_DIR}/scripts/copyLib.hs ${cabal_file} "%comp:insieme-hat" ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}

	# Create symlinks for hat cabal superproject dir
	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/hat-vanilla-project"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle" "${CMAKE_BINARY_DIR}/hat-vanilla-project/dist-newstyle"

	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/hat-profiling-project"

	BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so

	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}

	COMMENT "Building Insieme HAT"

	${HAT_PACKAGES_SYMLINK_COMMANDS}
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
		COMMAND ${haskell_env} ./scripts/hat-exec ${CMAKE_CURRENT_BINARY_DIR} test
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
		COMMAND ${haskell_env} ./scripts/hat-haddock ${CMAKE_CURRENT_BINARY_DIR} "%library"
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
endif()

add_custom_target(clean_insieme_hat
	COMMAND ${CMAKE_COMMAND} -E remove "${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle/"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/cabal-home/.cabal"
)

set(insieme_hat ${CMAKE_CURRENT_BINARY_DIR}/libinsieme-hat.so PARENT_SCOPE)
