get_filename_component(CABAL_PATH_ENTRY "${CABAL_EXECUTABLE}" DIRECTORY)
set(CABAL_PATH_ENTRY "${CABAL_PATH_ENTRY}:")

file(GLOB_RECURSE insieme_hat_srcs insieme-hat.cabal src/*.hs src/*.chs tests/*.hs bench/*.hs)

get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

set(haskell_env
  ${CMAKE_COMMAND} -E env
  "PATH=${CABAL_PATH_ENTRY}${THIRD_PARTY_DIR}/ghc/bin/:$ENV{PATH}"
  "LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
  "LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
  "HOME=${CMAKE_CURRENT_BINARY_DIR}/cabal-home"
)


set(cabal ${haskell_env} ${CABAL_EXECUTABLE})
set(runhaskell ${haskell_env} runhaskell)
set(cabal_file ${CMAKE_CURRENT_SOURCE_DIR}/insieme-hat.cabal)

## Paths to Haskell packages to include in the cabal.project file.
set(HAT_PACKAGE_PATHS "${CMAKE_CURRENT_SOURCE_DIR} ${HAT_PACKAGE_PATHS}")
configure_file(cabal.project.in cabal.project)

string (REPLACE ";" " " haskell_env_cmd "${haskell_env}")
configure_file(haskell-env.sh.in ${PROJECT_BINARY_DIR}/haskell-env.sh)

if(ANALYSIS_HASKELL_PROFILING)
  add_custom_command(
	OUTPUT libinsieme-hat.so

	# install dependencies and build package
	COMMAND ${cabal} update
	# -j1 because of cabal#4144
	COMMAND ${cabal} new-build -j --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle --project-file=${CMAKE_CURRENT_BINARY_DIR}/cabal.project -v1 --enable-profiling -fprofiling

	DEPENDS ${insieme_hat_srcs} scripts/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}/cabal.project
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMENT "Building Insieme HAT (with profiling)"
	)
else()
  add_custom_command(
	OUTPUT libinsieme-hat.so

	# install dependencies and build package
	COMMAND ${cabal} update
	# -j1 because of cabal#4144
	COMMAND ${cabal} new-build -j --builddir=${CMAKE_CURRENT_BINARY_DIR}/dist-newstyle --project-file=${CMAKE_CURRENT_BINARY_DIR}/cabal.project -v1 --enable-tests --enable-benchmarks
	# copy library to binary dir
	COMMAND ${runhaskell} scripts/copyLib.hs ${cabal_file} "%comp:insieme-hat" ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}

	DEPENDS ${insieme_hat_srcs} scripts/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}/cabal.project
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMENT "Building Insieme HAT"
)
endif()

add_custom_target(insieme_hat DEPENDS libinsieme-hat.so)

add_test(
  NAME insieme_hat_tests
  COMMAND ${haskell_env} ./scripts/hat-exec ${CMAKE_CURRENT_BINARY_DIR} test
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

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
