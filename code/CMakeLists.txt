cmake_minimum_required(VERSION 3.5)
project(insieme LANGUAGES C CXX)

# -- Module Path
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../cmake)

# -- Prefix Path
set(THIRD_PARTY_DIR ${CMAKE_SOURCE_DIR}/../third_party CACHE STRING "Third Party Library Directory")
file(GLOB prefix_paths ${THIRD_PARTY_DIR}/*)
list(APPEND CMAKE_PREFIX_PATH ${prefix_paths})

# -- Project Settings
include(build_settings)
include(coverage)
include(doxygen)
include(git_metadata)

# -- Dependency Settings
include(dependencies/boost)
include(dependencies/cudd)
include(dependencies/llvm)
include(dependencies/googletest)
include(dependencies/opencl)
include(dependencies/papi)
include(dependencies/pthread)
include(dependencies/valgrind)

# -- CMake Modules
include(add_module)
include(add_souffle)
include(file_globs)
include(msvc_source_group)
include(nproc)
include(run_ruby)

# -- Insieme
include(insieme_specific)

# -- Project Modules
add_subdirectory(common)

if(BUILD_COMPILER)
	add_subdirectory(analysis)
	add_subdirectory(annotations)
	add_subdirectory(backend)
	add_subdirectory(core)
	add_subdirectory(driver)
	add_subdirectory(frontend)
	add_subdirectory(transform)
	add_subdirectory(utils)
endif()

if(BUILD_RUNTIME)
	add_subdirectory(runtime)
endif()

if(BUILD_DOCS)
	add_subdirectory(../docs/insieme_dev_doc ${PROJECT_BINARY_DIR}/insieme_dev_doc)
endif()

# -- Integration Test Files
add_subdirectory(../test ${PROJECT_BINARY_DIR}/test)
