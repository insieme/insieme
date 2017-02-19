cmake_minimum_required(VERSION 3.5)
project(insieme LANGUAGES C CXX)

# -- Module Path
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../cmake)

# -- Prefix Path
set(THIRD_PARTY_DIR ${PROJECT_SOURCE_DIR}/../third_party CACHE STRING "Third Party Library Directory")
file(GLOB prefix_paths ${THIRD_PARTY_DIR}/*)
list(APPEND CMAKE_PREFIX_PATH ${prefix_paths})

# -- Project Settings
include(build_settings)
include(doxygen)
include(git_metadata)

# -- Dependency Versions
set(BOOST_VERSION 1.59.0 CACHE STRING "Boost Version")
set(CUDD_VERSION 2.4.2 CACHE STRING "CUDD Version")
set(GMP_VERSION 6.0.0 CACHE STRING "GMP Version")
set(LLVM_VERSION 3.6.2 CACHE STRING "LLVM Version")
set(ZLIB_VERSION 1.2.8 CACHE STRING "ZLib Version")

# -- Dependency Settings
include(boost_settings)
include(energy_settings)
include(opencl_settings)
include(papi_settings)
include(valgrind_settings)

# -- CMake Modules
include(file_globs)
include(msvc_source_group)
include(add_module)
include(nproc)
include(run_ruby)
include(add_souffle)

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
