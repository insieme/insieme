cmake_minimum_required(VERSION 3.5)
project(insieme LANGUAGES C CXX)

# -- Module Path
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# -- Prefix Path
set(THIRD_PARTY_DIR ${PROJECT_SOURCE_DIR}/third_party CACHE STRING "Third Party Library Directory")
file(GLOB prefix_paths ${THIRD_PARTY_DIR}/*)
list(APPEND CMAKE_PREFIX_PATH ${prefix_paths})

# -- Project Settings
include(build_settings)
include(energy_settings)
include(opencl_settings)
include(papi_settings)
include(valgrind_settings)
include(doxygen)
include(git_metadata)
include(insieme_specific)

# -- Dependency Versions
set(Boost_VERSION 1.59.0 CACHE STRING "Boost Version" FORCE)
set(CUDD_VERSION 2.4.2 CACHE STRING "CUDD Version" FORCE)
set(LLVM_VERSION 3.6.2 CACHE STRING "LLVM Version" FORCE)

# -- Dependency Settings
include(boost_settings)

# -- CMake Modules
include(file_globs)
include(add_module)
include(nproc)
include(run_ruby)

# -- Project Modules
add_subdirectory(code/common)

if(BUILD_COMPILER)
	add_subdirectory(code/analysis)
	add_subdirectory(code/annotations)
	add_subdirectory(code/backend)
	add_subdirectory(code/core)
	add_subdirectory(code/driver)
	add_subdirectory(code/frontend)
	add_subdirectory(code/transform)
	add_subdirectory(code/utils)
endif()

if(BUILD_RUNTIME)
	add_subdirectory(code/runtime)
endif()
