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
include(valgrind_settings)
include(doxygen)
include(git_metadata)
include(insieme_specific)

# -- Dependency Versions
set(Boost_VERSION 1.59.0 CACHE STRING "Boost Version")
set(CUDD_VERSION 2.4.2 CACHE STRING "CUDD Version")

# -- Dependency Settings
include(boost_settings)

# -- CMake Modules
include(file_globs)
include(add_module)
include(nproc)

# -- Project Modules
add_subdirectory(code/common)
add_subdirectory(code/utils)
add_subdirectory(code/core)
