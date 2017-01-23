cmake_minimum_required(VERSION 3.5)
project(%PROJECT% VERSION 0.0.0 LANGUAGES C CXX)

# -- Module Path
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# -- Prefix Path
set(THIRD_PARTY_DIR ${PROJECT_SOURCE_DIR}/third_party CACHE STRING "Third Party Library Directory")
file(GLOB prefix_paths ${THIRD_PARTY_DIR}/*)
list(APPEND CMAKE_PREFIX_PATH ${prefix_paths})

# -- Project Settings
include(build_settings)
include(doxygen)

# -- Dependency Versions
set(BOOST_VERSION 1.59.0 CACHE STRING "Boost Version")

# -- Dependency Settings
include(boost_settings)
include(valgrind_settings)

# -- CMake Modules
include(add_module)
include(file_globs)
include(msvc_source_group)
include(nproc)

# -- Project Modules
