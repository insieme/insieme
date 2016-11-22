cmake_minimum_required(VERSION 3.6)
project(%PROJECT% VERSION 0.0.0 LANGUAGES C CXX)

# -- Module Path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# -- Prefix Path
file(GLOB prefix_paths ${CMAKE_CURRENT_SOURCE_DIR}/libs/*)

# -- Extends
#add_subdirectory(insieme)

# -- Project Settings
include(build_settings)
include(boost_settings)
include(doxygen)

# -- Dependencies
#set(GMP_VERSION 6.0.0 CACHE STRING "GMP Version")

# -- CMake Modules
include(file_globs)
include(add_unittest)
#include(msvc_file_completion)

# -- Project Modules
