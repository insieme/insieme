project(insieme_common CXX)

cmake_minimum_required(VERSION 2.8)

#todo get rid of this... "only" used to get the path to the common_include_dir right
#include(../environment.cmake)

#file(GLOB_RECURSE insieme_common_incs include/*.h )

set(insieme_common_include_dir ${CMAKE_CURRENT_LIST_DIR}/include)

# provide a target (header-only) to depend on by others
add_library(insieme_common INTERFACE)
target_include_directories(insieme_common INTERFACE ${insieme_common_include_dir})
