cmake_minimum_required(VERSION 2.6)

#todo get rid of this... "only" used to get the path to the common_include_dir right
include(../environment.cmake)

#file(GLOB_RECURSE insieme_common_incs include/*.h )

# provide a target (header-only) to depend on by others
add_library(insieme_common INTERFACE)
target_include_directories(insieme_common INTERFACE ${insieme_common_include_dir})
