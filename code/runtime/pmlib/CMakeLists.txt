#
# Builds the power measurement library
#
project(insieme_power_measurement CXX)

#include(../../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -rdynamic -fPIC")

# add flags for debug mode
set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -fPIC")
  
# add flags for release mode
set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -fPIC")

set(LINKING_TYPE SHARED)

# collect source files
file(GLOB_RECURSE insieme_power_measurement_srcs *.cpp)
file(GLOB_RECURSE insieme_power_measurement_incs *.h)

#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/inputs.h.in

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_library(insieme_power_measurement ${LINKING_TYPE} ${insieme_power_measurement_srcs} ${insieme_power_measurement_incs})

