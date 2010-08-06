#
# Builds the insieme util library
#
project(insieme_utils CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files 
file(GLOB insieme_utils_SOURCES 	src/*.cpp )

# add include directory
include_directories(include)

# build the utils library
add_library(insieme_utils ${insieme_utils_SOURCES})
