#
# Builds the insieme core library
#
project(insieme_core CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_core_SOURCES 	src/*.cpp )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )

# Build the core library
add_library(insieme_core ${insieme_core_SOURCES})


# Build dummy executable
add_executable( main src/main.cpp )

# register linked libraries
target_link_libraries( main insieme_core insieme_utils )

