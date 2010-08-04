project(insieme_utils)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_utils_SOURCES 	src/*.cpp )

file(GLOB insieme_util_INCS "include/*.h")

INCLUDE_DIRECTORIES(include)

# Create an executable file called helloworld from sources:
add_library(insieme_utils ${insieme_utils_SOURCES} ${insieme_util_INCS})
