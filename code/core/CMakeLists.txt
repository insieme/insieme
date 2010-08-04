project(insieme_compiler_core)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_compiler_core_SOURCES 	src/*.cpp )

file(GLOB insieme_compiler_core_INCS "include/*.h")

# FIXME util include
INCLUDE_DIRECTORIES(include ../utils/include)

# Create an executable file called helloworld from sources:
add_executable(insieme_compiler_core ${insieme_compiler_core_SOURCES} ${insieme_compiler_core_INCS})
