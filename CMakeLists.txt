project(insieme NONE)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# enable ctest support for testing
enable_testing()

add_subdirectory(docs)
add_subdirectory(code)
