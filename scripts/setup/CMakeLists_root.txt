# This CMakeLists.txt should only be run by ExternalProject

cmake_minimum_required(VERSION 3.5)

option(INVOKED_AS_EXTERNAL_PROJECT OFF)
if(NOT INVOKED_AS_EXTERNAL_PROJECT)
	message(FATAL_ERROR "Root CMakeLists.txt only used for External Projects, call CMake on `code` folder instead.")
endif()

enable_testing()
add_subdirectory(code)
