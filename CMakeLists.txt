project(insieme NONE)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# enable ctest support for testing
enable_testing()

find_package(Git)
if(GIT_FOUND)
	# deduce the code version using git describe
	get_filename_component( cur_dir ${CMAKE_CURRENT_LIST_FILE} PATH )
	set ( insieme_version "`(cd ${cur_dir}; ${GIT_EXECUTABLE} describe --dirty)`")
else()
	set ( insieme_version "unknown" )
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test/test.py.in ${CMAKE_BINARY_DIR}/test.py)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test/test.cfg.in ${CMAKE_BINARY_DIR}/test.cfg)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test/newtest.py.in ${CMAKE_BINARY_DIR}/newtest.py)

add_subdirectory(docs)
add_subdirectory(code)
