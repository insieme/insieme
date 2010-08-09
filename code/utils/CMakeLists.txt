#
# Builds the insieme util library
#
project(insieme_utils CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files 
file(GLOB insieme_utils_srcs 	src/*.cpp )
file(GLOB insieme_utils_incs	include/*.h )

# add include directory
include_directories(include)

# build the utils library
add_library(insieme_utils ${insieme_utils_srcs} ${insieme_utils_incs} )

# enable testing for this project
find_package(CxxTest)
if(CXXTEST_FOUND)
	include_directories(${CXXTEST_INCLUDE_DIR})

	# search for all test cases and register them
	file(GLOB test_cases test/*.h)
	foreach( case_file ${test_cases}) 
		get_filename_component( case_name ${case_file} NAME_WE )
	
		cxxtest_add_test(ut_${case_name} ${case_name}.cc ${case_file})
		target_link_libraries(ut_${case_name} insieme_utils)	
	endforeach(case_file)
endif()
