#
# Builds the insieme core library
#
project(insieme_core CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_core_srcs 	src/*.cpp )
file(GLOB insieme_core_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )

# automatically run the script for generating ASTBuilder interface
execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMMAND ruby builderbuilder.rb )
         
# Build the core library
add_library(insieme_core SHARED ${insieme_core_srcs} ${insieme_core_incs} )

# add dependencies
add_dependencies( insieme_core insieme_utils )

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_executable(ut_${case_name} ${case_file})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} insieme_core)
	target_link_libraries(ut_${case_name} insieme_utils)
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	target_link_libraries(ut_${case_name} ${xerces_LIB})
	
	if(case_name EQUAL "xml_dump_test")
		set(CONDUCT_MEMORY_CHECKS off)
	endif()
	
	add_unit_test(${case_name})
endforeach(case_file)
