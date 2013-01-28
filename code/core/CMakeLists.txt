#
# Builds the insieme core library
#
project(insieme_core CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_core_srcs 	src/*.cpp src/*.def src/*.inc )
file(GLOB_RECURSE insieme_core_incs 	include/*.h include/*.def include/*.inc )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )

# automatically run the script for generating IRBuilder interface
execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMMAND ${RUBY_EXECUTABLE} builderbuilder.rb  RESULT_VARIABLE builderbuilderSuccess)

if (NOT ${builderbuilderSuccess} EQUAL 0)
	message(FATAL_ERROR "Ruby builder builder script failed to execute!")
endif()
         
# Build the core library
add_library(insieme_core ${LINKING_TYPE} ${insieme_core_srcs} ${insieme_core_incs} ${insieme_core_defs})

# add dependencies
# add_dependencies(insieme_core insieme_utils)

target_link_libraries( insieme_core ${cudd_LIB}) 
include_directories( test/include/ )

set ( ut_prefix  ut_core_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} ${cudd_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_core_include_dir}/insieme DESTINATION include)
	# install (FILES include/insieme/core/transform/manipulation.h DESTINATION include/core/)

	install(TARGETS insieme_core LIBRARY DESTINATION lib)
endif(DO_INSTALL)
