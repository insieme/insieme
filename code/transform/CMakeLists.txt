#
# Builds the insieme transform library
#
project(insieme_transform CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_transform_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_transform_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_transform_include_dir} ) 
include_directories( ${insieme_analysis_include_dir} ) 

# Build the transform library
add_library(insieme_transform ${LINKING_TYPE} ${insieme_transform_srcs} ${insieme_transform_incs} )

# add dependencies
# add_dependencies( insieme_transform insieme_core insieme_utils insieme_analysis)

set ( ut_prefix  ut_transform_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )	
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_transform LIBRARY DESTINATION lib)
endif(DO_INSTALL)
