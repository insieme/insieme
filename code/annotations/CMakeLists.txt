#
# Builds the insieme annotations library
#
project(insieme_annotations CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_annotations_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_annotations_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_core_include_dir} )

# Build the core library
add_library(insieme_annotations ${LINKING_TYPE} ${insieme_annotations_srcs} ${insieme_annotations_incs} )

# add dependencies
# add_dependencies( insieme_annotations insieme_core insieme_utils )

set ( ut_prefix  ut_annotations_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_annotations LIBRARY DESTINATION lib)
endif(DO_INSTALL)
