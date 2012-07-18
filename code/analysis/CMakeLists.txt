#
# Builds the insieme analysis library
#
project(insieme_analysis CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_config.h.in
	${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/analysis/cmake_config.h)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_analysis_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_analysis_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_analysis_include_dir} ) 
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_transform_include_dir} )

# Build the core library
add_library(insieme_analysis ${LINKING_TYPE} ${insieme_analysis_srcs} ${insieme_analysis_incs} )

target_link_libraries( insieme_analysis  
					   ${isl_LIB} 
					   ${cloog_LIB} 
					   ${barvinok_LIB} 
					   ${gmp_LIB}
					 ) 

set ( ut_prefix  ut_analysis_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_transform)

	target_link_libraries(${case_name} ${isl_LIB})
	target_link_libraries(${case_name} ${gmp_LIB})
	target_link_libraries(${case_name} ${cloog_LIB})
	target_link_libraries(${case_name} ${barvinok_LIB})
	target_link_libraries(${case_name} ${pthread_LIB})

	if(${case_name} STREQUAL ut_analysis_dependence_test)
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()

endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_analysis_include_dir}/insieme DESTINATION include)
	INSTALL(TARGETS insieme_analysis LIBRARY DESTINATION lib)
endif(DO_INSTALL)
