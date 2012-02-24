#
# Builds the insieme simple backend
#
project(insieme_opencl_backend CXX)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_opencl_backend_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_opencl_backend_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )
include_directories( ${insieme_opencl_backend_include_dir} )

# Build the opencl library
add_library(insieme_opencl_backend ${LINKING_TYPE} ${insieme_opencl_backend_srcs} ${insieme_opencl_backend_incs} )
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/opencl_backend_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/opencl_backend/opencl_backend_config.h)
# add dependencies
add_dependencies( insieme_opencl_backend insieme_simple_backend insieme_core insieme_annotations insieme_utils )
target_link_libraries(insieme_opencl_backend insieme_annotations)

set ( ut_prefix  ut_sbe_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )	
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_simple_backend)
	target_link_libraries(${case_name} insieme_opencl_backend)
	target_link_libraries(${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name} off)
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_opencl_backend LIBRARY DESTINATION lib)
endif(DO_INSTALL)
