#
# Builds the insieme simple backend
#
project(insieme_backend CXX)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_backend_srcs 	src/*.cpp src/*.def src/*.inc)
file(GLOB_RECURSE insieme_backend_incs 	include/*.h include/*.def include/*.inc)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/kernel_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/backend/ocl_kernel/kernel_config.h)

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_frontend_include_dir} ) #REMOVE: added only for ocl_standalone_test 
include_directories( ${insieme_transform_include_dir} )
include_directories( ${insieme_analysis_include_dir} )
include_directories( ${insieme_backend_include_dir} )

# Build the core library
add_library(insieme_backend ${LINKING_TYPE} ${insieme_backend_srcs} ${insieme_backend_incs} )

# add dependencies
#add_dependencies( insieme_backend insieme_core insieme_annotations insieme_utils )
#target_link_libraries(insieme_backend insieme_utils)
#target_link_libraries(insieme_backend insieme_core)
#target_link_libraries(insieme_backend insieme_annotations)
#target_link_libraries(insieme_backend insieme_transform)
#target_link_libraries(insieme_backend insieme_analysis)
#target_link_libraries(insieme_backend insieme_frontend) #REMOVE: added only for ocl_standalone_test

set ( ut_prefix  ut_be_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_frontend)
	target_link_libraries(${case_name} insieme_annotations)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} insieme_transform)
	target_link_libraries(${case_name} insieme_analysis)
	target_link_libraries(${case_name} insieme_backend)
	
	if(${case_name} STREQUAL ut_be_opencl_test OR
		${case_name} STREQUAL ut_be_kernel_poly_test)
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	#INSTALL(FILES include/insieme/backend/simple_backend.h DESTINATION include/insieme)
	INSTALL(TARGETS insieme_backend LIBRARY DESTINATION lib)
endif(DO_INSTALL)


