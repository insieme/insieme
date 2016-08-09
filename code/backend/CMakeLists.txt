#
# Builds the insieme backend
#
project(insieme_backend CXX)
message(STATUS "========== configure backend =============")
cmake_minimum_required(VERSION 3.2)

# Create a variable called insieme_backend_srcs containing all .cpp files:
file(GLOB_RECURSE insieme_backend_srcs 	src/*.cpp src/*.def src/*.inc)
insieme_glob_headers(insieme_backend_incs include)

# Build the core library
add_library(insieme_backend ${LINKING_TYPE} ${insieme_backend_srcs} ${insieme_backend_incs} )
target_include_directories(insieme_backend PUBLIC ${insieme_backend_include_dir})
target_include_directories(insieme_backend PUBLIC ${insieme_common_include_dir})

# lookup Boost library
insieme_find_package(NAME Boost)
target_include_directories(insieme_backend SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})

target_link_libraries(insieme_backend insieme_core)
target_link_libraries(insieme_backend insieme_common)
target_link_libraries(insieme_backend insieme_utils)
target_link_libraries(insieme_backend insieme_annotations)
target_link_libraries(insieme_backend insieme_transform)

# =============================================  TESTING  =====================================
#target for backend unit_tests
set(ut_prefix  ut_be )

#custom target for the all BE unit tests
add_custom_target(${ut_prefix})

file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	insieme_fix_case_name( case_subdir ${case_file} )

	set ( case_name ${ut_prefix}${case_subdir}_${case_name} )
	add_executable(${case_name} ${case_file})

	#dependency on test suit target
	add_dependencies(${ut_prefix} ${case_name})

	target_link_libraries(${case_name} insieme_backend)
	
	if(${case_name} STREQUAL ut_be_opencl_test OR
		${case_name} STREQUAL ut_be_kernel_poly_test)
		add_unit_test(${case_name} ${ut_prefix} off)
	else()
		add_unit_test(${case_name} ${ut_prefix})
	endif()
endforeach(case_file)
