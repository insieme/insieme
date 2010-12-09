#
# Builds the insieme frontend library
#
project(insieme_frontend CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files
file(GLOB_RECURSE insieme_frontend_srcs 	src/*.cpp)
file(GLOB_RECURSE insieme_frontend_incs	include/*.h)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/clang_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/frontend/clang_config.h)

# add include directory
include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${LLVM_HOME}/include
	${insieme_utils_include_dir}
	${insieme_core_include_dir}
	${insieme_c_info_include_dir} )

# build the frontend library
add_library(insieme_frontend ${LINKING_TYPE} ${insieme_frontend_srcs} ${insieme_frontend_incs} )

# add dependencies
add_dependencies( insieme_frontend insieme_core insieme_utils insieme_c_info )
target_link_libraries(insieme_frontend ${clang_LIBs} ${pthread_LIB} ${llvm_LIBs})

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.cc)
foreach ( case_file ${test_cases})
 	get_filename_component( case_name ${case_file} NAME_WE )

	add_executable(ut_${case_name} ${case_file})
	include_directories(ut_${case_name} ${insieme_utils_include_dir})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} insieme_frontend)
	target_link_libraries(ut_${case_name} insieme_core)
	target_link_libraries(ut_${case_name} insieme_utils)
	target_link_libraries(ut_${case_name} insieme_c_info)
	target_link_libraries(ut_${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(ut_${case_name} ${clang_LIBs})
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	target_link_libraries(ut_${case_name} ${llvm_LIBs})
	
	if(${case_name} STREQUAL "kernel_matcher_test")
		add_unit_test(${case_name} off)
	else(${case_name} STREQUAL "ocl_compiler_test")
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()
endforeach(case_file)

