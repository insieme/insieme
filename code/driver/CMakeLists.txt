#
# Builds the insieme default driver
#
project(insieme_driver CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/driver_config.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/driver/driver_config.h)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_driver_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_driver_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )
include_directories( ${insieme_opencl_backend_include_dir} )
include_directories( ${insieme_c_info_include_dir} )
include_directories( ${insieme_driver_include_dir} )
include_directories( ${insieme_xml_include_dir} )


# Build dummy executable
add_executable( main ${insieme_driver_srcs} )

# add dependencies
add_dependencies( main insieme_core insieme_utils insieme_frontend insieme_simple_backend insieme_c_info )

# register linked libraries
target_link_libraries( main insieme_core insieme_utils insieme_frontend insieme_simple_backend insieme_opencl_backend insieme_c_info insieme_xml )

# add boost_program_options library
target_link_libraries( main ${Boost_PROGRAM_OPTIONS_LIBRARY})
target_link_libraries( main ${glog_LIB})

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )

	add_executable(ut_${case_name} ${case_file})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} ${glog_LIB})
	target_link_libraries(ut_${case_name} insieme_core)
	target_link_libraries(ut_${case_name} insieme_frontend)
	target_link_libraries(ut_${case_name} insieme_simple_backend)
	target_link_libraries(ut_${case_name} insieme_opencl_backend)
	target_link_libraries(ut_${case_name} insieme_c_info)
	target_link_libraries(ut_${case_name} insieme_utils)
	target_link_libraries(ut_${case_name} insieme_xml)	# FIXME
	target_link_libraries(ut_${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(ut_${case_name} ${pthread_LIB})

	add_unit_test(${case_name} off)
endforeach(case_file)
