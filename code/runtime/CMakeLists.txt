#
# Builds the insieme runtime
#
project(insieme_runtime C CXX)

include(../environment.cmake)

file(GLOB_RECURSE insieme_runtime_srcs 	src/*.c )
file(GLOB_RECURSE insieme_runtime_incs 	include/*.h )

message(STATUS "HELLO" $ENV{OPENCL_ROOT})
# We also search for OpenCL in the NVIDIA SDK default location
find_library(OPENCL_LIBS NAMES OpenCL PATHS $ENV{OPENCL_ROOT}/lib/x86_64)

# list all include paths
include_directories(include/ $ENV{OPENCL_ROOT}/include/)

# Build the runtime
add_executable(insieme_runtime ${insieme_runtime_srcs} ${insieme_runtime_incs} )
target_link_libraries(insieme_runtime ${OPENCL_LIBS} ${pthread_LIB} dl rt )

# enable tests for this project
find_package(GTest REQUIRED)

include_directories(include/ ${GTEST_INCLUDE_DIRS} ${OPENCL_INCLUDE_DIR})

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

set ( ut_prefix  ut_rt_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} dl)
	target_link_libraries(insieme_runtime rt)
	
	# disable valgrind for parallel test cases
	set(VFLAG ${TRUE})
	if(case_name MATCHES ".*_par")
		set(VFLAG FALSE)
	endif()
	add_unit_test(${case_name} ${VFLAG})
endforeach(case_file)

file(GLOB test_cases test/*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_library(${case_name} SHARED ${case_file})
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} dl)
	target_link_libraries(insieme_runtime rt)
endforeach(case_file)
