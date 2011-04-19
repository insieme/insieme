#
# Builds the insieme runtime
#
project(insieme_runtime C CXX)

include(../environment.cmake)

file(GLOB_RECURSE insieme_runtime_srcs 	src/*.c )
file(GLOB_RECURSE insieme_runtime_incs 	include/*.h )

# list all include paths
include_directories(include/)

# Build the runtime
add_executable(insieme_runtime ${insieme_runtime_srcs} ${insieme_runtime_incs} )
target_link_libraries(insieme_runtime ${pthread_LIB})
target_link_libraries(insieme_runtime dl)


# enable tests for this project
find_package(GTest REQUIRED)

include_directories(include/ ${GTEST_INCLUDE_DIRS})

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")
file(GLOB test_cases test/*.cc)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_executable(ut_${case_name} ${case_file})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	target_link_libraries(ut_${case_name} dl)
	
	add_unit_test(${case_name})
endforeach(case_file)

file(GLOB test_cases test/*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_library(${case_name} ${case_file})
	target_link_libraries(${case_name} ${pthread_LIB})
	target_link_libraries(${case_name} dl)
endforeach(case_file)
