#
# Builds the insieme runtime
#
project(insieme_runtime C)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_runtime_srcs 	src/*.c )
file(GLOB_RECURSE insieme_runtime_incs 	include/*.h )

# list all include paths

# Build the runtime library
add_library(insieme_runtime ${LINKING_TYPE} ${insieme_runtime_srcs} ${insieme_runtime_incs} )


# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.c)
foreach (case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_executable(ut_${case_name} ${case_file})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} insieme_runtime)
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)
