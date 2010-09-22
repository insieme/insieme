#
# Builds the insieme simple backend
#
project(insieme_simple_backend CXX)

include(../environment.cmake)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB insieme_simple_backend_srcs 	src/*.cpp )
file(GLOB insieme_simple_backend_incs 	include/*.h )

# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_c_info_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )

# Build the core library
add_library(insieme_simple_backend SHARED ${insieme_simple_backend_srcs} ${insieme_simple_backend_incs} )

# add dependencies
add_dependencies( insieme_simple_backend insieme_core insieme_c_info insieme_utils )
target_link_libraries(insieme_simple_backend insieme_c_info)

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

file(GLOB test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	add_executable(ut_${case_name} ${case_file})
	target_link_libraries(ut_${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(ut_${case_name} insieme_core)
	target_link_libraries(ut_${case_name} insieme_c_info)
	target_link_libraries(ut_${case_name} insieme_utils)
	target_link_libraries(ut_${case_name} insieme_simple_backend)
	target_link_libraries(ut_${case_name} ${pthread_LIB})
	
	add_unit_test(${case_name})
endforeach(case_file)
