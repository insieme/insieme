#
# Builds the insieme machine_learning library
#
project(insieme_machine_learning CXX)

include(../environment.cmake)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# collect source files
file(GLOB_RECURSE insieme_machine_learning_srcs 	src/*.cpp)
file(GLOB_RECURSE insieme_machine_learning_incs	include/*.h)

# add include directory
include_directories(
	${CMAKE_CURRENT_SOURCE_DIR}/include
	${SHARK_HOME}/include
	${KOMPEX_HOME}/inc )

# build the receive library
add_library(insieme_machine_learning ${LINKING_TYPE} ${insieme_machine_learning_srcs} ${insieme_machine_learning_incs} )

# add dependencies
add_dependencies( insieme_machine_learning insieme_core )
target_link_libraries(insieme_machine_learning ${shark_LIB} ${kompex_LIB})

# enable tests for this project
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

set ( ut_prefix  ut_ml_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	include_directories(${case_name} ${insieme_utils_include_dir})
	target_link_libraries(${case_name} ${GTEST_BOTH_LIBRARIES})
	target_link_libraries(${case_name} machine_learning)
	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} ${shark_LIB})
	target_link_libraries(${case_name} ${kompex_LIB})
	
	if(${case_name} STREQUAL ut_fe_ocl_host_compiler_test)
		add_unit_test(${case_name} off)
	else()
		add_unit_test(${case_name})
	endif()
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS insieme_machine_learning LIBRARY DESTINATION lib)
endif(DO_INSTALL)

